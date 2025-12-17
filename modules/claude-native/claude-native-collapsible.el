;;; claude-native-collapsible.el --- Collapsible tool output for Claude Native -*- lexical-binding: t -*-

;; Author: ave70011
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:
;; Provides collapsible/expandable tool output regions for the Claude Native UI.
;; Tool execution output can be collapsed to a summary line to reduce visual
;; clutter, with TAB to toggle expansion.

;;; Code:

(require 'cl-lib)
(require 'claude-native-ui)

;;; Tool Input Preview Formatting

(defun claude-native--format-tool-input-short (tool-name input)
  "Format INPUT preview for TOOL-NAME, short version for inline display.
Returns nil if no meaningful preview can be generated."
  (let ((max-len 50))
    (cond
     ((equal tool-name "Bash")
      (when-let ((cmd (gethash "command" input)))
        (let ((preview (if (> (length cmd) max-len)
                           (concat (substring cmd 0 max-len) "...")
                         cmd)))
          (format " %s" preview))))
     ((member tool-name '("Read" "Write" "Edit"))
      (when-let ((path (gethash "file_path" input)))
        (let* ((filename (file-name-nondirectory path))
               (preview (if (> (length filename) max-len)
                            (concat (substring filename 0 max-len) "...")
                          filename)))
          (format " %s" preview))))
     ((member tool-name '("Glob" "Grep"))
      (when-let ((pattern (gethash "pattern" input)))
        (let ((preview (if (> (length pattern) max-len)
                           (concat (substring pattern 0 max-len) "...")
                         pattern)))
          (format " %s" preview))))
     (t nil))))

;;; Collapsible Tool Output Functions

(defun claude-native--generate-collapsed-summary (_tool-name _input duration line-count)
  "Generate collapsed summary text for a tool block.
TOOL-NAME is the name of the tool (unused, reserved for future).
INPUT is the tool input (unused, reserved for future).
DURATION is the execution time in seconds.
LINE-COUNT is the number of lines in the tool output."
  (let* ((duration-str (if duration (format "✓ %.1fs" duration) ""))
         (line-info (if (and line-count (> line-count 0))
                        (format "%d lines" line-count)
                      ""))
         (details (string-join (delq nil (list duration-str line-info)) " · ")))
    (concat "▸ " (if (string-empty-p details) "(click TAB to expand)" details))))

(defun claude-native--start-tool-region (session tool-name input)
  "Mark the start of a collapsible tool region in SESSION.
TOOL-NAME and INPUT describe the tool being executed.
Records the current buffer position for later overlay creation."
  (when-let ((history-buf (plist-get session :history-buf)))
    (when (buffer-live-p history-buf)
      (with-current-buffer history-buf
        (plist-put session :current-tool-region
                   (list :start (point-max)
                         :tool-name tool-name
                         :input input
                         :start-time (current-time)))))))

(defun claude-native--finalize-tool-region (session)
  "Finalize the current tool region and create a collapsible overlay.
SESSION is the session plist. Creates an overlay spanning from the
recorded start position to current point, with collapse/expand capability."
  (when-let* ((history-buf (plist-get session :history-buf))
              (region (plist-get session :current-tool-region)))
    (when (buffer-live-p history-buf)
      (with-current-buffer history-buf
        (let* ((start (plist-get region :start))
               (end (point-max))
               (tool-name (plist-get region :tool-name))
               (input (plist-get region :input))
               (start-time (plist-get region :start-time))
               (duration (when start-time
                           (float-time (time-subtract (current-time) start-time))))
               ;; Count lines in the tool output region
               (line-count (count-lines start end))
               ;; Create the overlay
               (ov (make-overlay start end history-buf)))
          ;; Store metadata on the overlay
          (overlay-put ov 'claude-tool-block t)
          (overlay-put ov 'claude-tool-name tool-name)
          (overlay-put ov 'claude-tool-input input)
          (overlay-put ov 'claude-tool-duration duration)
          (overlay-put ov 'claude-tool-line-count line-count)
          (overlay-put ov 'claude-expanded t)  ; Start expanded
          ;; Store the original text for restoration
          (overlay-put ov 'claude-original-text
                       (buffer-substring-no-properties start end))
          ;; Generate collapsed summary
          (overlay-put ov 'claude-collapsed-summary
                       (claude-native--generate-collapsed-summary
                        tool-name input duration line-count))
          ;; Add to session's overlay list
          (plist-put session :tool-overlays
                     (cons ov (plist-get session :tool-overlays)))
          ;; Clear current region
          (plist-put session :current-tool-region nil)
          ov)))))

(defun claude-native--collapse-tool-overlay (overlay)
  "Collapse OVERLAY to show only the summary line."
  (when (and overlay (overlay-buffer overlay))
    (let* ((summary (overlay-get overlay 'claude-collapsed-summary))
           (tool-name (overlay-get overlay 'claude-tool-name))
           (input (overlay-get overlay 'claude-tool-input))
           (input-preview (when input
                            (claude-native--format-tool-input-short tool-name input)))
           (header (if input-preview
                       (format "%s[%s]%s" claude-native-tool-bullet tool-name input-preview)
                     (format "%s[%s]" claude-native-tool-bullet tool-name))))
      (overlay-put overlay 'claude-expanded nil)
      (overlay-put overlay 'display
                   (concat (propertize (concat "\n" header)
                                       'face 'claude-native-tool-face)
                           (propertize (concat "\n  " summary "\n")
                                       'face 'claude-native-collapsed-face))))))

(defun claude-native--expand-tool-overlay (overlay)
  "Expand OVERLAY to show the full tool output."
  (when (and overlay (overlay-buffer overlay))
    (overlay-put overlay 'claude-expanded t)
    ;; Remove display property to show original text
    (overlay-put overlay 'display nil)))

(defun claude-native--toggle-tool-overlay (overlay)
  "Toggle OVERLAY between collapsed and expanded state."
  (when (and overlay (overlay-buffer overlay))
    (if (overlay-get overlay 'claude-expanded)
        (claude-native--collapse-tool-overlay overlay)
      (claude-native--expand-tool-overlay overlay))))

(defun claude-native--get-tool-overlay-at-point ()
  "Return the tool block overlay at point, or nil if none."
  (cl-find-if (lambda (ov)
                (overlay-get ov 'claude-tool-block))
              (overlays-at (point))))

(defun claude-native--collapse-old-tools (session)
  "Collapse tool overlays that are older than the configured threshold.
SESSION is the session plist. Collapses all but the most recent
`claude-native-collapse-after-blocks' tool blocks."
  (when claude-native-auto-collapse-tools
    (let* ((overlays (plist-get session :tool-overlays))
           (num-to-keep claude-native-collapse-after-blocks)
           (to-collapse (nthcdr num-to-keep overlays)))
      ;; Collapse older overlays
      (dolist (ov to-collapse)
        (when (and (overlay-buffer ov)
                   (overlay-get ov 'claude-expanded))
          (claude-native--collapse-tool-overlay ov))))))

(provide 'claude-native-collapsible)
;;; claude-native-collapsible.el ends here
