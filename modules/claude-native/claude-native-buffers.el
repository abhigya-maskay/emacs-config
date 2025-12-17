;;; claude-native-buffers.el --- Buffer management for Claude Native -*- lexical-binding: t -*-

;; Author: ave70011
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:
;; Provides buffer creation and management for the Claude Native UI:
;; - History buffer creation and content insertion
;; - Input buffer creation and prompt handling
;; - Block separator insertion for conversation flow

;;; Code:

(require 'claude-native-ui)
(require 'claude-native-modes)
(require 'claude-native-scroll)

;;; Buffer Creation

(defun claude-native--get-history-buffer ()
  "Get or create the Claude history buffer."
  (let ((buf (get-buffer-create claude-native-history-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'claude-native-history-mode)
        (claude-native-history-mode)))
    buf))

(defun claude-native--get-input-buffer ()
  "Get or create the Claude input buffer."
  (let ((buf (get-buffer-create claude-native-input-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'claude-native-input-mode)
        (claude-native-input-mode)
        (claude-native--insert-prompt)))
    buf))

(defun claude-native--insert-prompt ()
  "Insert the input prompt at the beginning of the input buffer."
  (when-let ((buf (get-buffer claude-native-input-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "> " 'face 'claude-native-prompt-face
                            'read-only t 'rear-nonsticky t))))))

(defun claude-native--get-input-text ()
  "Extract user input text from input buffer (after the prompt)."
  (with-current-buffer (get-buffer claude-native-input-buffer-name)
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (if (string-prefix-p "> " text)
          (string-trim (substring text 2))
        (string-trim text)))))

;;; History Insertion

(defun claude-native--insert-in-history (session text &optional face no-newline markdown-p)
  "Insert TEXT into SESSION's history buffer with optional FACE.
Unless NO-NEWLINE, ensures text ends with newline.  If MARKDOWN-P
is non-nil, content is fontified by gfm-mode.  Auto-scrolls the
history window and keeps spinner/timer overlays at the bottom."
  (when-let ((history-buf (plist-get session :history-buf)))
    (when (buffer-live-p history-buf)
      (with-current-buffer history-buf
        (let ((inhibit-read-only t)
              (start-pos (point-max))
              (spinner-overlay (plist-get session :spinner-overlay))
              (tool-overlay (plist-get session :tool-overlay)))
          (goto-char (point-max))
          ;; Insert with face only if NOT markdown content
          (if (and face (not markdown-p))
              (insert (propertize text 'face face))
            (insert text))
          (unless (or no-newline (string-suffix-p "\n" text))
            (insert "\n"))
          ;; Trigger font-lock for markdown content
          (when markdown-p
            (font-lock-ensure start-pos (point-max)))
          ;; Always force redisplay to show visual changes immediately
          (redisplay t)
          ;; Move spinner overlay to new end position if active
          (when (and spinner-overlay (overlay-buffer spinner-overlay))
            (let ((new-pos (point-max)))
              (move-overlay spinner-overlay new-pos new-pos)))
          ;; Move tool timer overlay to new end position only if timer is still running
          ;; (don't move after timer stopped, so completion mark stays in place)
          (when (and tool-overlay
                     (overlay-buffer tool-overlay)
                     (plist-get session :tool-timer))
            (let ((new-pos (point-max)))
              (move-overlay tool-overlay new-pos new-pos)))
          ;; Schedule deferred scroll to bottom
          ;; This coalesces multiple rapid insertions into a single scroll
          (claude-native--schedule-scroll session))))))

(defun claude-native--insert-block-separator (session)
  "Insert block separator if not the first block in the current turn.
Increments the block count for SESSION."
  (let ((count (or (plist-get session :block-count) 0)))
    (when (> count 0)
      (claude-native--insert-in-history
       session
       (concat "\n" claude-native-block-separator)
       'claude-native-separator-face))
    (plist-put session :block-count (1+ count))))

(provide 'claude-native-buffers)
;;; claude-native-buffers.el ends here
