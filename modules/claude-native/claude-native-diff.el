;;; claude-native-diff.el --- Diff display for Claude Native tool approvals -*- lexical-binding: t -*-

;; Author: ave70011
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:
;; Provides diff display functionality for Edit and Write tool approvals
;; in the Claude Native UI. Shows unified diffs with syntax highlighting,
;; ghost text for removed lines, and green highlighting for additions.

;;; Code:

(require 'cl-lib)

;;; External Dependencies
;; These are defined in claude-native.el
(declare-function claude-native--log "claude-native")

;;; Helper Functions

(defun claude-native--count-matches (regexp string)
  "Count occurrences of REGEXP in STRING."
  (let ((count 0)
        (start 0))
    (while (string-match regexp string start)
      (setq count (1+ count)
            start (match-end 0)))
    count))

(defun claude-native--normalize-quotes (str)
  "Normalize quote characters in STR for matching.
Converts curly quotes to straight quotes."
  (let ((result str))
    ;; Single quotes: curly → straight
    (setq result (string-replace "\u2018" "'" result))  ; LEFT SINGLE QUOTATION MARK
    (setq result (string-replace "\u2019" "'" result))  ; RIGHT SINGLE QUOTATION MARK
    (setq result (string-replace "\u201B" "'" result))  ; SINGLE HIGH-REVERSED-9
    ;; Double quotes: curly → straight
    (setq result (string-replace "\u201C" "\"" result)) ; LEFT DOUBLE QUOTATION MARK
    (setq result (string-replace "\u201D" "\"" result)) ; RIGHT DOUBLE QUOTATION MARK
    (setq result (string-replace "\u201F" "\"" result)) ; DOUBLE HIGH-REVERSED-9
    result))

(defun claude-native--apply-edit (original old-string new-string)
  "Apply Edit tool change to ORIGINAL content.
Replaces OLD-STRING with NEW-STRING.
Tries exact match first, then normalized quotes as fallback.
Returns (content . error-message) cons.
- On success: (proposed-content . nil)
- On failure: (nil . error-message)"
  (cond
   ;; Exact match
   ((string-match-p (regexp-quote old-string) original)
    (let ((matches (claude-native--count-matches (regexp-quote old-string) original)))
      (if (> matches 1)
          (cons nil (format "old_string found %d times (must be unique)" matches))
        (cons (replace-regexp-in-string
               (regexp-quote old-string) new-string original t t)
              nil))))
   ;; Try normalized quotes
   (t
    (let* ((norm-old (claude-native--normalize-quotes old-string))
           (norm-orig (claude-native--normalize-quotes original)))
      (if (not (string-match-p (regexp-quote norm-old) norm-orig))
          (cons nil "old_string not found in file")
        (let ((matches (claude-native--count-matches (regexp-quote norm-old) norm-orig)))
          (if (> matches 1)
              (cons nil (format "old_string found %d times (must be unique)" matches))
            ;; Find position in normalized, apply to original
            (when-let ((pos (string-match (regexp-quote norm-old) norm-orig)))
              (let ((before (substring original 0 pos))
                    (after (substring original (+ pos (length old-string)))))
                (cons (concat before new-string after) nil))))))))))

;;; Diff Display Functions

(defun claude-native--show-diff-approval (session file-path old-string new-string)
  "Show unified diff comparison for Edit tool approval in SESSION.
FILE-PATH is the file being edited.
OLD-STRING is the text to be replaced.
NEW-STRING is the replacement text.
Returns t if a preview was successfully launched, nil otherwise."
  (condition-case err
      (let* ((file-exists (file-exists-p file-path))
             (cache (plist-get session :file-content-cache))
             ;; Use cached content if available (for parallel edits to same file)
             ;; Fall back to reading from disk if not cached
             (original-content (or (when cache (gethash file-path cache))
                                   (when file-exists
                                     (with-temp-buffer
                                       (insert-file-contents file-path)
                                       (buffer-string)))))
             (result (when original-content
                       (claude-native--apply-edit original-content old-string new-string)))
             (proposed-content (car result))
             (error-msg (cdr result)))
        (cond
         ((not file-exists)
          (claude-native--show-proposed-buffer session file-path new-string "new file"))
         (error-msg
          (claude-native--show-proposed-buffer session file-path new-string error-msg))
         ((string-empty-p old-string)
          (claude-native--show-proposed-buffer session file-path proposed-content "full replacement"))
         (t
          (claude-native--launch-unified-diff session file-path proposed-content old-string new-string))))
    (error
     (claude-native--log "Diff error: %s" (error-message-string err))
     nil)))

(defun claude-native--show-write-diff (session file-path content)
  "Show diff view for Write tool approval in SESSION.
FILE-PATH is the file being written.
CONTENT is the content to be written.
For new files, shows all content as additions (green).
For existing files, shows a comparison between old and new content.
Returns t if a preview was successfully launched, nil otherwise."
  (condition-case err
      (let* ((file-exists (file-exists-p file-path))
             (original-content (when file-exists
                                 (with-temp-buffer
                                   (insert-file-contents file-path)
                                   (buffer-string)))))
        (if file-exists
            ;; File exists - show comparison between old and new
            (claude-native--launch-write-comparison session file-path original-content content)
          ;; New file - show all content as additions
          (claude-native--launch-new-file-diff session file-path content)))
    (error
     (claude-native--log "Write diff error: %s" (error-message-string err))
     nil)))

(defun claude-native--launch-new-file-diff (session file-path content)
  "Show new file CONTENT with all lines highlighted as additions.
SESSION is the session plist.
FILE-PATH is used for buffer naming and major mode.
CONTENT is the new file content to display.
Returns t on success."
  (plist-put session :diff-window-config (current-window-configuration))
  (let* ((filename (file-name-nondirectory file-path))
         (diff-buf-name (format "*claude-diff:%s*" filename))
         (diff-buf (get-buffer-create diff-buf-name)))
    (with-current-buffer diff-buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content)
        ;; Set major mode for syntax highlighting
        (let ((buffer-file-name file-path))
          (set-auto-mode))
        ;; Add green overlay to entire content (new file = all additions)
        (let ((ov (make-overlay (point-min) (point-max))))
          (overlay-put ov 'face 'diff-added)
          (overlay-put ov 'claude-diff t))
        (goto-char (point-min)))
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (font-lock-ensure))
    ;; Store buffer reference for cleanup
    (plist-put session :diff-buffer diff-buf)
    ;; Display in main window
    (let ((main-window (cl-find-if
                        (lambda (w)
                          (not (window-parameter w 'window-side)))
                        (window-list))))
      (when main-window
        (select-window main-window)
        (switch-to-buffer diff-buf)))
    (message "New file: %s" filename)
    t))

(defun claude-native--launch-write-comparison (session file-path original-content new-content)
  "Show comparison between ORIGINAL-CONTENT and NEW-CONTENT for Write.
SESSION is the session plist.  FILE-PATH is used for buffer naming.
Shows old content as ghost text (red) and new content (green).
Returns t on success."
  (plist-put session :diff-window-config (current-window-configuration))
  (let* ((filename (file-name-nondirectory file-path))
         (diff-buf-name (format "*claude-diff:%s*" filename))
         (diff-buf (get-buffer-create diff-buf-name)))
    (with-current-buffer diff-buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Insert original content as "removed" (ghost text)
        (insert original-content)
        (let ((ov (make-overlay (point-min) (point-max))))
          (overlay-put ov 'face 'diff-removed)
          (overlay-put ov 'claude-diff t))
        ;; Add separator
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert (propertize "─── replaced by ───\n" 'face 'shadow))
        ;; Insert new content as "added"
        (let ((new-start (point)))
          (insert new-content)
          (let ((ov (make-overlay new-start (point-max))))
            (overlay-put ov 'face 'diff-added)
            (overlay-put ov 'claude-diff t)))
        ;; Set major mode for syntax highlighting
        (let ((buffer-file-name file-path))
          (set-auto-mode))
        (goto-char (point-min)))
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (font-lock-ensure))
    ;; Store buffer reference for cleanup
    (plist-put session :diff-buffer diff-buf)
    ;; Display in main window
    (let ((main-window (cl-find-if
                        (lambda (w)
                          (not (window-parameter w 'window-side)))
                        (window-list))))
      (when main-window
        (select-window main-window)
        (switch-to-buffer diff-buf)))
    (message "File replacement: %s" filename)
    t))

(defun claude-native--show-proposed-buffer (session file-path content reason)
  "Show proposed CONTENT in a buffer for review.
SESSION is the session plist.
FILE-PATH is used for buffer naming and major mode.
CONTENT is the proposed content to display.
REASON is a short description shown in the message."
  ;; Save window configuration before displaying
  (plist-put session :diff-window-config (current-window-configuration))
  (let* ((filename (file-name-nondirectory file-path))
         (buf-name (format "*claude-proposed:%s*" filename))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content))
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (let ((buffer-file-name file-path))
        (set-auto-mode))
      (goto-char (point-min)))
    ;; Store buffer reference for cleanup
    (plist-put session :diff-proposed-buffer buf)
    ;; Display the buffer
    (display-buffer buf '(display-buffer-pop-up-window))
    (message "Showing proposed content (%s): %s" reason filename)
    t))

(defun claude-native--launch-unified-diff (session file-path proposed-content old-string new-string)
  "Launch inline diff view for SESSION.
Shows full file with removed lines (red) and added lines (green).
FILE-PATH is used for buffer naming and mode.  PROPOSED-CONTENT
is the full file after the edit.  OLD-STRING is text being replaced.
NEW-STRING is the replacement text.  Returns t on success."
  ;; Save window configuration before diff display
  (plist-put session :diff-window-config (current-window-configuration))
  (let* ((filename (file-name-nondirectory file-path))
         (diff-buf-name (format "*claude-diff:%s*" filename))
         (diff-buf (get-buffer-create diff-buf-name))
         (diff-pos nil))
    ;; Build display content with ghost text and overlays
    (with-current-buffer diff-buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Insert proposed content (full file after edit)
        (insert proposed-content)
        ;; Set major mode for syntax highlighting
        (let ((buffer-file-name file-path))
          (set-auto-mode))
        ;; Find where new_string is and add overlays
        (goto-char (point-min))
        (when (search-forward new-string nil t)
          (let* ((new-end (point))
                 (new-start (- new-end (length new-string))))
            ;; Save position for scrolling after display
            (setq diff-pos new-start)
            ;; Insert ghost text (old_string) above with red face and separator
            (goto-char new-start)
            (save-excursion
              (insert old-string)
              ;; Ensure newline after old content for visual separation
              (let ((added-newline (not (bolp))))
                (unless (bolp) (insert "\n"))
                ;; Add visual separator between removed and added content
                (let ((sep "─── replaced by ───\n"))
                  (insert (propertize sep 'face 'shadow))
                  ;; Calculate positions accounting for separator
                  (let* ((extra-len (+ (if added-newline 1 0) (length sep)))
                         (old-end (+ new-start (length old-string))))
                    ;; Overlay for removed content (red)
                    (let ((ov (make-overlay new-start old-end)))
                      (overlay-put ov 'face 'diff-removed)
                      (overlay-put ov 'claude-diff t))
                    ;; Overlay for added content (green) - after old-string + separator
                    (let ((ov (make-overlay (+ old-end extra-len)
                                            (+ old-end extra-len (length new-string)))))
                      (overlay-put ov 'face 'diff-added)
                      (overlay-put ov 'claude-diff t))))))))
        (goto-char (point-min)))
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      ;; Force immediate fontification
      (font-lock-ensure))
    ;; Store buffer reference for cleanup
    (plist-put session :diff-buffer diff-buf)
    ;; Display diff buffer - use main area, not side window
    (let ((main-window (cl-find-if
                        (lambda (w)
                          (not (window-parameter w 'window-side)))
                        (window-list))))
      (when main-window
        (select-window main-window)
        (switch-to-buffer diff-buf)
        ;; Scroll to show the diff location
        (when diff-pos
          (goto-char diff-pos)
          (recenter))))
    t))

(defun claude-native--diff-cleanup (session)
  "Clean up diff buffers and state for SESSION.
Restores the window configuration that was saved before diff launched."
  (let ((diff-buf (plist-get session :diff-buffer))
        (prop-buf (plist-get session :diff-proposed-buffer))
        (win-config (plist-get session :diff-window-config)))
    ;; Kill diff buffer (overlays are automatically removed)
    (when (and diff-buf (buffer-live-p diff-buf))
      (kill-buffer diff-buf))
    ;; Kill proposed buffer if it exists (from fallback cases)
    (when (and prop-buf (buffer-live-p prop-buf))
      (kill-buffer prop-buf))
    ;; Restore window configuration
    (when win-config
      (set-window-configuration win-config))
    ;; Clear session state
    (plist-put session :diff-buffer nil)
    (plist-put session :diff-proposed-buffer nil)
    (plist-put session :diff-window-config nil)))

(provide 'claude-native-diff)
;;; claude-native-diff.el ends here
