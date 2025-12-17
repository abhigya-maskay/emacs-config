;;; claude-native-spinner.el --- Spinner and timer animations for Claude Native -*- lexical-binding: t -*-

;; Author: ave70011
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:
;; Provides spinner and tool timer animations for the Claude Native UI.
;; These visual indicators show when Claude is thinking or executing tools.

;;; Code:

(require 'claude-native-ui)

;;; Spinner Configuration

(defvar claude-native--spinner-frames '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Animation frames for the thinking spinner (braille dots).")

(defvar claude-native--spinner-interval 0.1
  "Interval in seconds between spinner frame updates.")

;;; Spinner Functions

(defun claude-native--start-spinner (session)
  "Start the thinking spinner in SESSION's history buffer.
Creates an overlay at the end of the buffer and starts animation timer.
Shows elapsed time alongside the spinner animation."
  (claude-native--stop-spinner session)
  (plist-put session :spinner-start-time (current-time))
  (when-let ((history-buf (plist-get session :history-buf)))
    (when (buffer-live-p history-buf)
      (with-current-buffer history-buf
        (let* ((inhibit-read-only t)
               (start-pos (progn (goto-char (point-max))
                                 (insert "\n")
                                 (point)))
               (overlay (make-overlay start-pos start-pos history-buf)))
          (overlay-put overlay 'after-string
                       (propertize (concat "  " (car claude-native--spinner-frames) " 0.0s")
                                   'face 'claude-native-spinner-face))
          (overlay-put overlay 'claude-spinner t)
          (plist-put session :spinner-overlay overlay)
          (plist-put session :spinner-frame-index 0)
          (plist-put session :spinner-timer
                     (run-at-time nil claude-native--spinner-interval
                                  #'claude-native--animate-spinner session))
          (when-let ((win (get-buffer-window history-buf t)))
            (with-selected-window win
              (goto-char (point-max))
              (recenter -1))))))))

(defun claude-native--animate-spinner (session)
  "Advance spinner animation by one frame for SESSION.
Updates both the animation frame and the elapsed time display."
  (when-let ((overlay (plist-get session :spinner-overlay)))
    (when (overlay-buffer overlay)
      (let* ((start-time (plist-get session :spinner-start-time))
             (elapsed (if start-time
                          (float-time (time-subtract (current-time) start-time))
                        0.0))
             (idx (plist-get session :spinner-frame-index))
             (next-idx (mod (1+ idx) (length claude-native--spinner-frames)))
             (frame (nth next-idx claude-native--spinner-frames)))
        (plist-put session :spinner-frame-index next-idx)
        (overlay-put overlay 'after-string
                     (propertize (format "  %s %.1fs" frame elapsed)
                                 'face 'claude-native-spinner-face))))))

(defun claude-native--stop-spinner (session)
  "Stop and remove the thinking spinner for SESSION.
Cancels the timer and deletes the overlay."
  (when-let ((timer (plist-get session :spinner-timer)))
    (cancel-timer timer)
    (plist-put session :spinner-timer nil))
  (when-let ((overlay (plist-get session :spinner-overlay)))
    (when-let ((buf (overlay-buffer overlay)))
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (start (overlay-start overlay)))
          (when (and start (> start 1))
            (delete-region (1- start) start))
          (delete-overlay overlay))))
    (plist-put session :spinner-overlay nil))
  (plist-put session :spinner-frame-index 0)
  (plist-put session :spinner-start-time nil))

;;; Tool Timer Functions

(defun claude-native--start-tool-timer (session)
  "Start the tool execution timer in SESSION's history buffer.
Creates an overlay showing spinner + elapsed time."
  (claude-native--stop-tool-timer session)
  (plist-put session :tool-start-time (current-time))
  (when-let ((history-buf (plist-get session :history-buf)))
    (when (buffer-live-p history-buf)
      (with-current-buffer history-buf
        (let* ((inhibit-read-only t)
               (start-pos (progn (goto-char (point-max))
                                 (insert "\n")
                                 (point)))
               (overlay (make-overlay start-pos start-pos history-buf)))
          (overlay-put overlay 'after-string
                       (propertize (concat "  " (car claude-native--spinner-frames) " 0.0s")
                                   'face 'claude-native-tool-timer-face))
          (overlay-put overlay 'claude-tool-timer t)
          (plist-put session :tool-overlay overlay)
          (plist-put session :spinner-frame-index 0)
          (plist-put session :tool-timer
                     (run-at-time nil claude-native--spinner-interval
                                  #'claude-native--update-tool-timer session))
          (when-let ((win (get-buffer-window history-buf t)))
            (with-selected-window win
              (goto-char (point-max))
              (recenter -1))))))))

(defun claude-native--update-tool-timer (session)
  "Update the tool timer display with current elapsed time."
  (when-let ((overlay (plist-get session :tool-overlay)))
    (when (overlay-buffer overlay)
      (let* ((start-time (plist-get session :tool-start-time))
             (elapsed (float-time (time-subtract (current-time) start-time)))
             (idx (plist-get session :spinner-frame-index))
             (next-idx (mod (1+ idx) (length claude-native--spinner-frames)))
             (frame (nth next-idx claude-native--spinner-frames)))
        (plist-put session :spinner-frame-index next-idx)
        (overlay-put overlay 'after-string
                     (propertize (format "  %s %.1fs" frame elapsed)
                                 'face 'claude-native-tool-timer-face))))))

(defun claude-native--stop-tool-timer (session)
  "Stop the tool timer and show final elapsed time with checkmark.
Unlike the spinner, the overlay is kept to show completion status."
  (when-let ((timer (plist-get session :tool-timer)))
    (cancel-timer timer)
    (plist-put session :tool-timer nil))
  (when-let ((overlay (plist-get session :tool-overlay)))
    (when (overlay-buffer overlay)
      (let* ((start-time (plist-get session :tool-start-time))
             (elapsed (if start-time
                          (float-time (time-subtract (current-time) start-time))
                        0.0)))
        (overlay-put overlay 'after-string
                     (propertize (format "  ✓ %.1fs" elapsed)
                                 'face 'claude-native-tool-timer-face)))))
  (plist-put session :tool-start-time nil)
  (plist-put session :tool-name nil))

(defun claude-native--clear-tool-timer-overlay (session)
  "Remove the tool timer overlay completely.
Called when tool result is received to clean up before output."
  (when-let ((overlay (plist-get session :tool-overlay)))
    (when-let ((buf (overlay-buffer overlay)))
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (start (overlay-start overlay)))
          (when (and start (> start 1))
            (delete-region (1- start) start))
          (delete-overlay overlay))))
    (plist-put session :tool-overlay nil)))

(provide 'claude-native-spinner)
;;; claude-native-spinner.el ends here
