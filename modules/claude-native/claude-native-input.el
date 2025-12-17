;;; claude-native-input.el --- Input handling for Claude Native -*- lexical-binding: t -*-

;;; Commentary:
;; Provides input sending and session initialization for Claude Native.

;;; Code:

(require 'claude-native-ui)
(require 'claude-native-session)
(require 'claude-native-messages)
(require 'claude-native-scroll)
(require 'claude-native-process)
(require 'claude-native-spinner)
(require 'claude-native-buffers)
(require 'claude-native-approval)

;;; Input Sending

(defun claude-native--render-user-message (session text)
  "Render user TEXT in SESSION's history buffer.
Resets the block count for the new turn."
  (plist-put session :block-count 0)
  (claude-native--insert-in-history session "\n" nil t)
  (claude-native--insert-in-history
   session
   (format "You: %s\n" text)
   'claude-native-user-face)
  ;; Force immediate scroll so user sees their message
  (claude-native--scroll-immediately session))

(defun claude-native--send-message (session text)
  "Start a new conversation turn with TEXT.
Spawns a CLI process for this turn using --resume if session exists."
  (let ((current-proc (plist-get session :current-proc)))
    (when (and current-proc (process-live-p current-proc))
      (user-error "A turn is already in progress"))
    (claude-native--spawn-turn session text)
    (claude-native--start-spinner session)))

(defun claude-native--start-session ()
  "Initialize a Claude session for the current project.
Sets up buffers and approval server (per-turn model).
Returns the session plist."
  (claude-native--ensure-cli)
  ;; Start the approval server if not already running
  (claude-native--start-approval-server)
  (let* ((directory (claude-native--get-working-directory))
         (session (claude-native--get-or-create-session directory))
         (history-buf (claude-native--get-history-buffer))
         (input-buf (claude-native--get-input-buffer)))
    ;; Set up buffers if not already done
    (unless (plist-get session :history-buf)
      (plist-put session :history-buf history-buf)
      (plist-put session :input-buf input-buf)
      ;; Insert startup message
      (with-current-buffer history-buf
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (propertize (format "Claude session for %s\n"
                                      (file-name-nondirectory
                                       (directory-file-name directory)))
                              'face 'claude-native-system-face))))
      ;; Add buffer kill hooks
      (with-current-buffer history-buf
        (add-hook 'kill-buffer-hook
                  (lambda () (claude-native--cleanup-session directory))
                  nil t))
      (with-current-buffer input-buf
        (add-hook 'kill-buffer-hook
                  (lambda () (claude-native--cleanup-session directory))
                  nil t)))
    session))

(provide 'claude-native-input)
;;; claude-native-input.el ends here
