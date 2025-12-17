;;; claude-native-commands.el --- Slash commands for Claude Native -*- lexical-binding: t -*-

;; Author: ave70011
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:
;; Provides slash command handling for the Claude Native UI:
;; - /clear: Clear conversation and start fresh
;; - /exit: Terminate session completely

;;; Code:

(require 'claude-native-ui)
(require 'claude-native-session)
(require 'claude-native-spinner)
(require 'claude-native-window)

;;; Slash Command Dispatcher

(defun claude-native--handle-slash-command (text session)
  "Check if TEXT is a slash command and execute it.
SESSION is the current session plist.
Returns t if a command was handled, nil otherwise."
  (when (string-prefix-p "/" text)
    (let ((cmd (downcase (car (split-string (string-trim text))))))
      (pcase cmd
        ("/clear" (claude-native--cmd-clear session) t)
        ("/exit" (claude-native--cmd-exit session) t)
        (_ (user-error "Unknown command: %s" cmd))))))

;;; Command Implementations

(defun claude-native--cmd-clear (session)
  "Clear conversation and start fresh session.
SESSION is the current session plist."
  ;; Kill running process
  (when-let ((proc (plist-get session :current-proc)))
    (when (process-live-p proc)
      (kill-process proc)))
  ;; Stop timers
  (claude-native--stop-spinner session)
  (claude-native--stop-tool-timer session)
  ;; Clean up any pending approval connections
  (dolist (approval (plist-get session :pending-approvals))
    (let ((conn (plist-get approval :connection)))
      (when (and conn (process-live-p conn))
        (delete-process conn))))
  ;; Clean up tool overlays
  (dolist (ov (plist-get session :tool-overlays))
    (when (overlay-buffer ov)
      (delete-overlay ov)))
  ;; Reset session state
  (plist-put session :session-id nil)
  (plist-put session :current-proc nil)
  (plist-put session :state 'idle)
  (plist-put session :pending-approvals nil)
  (plist-put session :current-approval nil)
  (plist-put session :approval-stage nil)
  (plist-put session :tool-overlays nil)
  (plist-put session :current-tool-region nil)
  ;; Clear history buffer
  (when-let ((buf (plist-get session :history-buf)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)))))
  (message "Conversation cleared"))

(defun claude-native--cmd-exit (session)
  "Exit Claude, terminate session completely.
SESSION is the current session plist."
  (let ((directory (claude-native--get-working-directory))
        (history-buf (plist-get session :history-buf))
        (input-buf (plist-get session :input-buf)))
    ;; Kill running process
    (when-let ((proc (plist-get session :current-proc)))
      (when (process-live-p proc)
        (kill-process proc)))
    ;; Stop timers
    (claude-native--stop-spinner session)
    (claude-native--stop-tool-timer session)
    ;; Clean up any pending approval connections
    (dolist (approval (plist-get session :pending-approvals))
      (let ((conn (plist-get approval :connection)))
        (when (and conn (process-live-p conn))
          (delete-process conn))))
    ;; Clean up tool overlays
    (dolist (ov (plist-get session :tool-overlays))
      (when (overlay-buffer ov)
        (delete-overlay ov)))
    ;; Hide windows FIRST (before killing buffers)
    (claude-native--hide-windows)
    ;; Now kill buffers safely
    (when (buffer-live-p history-buf) (kill-buffer history-buf))
    (when (buffer-live-p input-buf) (kill-buffer input-buf))
    ;; Remove session from hash table
    (claude-native--remove-session directory)
    (message "Claude session terminated")))

(provide 'claude-native-commands)
;;; claude-native-commands.el ends here
