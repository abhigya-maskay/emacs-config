;;; claude-native-commands.el --- Slash commands for Claude Native -*- lexical-binding: t -*-

(require 'claude-native-ui)
(require 'claude-native-session)
(require 'claude-native-window)

(defun claude-native--handle-slash-command (text session)
  "Check if TEXT is a slash command and execute it.
SESSION is the current session plist.
Returns t if a command was handled, nil otherwise."
  (when (string-prefix-p "/" text)
    (let ((cmd (downcase (car (split-string (string-trim text))))))
      (pcase cmd
        ("/clear" (claude-native--cmd-clear session) t)
        ("/exit" (claude-native--cmd-exit session) t)
        (_ nil)))))

(defun claude-native--cmd-clear (session)
  "Clear conversation and start fresh session.
SESSION is the current session plist."
  (when-let ((proc (plist-get session :current-proc)))
    (when (process-live-p proc)
      (kill-process proc)))
  (when-let ((timer (plist-get session :spinner-timer)))
    (cancel-timer timer))
  (when-let ((timer (plist-get session :tool-timer)))
    (cancel-timer timer))
  (dolist (approval (plist-get session :pending-approvals))
    (let ((conn (plist-get approval :connection)))
      (when (and conn (process-live-p conn))
        (delete-process conn))))
  (when-let ((buf (plist-get session :history-buf)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (remove-overlays (point-min) (point-max))
          (erase-buffer)))))
  (dolist (buf (buffer-list))
    (when (string-prefix-p "*claude-diff:" (buffer-name buf))
      (kill-buffer buf)))
  (plist-put session :session-id nil)
  (plist-put session :current-proc nil)
  (plist-put session :state 'idle)
  (plist-put session :pending-approvals nil)
  (plist-put session :current-approval nil)
  (plist-put session :approval-stage nil)
  (plist-put session :tool-overlays nil)
  (plist-put session :current-tool-region nil)
  (plist-put session :spinner-overlay nil)
  (plist-put session :spinner-timer nil)
  (plist-put session :spinner-frame-index 0)
  (plist-put session :spinner-start-time nil)
  (plist-put session :tool-overlay nil)
  (plist-put session :tool-timer nil)
  (plist-put session :tool-start-time nil)
  (plist-put session :tool-name nil)
  (message "Conversation cleared"))

(defun claude-native--cmd-exit (session)
  "Exit Claude, terminate session completely.
SESSION is the current session plist."
  (let ((directory (claude-native--get-working-directory))
        (history-buf (plist-get session :history-buf))
        (input-buf (plist-get session :input-buf)))
    (when-let ((proc (plist-get session :current-proc)))
      (when (process-live-p proc)
        (kill-process proc)))
    (when-let ((timer (plist-get session :spinner-timer)))
      (cancel-timer timer))
    (when-let ((timer (plist-get session :tool-timer)))
      (cancel-timer timer))
    (dolist (approval (plist-get session :pending-approvals))
      (let ((conn (plist-get approval :connection)))
        (when (and conn (process-live-p conn))
          (delete-process conn))))
    (when (buffer-live-p history-buf)
      (with-current-buffer history-buf
        (remove-overlays (point-min) (point-max))))
    (dolist (buf (buffer-list))
      (when (string-prefix-p "*claude-diff:" (buffer-name buf))
        (kill-buffer buf)))
    (claude-native--hide-windows)
    (when (buffer-live-p history-buf) (kill-buffer history-buf))
    (when (buffer-live-p input-buf) (kill-buffer input-buf))
    (claude-native--remove-session directory)
    (message "Claude session terminated")))

(provide 'claude-native-commands)
;;; claude-native-commands.el ends here
