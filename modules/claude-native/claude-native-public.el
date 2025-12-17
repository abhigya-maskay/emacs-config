;;; claude-native-public.el --- Public commands for Claude Native -*- lexical-binding: t -*-

;;; Commentary:
;; Provides the public interactive commands for Claude Native.

;;; Code:

(require 'claude-native-ui)
(require 'claude-native-session)
(require 'claude-native-window)
(require 'claude-native-input)
(require 'claude-native-buffers)
(require 'claude-native-collapsible)
(require 'claude-native-commands)

;; Forward declarations
(defvar claude-native--windows-visible)
(declare-function claude-native--send-denial-with-message "claude-native-approval-ui")

;;; Public Commands

;;;###autoload
(defun claude-native ()
  "Start Claude Code for the current project.
Sets up buffers and displays windows.  No process is started
until you send a message."
  (interactive)
  (claude-native--start-session)
  (claude-native--display-windows))

;;;###autoload
(defun claude-native-new-session ()
  "Start a fresh Claude conversation, discarding the previous session ID."
  (interactive)
  (let* ((directory (claude-native--get-working-directory))
         (session (claude-native--get-or-create-session directory)))
    ;; Clear session ID to start fresh
    (plist-put session :session-id nil)
    ;; Kill any running turn
    (when-let ((proc (plist-get session :current-proc)))
      (when (process-live-p proc)
        (kill-process proc)))
    (plist-put session :current-proc nil)
    (plist-put session :state 'idle)
    ;; Clear history
    (when-let ((history-buf (plist-get session :history-buf)))
      (when (buffer-live-p history-buf)
        (with-current-buffer history-buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (propertize "New conversation started\n"
                                'face 'claude-native-system-face))))))
    (message "Started new Claude conversation")))

;;;###autoload
(defun claude-native-toggle ()
  "Toggle Claude window visibility.
If no session exists, start one."
  (interactive)
  (let* ((directory (claude-native--get-working-directory))
         (session (claude-native--get-session directory)))
    (if (and session (plist-get session :history-buf))
        ;; Session exists, toggle windows
        (if claude-native--windows-visible
            (claude-native--hide-windows)
          (claude-native--display-windows))
      ;; No session, start one
      (claude-native))))

;;;###autoload
(defun claude-native-stop ()
  "Stop Claude and hide the panel.
Cancels any running turn and hides the Claude windows."
  (interactive)
  (let* ((directory (claude-native--get-working-directory))
         (session (claude-native--get-session directory))
         (proc (when session (plist-get session :current-proc))))
    ;; Cancel running turn if any
    (when (and proc (process-live-p proc))
      (kill-process proc)
      (plist-put session :current-proc nil)
      (plist-put session :state 'idle)
      (message "Claude turn cancelled"))
    ;; Always hide windows
    (claude-native--hide-windows)))

;;;###autoload
(defun claude-native-send-escape ()
  "Send Escape to cancel current Claude operation."
  (interactive)
  (let* ((directory (claude-native--get-working-directory))
         (session (claude-native--get-session directory))
         (proc (when session (plist-get session :current-proc))))
    (if (and proc (process-live-p proc))
        (process-send-string proc "\e")
      (user-error "No Claude turn in progress"))))

(defun claude-native-focus-input ()
  "Focus the Claude input buffer, displaying windows if needed."
  (interactive)
  (unless claude-native--windows-visible
    (claude-native--display-windows))
  (let ((buf (get-buffer claude-native-input-buffer-name)))
    (when buf
      (let ((win (get-buffer-window buf t)))
        (when win
          (select-window win)
          (goto-char (point-max)))))))

;;;###autoload
(defun claude-native-show-debug ()
  "Show the Claude debug buffer."
  (interactive)
  (display-buffer (get-buffer-create claude-native-debug-buffer-name)))

(defun claude-native-toggle-tool-at-point ()
  "Toggle collapse/expand of tool output at point.
If point is within a tool block, toggle between showing the full
output and a collapsed summary."
  (interactive)
  (if-let ((overlay (claude-native--get-tool-overlay-at-point)))
      (progn
        (claude-native--toggle-tool-overlay overlay)
        (message "Tool output %s"
                 (if (overlay-get overlay 'claude-expanded)
                     "expanded"
                   "collapsed")))
    (user-error "No tool output at point")))

(defun claude-native-send-input ()
  "Send the current input to Claude.
In normal mode, spawns a new turn.  During tool approval:
- In \\='choosing stage: error (use y/n/a keys)
- In \\='deny-message stage: send denial with entered message"
  (interactive)
  (let* ((directory (claude-native--get-working-directory))
         (session (claude-native--get-or-create-session directory))
         (current-proc (plist-get session :current-proc))
         (approval-stage (plist-get session :approval-stage)))
    (cond
     ;; Stage 2: Sending denial message
     ((eq approval-stage 'deny-message)
      (claude-native--send-denial-with-message session))
     ;; Stage 1: In approval mode, ignore RET
     ((eq approval-stage 'choosing)
      (user-error "Press y/n/a to respond to tool approval"))
     ;; Turn already in progress
     ((and current-proc (process-live-p current-proc))
      (user-error "A turn is already in progress - wait for completion"))
     ;; Normal input - spawn new turn
     (t
      (let ((text (claude-native--get-input-text)))
        (if (string-empty-p text)
            (user-error "Nothing to send")
          ;; Check for slash command first
          (if (claude-native--handle-slash-command text session)
              (claude-native--insert-prompt)  ; Reset input after command
            ;; Ensure buffers are set up
            (unless (plist-get session :history-buf)
              (claude-native--start-session))
            (claude-native--render-user-message session text)
            (claude-native--send-message session text)
            (claude-native--insert-prompt))))))))

;;;###autoload
(defun claude-native-send-prompt (&optional prompt)
  "Send PROMPT to Claude.
If PROMPT is nil, read from minibuffer.
Starts a new session if needed."
  (interactive)
  (let* ((directory (claude-native--get-working-directory))
         (session (claude-native--get-or-create-session directory))
         (current-proc (plist-get session :current-proc))
         (text (or prompt (read-string "Prompt: "))))
    (cond
     ((and current-proc (process-live-p current-proc))
      (user-error "A turn is already in progress"))
     ((string-empty-p text)
      (user-error "Nothing to send"))
     (t
      ;; Ensure buffers are set up
      (unless (plist-get session :history-buf)
        (claude-native--start-session))
      (claude-native--render-user-message session text)
      (claude-native--send-message session text)))))

;;;###autoload
(defun claude-native-send-region (start end)
  "Send region from START to END to Claude."
  (interactive "r")
  (claude-native-send-prompt (buffer-substring-no-properties start end)))

;;;###autoload
(defun claude-native-continue ()
  "Continue conversation with Claude (sessions persist automatically)."
  (interactive)
  (claude-native))

;;;###autoload
(defun claude-native-resume ()
  "Resume a specific Claude session (not yet implemented)."
  (interactive)
  (message "Resume not implemented. Sessions persist automatically."))

(provide 'claude-native-public)
;;; claude-native-public.el ends here
