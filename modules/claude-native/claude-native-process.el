;;; claude-native-process.el --- Process management for Claude Native -*- lexical-binding: t -*-

;; Author: ave70011
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:
;; Handles CLI process lifecycle for the Claude Native UI.
;; Manages spawning conversation turns, filtering output,
;; handling termination, and cleaning up sessions.

;;; Code:

(require 'cl-lib)
(require 'claude-native-ui)
(require 'claude-native-window)
(require 'claude-native-session)
(require 'claude-native-spinner)
(require 'claude-native-messages)

;;; External Dependencies
;; Forward declarations for other modules
(declare-function claude-native--insert-in-history "claude-native-buffers")
(declare-function claude-native--log "claude-native")

;;; Process Filter

(defun claude-native--turn-filter (process output)
  "Handle streaming output from a turn process.
PROCESS is the CLI process, OUTPUT is the received chunk.
Buffers partial lines and dispatches complete JSON messages."
  (let ((session nil))
    ;; Find which session owns this process
    (maphash (lambda (_dir sess)
               (when (eq (plist-get sess :current-proc) process)
                 (setq session sess)))
             claude-native--sessions)
    (when session
      ;; Append to line buffer and extract complete lines
      (let* ((buffer (concat (or (plist-get session :line-buffer) "") output))
             (parsed (claude-native--extract-json-lines buffer)))
        ;; Store remainder for next chunk
        (plist-put session :line-buffer (cdr parsed))
        ;; Process each complete JSON line
        (dolist (line (car parsed))
          (claude-native--log "Processing JSON: %s" (substring line 0 (min 100 (length line))))
          (when-let ((msg (claude-native--parse-json-line line)))
            (claude-native--dispatch-message session msg)))))))

;;; Session Cleanup

(defun claude-native--cleanup-session (directory)
  "Clean up Claude session for DIRECTORY."
  (let ((session (gethash directory claude-native--sessions)))
    (when session
      ;; Stop spinner and tool timer before cleanup
      (claude-native--stop-spinner session)
      (claude-native--stop-tool-timer session)
      (claude-native--clear-tool-timer-overlay session)
      ;; Cancel any pending scroll timer
      (when-let ((scroll-timer (plist-get session :scroll-timer)))
        (cancel-timer scroll-timer))
      ;; Remove from tracking first to prevent recursion from kill-buffer-hook
      (remhash directory claude-native--sessions)
      ;; Kill current turn process if running
      (when-let ((proc (plist-get session :current-proc)))
        (when (process-live-p proc)
          (kill-process proc)))
      ;; Kill buffers (won't recurse since we already removed from hash)
      (dolist (buf (list (plist-get session :history-buf)
                         (plist-get session :input-buf)))
        (when (and buf (buffer-live-p buf))
          (kill-buffer buf)))
      ;; Update window visibility state
      (setq claude-native--windows-visible nil))))

(defun claude-native--cleanup-all-sessions ()
  "Clean up all Claude sessions. For use in `kill-emacs-hook'."
  (maphash (lambda (dir _session)
             (claude-native--cleanup-session dir))
           claude-native--sessions))

(add-hook 'kill-emacs-hook #'claude-native--cleanup-all-sessions)

;;; Process Spawning

(defun claude-native--spawn-turn (session prompt)
  "Spawn a CLI process for a single conversation turn.
SESSION is the session plist.
PROMPT is the user message to send.
Returns the process object."
  (let* ((directory (claude-native--get-working-directory))
         (default-directory directory)
         (session-id (plist-get session :session-id))
         (args (claude-native--build-turn-args prompt session-id))
         (process-name (format "claude-turn[%s]"
                               (file-name-nondirectory
                                (directory-file-name directory))))
         ;; Set CLAUDE_EMACS_UI so the PreToolUse hook routes to our approval server
         (process-environment (cons "CLAUDE_EMACS_UI=1" process-environment))
         (process (make-process
                   :name process-name
                   :buffer nil
                   :command (cons claude-native-cli-path args)
                   :noquery t
                   :connection-type 'pty
                   :coding 'utf-8-emacs-unix
                   :filter #'claude-native--turn-filter
                   :sentinel #'claude-native--turn-sentinel)))
    (claude-native--log "Spawning turn: %s %s" claude-native-cli-path (string-join args " "))
    ;; Update session state for this turn
    (plist-put session :current-proc process)
    (plist-put session :state 'running)
    (plist-put session :line-buffer "")
    process))

;;; Process Sentinel

(defun claude-native--turn-sentinel (process event)
  "Handle turn process termination.
PROCESS is the CLI process, EVENT describes the termination."
  (let ((session nil))
    ;; Find which session owns this process
    (maphash (lambda (_dir sess)
               (when (eq (plist-get sess :current-proc) process)
                 (setq session sess)))
             claude-native--sessions)
    (when session
      ;; Always stop spinner and tool timer on process termination
      (claude-native--stop-spinner session)
      (claude-native--stop-tool-timer session)
      (claude-native--clear-tool-timer-overlay session)
      (cond
       ;; Normal completion
       ((string-match-p "finished" event)
        ;; session_id already extracted in handle-result
        (plist-put session :current-proc nil)
        (unless (eq (plist-get session :state) 'idle)
          (plist-put session :state 'idle)))
       ;; User cancelled or killed
       ((string-match-p "\\(killed\\|terminated\\)" event)
        (plist-put session :current-proc nil)
        (plist-put session :state 'idle)
        (claude-native--insert-in-history session "\n[cancelled]" 'claude-native-error-face))
       ;; Abnormal exit
       ((string-match-p "exited abnormally" event)
        (plist-put session :current-proc nil)
        (plist-put session :state 'idle)
        (claude-native--insert-in-history
         session (format "\n[error: %s]" (string-trim event)) 'claude-native-error-face))))))

(provide 'claude-native-process)
;;; claude-native-process.el ends here
