;;; claude-native-approval.el --- MCP approval socket server for Claude Native -*- lexical-binding: t -*-

;; Author: ave70011
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:
;; Provides the MCP approval socket server for the Claude Native UI.
;; Handles tool approval requests from the emacs-approver MCP server,
;; queues multiple pending approvals, and sends responses back via TCP.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'claude-native-ui)
(require 'claude-native-log)
;; Functions defined in claude-native-buffers.el
(declare-function claude-native--insert-in-history "claude-native-buffers")

;; Functions defined in claude-native-approval-ui.el
(declare-function claude-native--show-approval-ui "claude-native-approval-ui")

;; Functions defined in claude-native-messages.el
(declare-function claude-native--hash-to-alist "claude-native-messages")

;; Functions defined in claude-native-session.el
(declare-function claude-native--tool-allowed-p "claude-native-session")
(declare-function claude-native--get-working-directory "claude-native-session")
(declare-function claude-native--get-or-create-session "claude-native-session")

;; Functions defined in claude-native-diff.el
(declare-function claude-native--show-diff-approval "claude-native-diff")
(declare-function claude-native--show-write-diff "claude-native-diff")
(declare-function claude-native--diff-cleanup "claude-native-diff")

;; From claude-inline-edit.el
(declare-function claude-inline-edit--handle-approval "claude-inline-edit")
(defvar claude-inline-edit--current-session)

;;; MCP Approval Socket Server Variables

(defvar claude-native--approval-server nil
  "TCP server process for receiving MCP approval requests.")

(defvar claude-native--approval-port 9876
  "Port for the approval socket server.")

(defvar claude-native--approval-id-counter 0
  "Counter for generating unique approval IDs.")

(defvar claude-native--mcp-config-path
  (expand-file-name "~/.config/emacs/mcp-config.json")
  "Path to the MCP configuration file.")

;;; MCP Approval Socket Server Functions

(defun claude-native--start-approval-server ()
  "Start TCP server to receive approval requests from MCP server.
The server listens on `claude-native--approval-port' and handles
tool approval requests from the emacs-approver MCP server."
  (unless (and claude-native--approval-server
               (process-live-p claude-native--approval-server))
    (condition-case err
        (progn
          (setq claude-native--approval-server
                (make-network-process
                 :name "claude-approval-server"
                 :server t
                 :host "127.0.0.1"
                 :service claude-native--approval-port
                 :family 'ipv4
                 :filter #'claude-native--approval-filter
                 :sentinel #'claude-native--approval-sentinel
                 :noquery t))
          (claude-native--log "Approval server started on port %d" claude-native--approval-port))
      (error
       (claude-native--log "Failed to start approval server: %s" (error-message-string err))
       (message "Failed to start approval server: %s" (error-message-string err))
       nil))))

(defun claude-native--stop-approval-server ()
  "Stop the approval socket server."
  (when (and claude-native--approval-server
             (process-live-p claude-native--approval-server))
    (delete-process claude-native--approval-server))
  (setq claude-native--approval-server nil))

(defun claude-native--approval-sentinel (_proc event)
  "Handle approval server connection events.
PROC is the client connection (unused).  EVENT describes what happened."
  (claude-native--log "Approval connection event: %s" (string-trim event)))

(defun claude-native--generate-approval-id ()
  "Generate a unique approval ID."
  (cl-incf claude-native--approval-id-counter)
  (intern (format "approval-%d" claude-native--approval-id-counter)))

(defun claude-native--get-current-approval (session)
  "Get the current approval plist from SESSION's queue."
  (let ((current-id (plist-get session :current-approval))
        (queue (plist-get session :pending-approvals)))
    (cl-find current-id queue :key (lambda (a) (plist-get a :id)))))

(defun claude-native--show-next-approval (session)
  "Show the next pending approval from SESSION's queue.
Uses FIFO order (oldest request first)."
  (let ((queue (plist-get session :pending-approvals)))
    (when queue
      ;; Get oldest (last in list since we push to front)
      (let* ((next (car (last queue)))
             (id (plist-get next :id))
             (tool-name (plist-get next :name))
             (input (plist-get next :input)))
        (plist-put session :current-approval id)
        (plist-put session :state 'awaiting-approval)
        ;; Log for debugging
        (claude-native--log "Showing approval %s for tool %s (queue size: %d)"
                            id tool-name (length queue))
        ;; Launch unified diff for Edit/Write tools if enabled
        (when claude-native-use-diff-for-edit
          (cond
           ((equal tool-name "Edit")
            (let ((file-path (gethash "file_path" input))
                  (old-string (gethash "old_string" input))
                  (new-string (gethash "new_string" input)))
              (claude-native--show-diff-approval session file-path old-string new-string)))
           ((equal tool-name "Write")
            (let ((file-path (gethash "file_path" input))
                  (content (gethash "content" input)))
              (claude-native--show-write-diff session file-path content)))))
        ;; Show approval UI
        (claude-native--show-approval-ui session tool-name input)))))

(defun claude-native--approval-filter (proc output)
  "Handle incoming approval request from MCP server.
PROC is the client connection, OUTPUT is the JSON request.
Queues the approval and shows it if no other approval is being displayed."
  (claude-native--log "Received approval request: %s" output)
  (condition-case err
      (let* ((request (json-parse-string (string-trim output) :object-type 'hash-table))
             (tool-name (gethash "tool" request))
             (input (gethash "input" request)))
        ;; Check if inline edit session is active AND waiting for an approval
        (if (and (boundp 'claude-inline-edit--current-session)
                 claude-inline-edit--current-session
                 (memq (plist-get claude-inline-edit--current-session :state)
                       '(waiting approving)))
            (progn
              (claude-native--log "Routing approval to inline edit handler")
              (claude-inline-edit--handle-approval tool-name input proc))
          ;; Normal agent pane flow
          (let* ((directory (claude-native--get-working-directory))
                 (session (claude-native--get-or-create-session directory))
                 (approval-id (claude-native--generate-approval-id)))
            ;; Check if tool is in always-allowed list
            (if (claude-native--tool-allowed-p session tool-name)
                ;; Auto-approve immediately without queueing
                (progn
                  (claude-native--log "Auto-approving tool %s" tool-name)
                  (claude-native--insert-in-history
                   session (format "\n[%s] (auto-approved)" tool-name) 'claude-native-tool-face)
                  (let ((response (json-encode `((behavior . "allow")
                                                 (updatedInput . ,(claude-native--hash-to-alist input))))))
                    (process-send-string proc (concat response "\n"))
                    (delete-process proc)))
              ;; Queue the approval request
              (let ((approval (list :id approval-id
                                    :name tool-name
                                    :input input
                                    :connection proc)))
                (claude-native--log "Queueing approval %s for tool %s" approval-id tool-name)
                ;; Add to front of queue (we'll process from end for FIFO)
                (plist-put session :pending-approvals
                           (cons approval (plist-get session :pending-approvals)))
                ;; Cache file content for Edit tools (before file is modified by Claude)
                ;; This ensures subsequent approvals for the same file can find their old_string
                (when (and (equal tool-name "Edit")
                           (gethash "file_path" input))
                  (let* ((file-path (gethash "file_path" input))
                         (cache (or (plist-get session :file-content-cache)
                                    (make-hash-table :test 'equal))))
                    (unless (gethash file-path cache)
                      (when (file-exists-p file-path)
                        (puthash file-path
                                 (with-temp-buffer
                                   (insert-file-contents file-path)
                                   (buffer-string))
                                 cache)))
                    (plist-put session :file-content-cache cache)))
                ;; Show in history
                (claude-native--insert-in-history
                 session (format "\n[%s] awaiting approval..." tool-name) 'claude-native-tool-face)
                ;; If no approval is currently displayed, show this one
                (unless (plist-get session :current-approval)
                  (claude-native--show-next-approval session)))))))
    (error
     (claude-native--log "Error processing approval request: %s" (error-message-string err))
     ;; Send deny response on error
     (let ((response (json-encode `((behavior . "deny")
                                    (message . ,(format "Error: %s" (error-message-string err)))))))
       (process-send-string proc (concat response "\n"))
       (delete-process proc)))))

(defun claude-native--send-approval-response (session allow &optional reason)
  "Send approval response back to MCP server via socket.
SESSION is the session plist.
ALLOW is non-nil to approve, nil to deny.
REASON is an optional denial message."
  (let* ((approval (claude-native--get-current-approval session))
         (conn (plist-get approval :connection))
         (input (plist-get approval :input))
         (tool-name (plist-get approval :name))
         (current-id (plist-get session :current-approval))
         (response (if allow
                       (json-encode `((behavior . "allow")
                                      (updatedInput . ,(claude-native--hash-to-alist input))))
                     (json-encode `((behavior . "deny")
                                    (message . ,(or reason "User denied permission")))))))
    (claude-native--log "Sending approval response for %s: %s" current-id response)
    ;; Clean up current diff before transitioning to next approval
    (claude-native--diff-cleanup session)
    ;; Store tool info for display when tool_use event arrives
    (when allow
      (plist-put session :last-tool-input
                 (list :name tool-name :input input)))
    (if (and conn (process-live-p conn))
        (progn
          (process-send-string conn (concat response "\n"))
          (delete-process conn)
          (claude-native--log "Response sent to MCP server for %s" current-id))
      (claude-native--log "Warning: No active approval connection for %s" current-id))
    ;; Remove this approval from the queue
    (plist-put session :pending-approvals
               (cl-remove current-id (plist-get session :pending-approvals)
                          :key (lambda (a) (plist-get a :id))))
    (plist-put session :current-approval nil)
    (plist-put session :approval-stage nil)
    ;; Clean up cache entry if no more pending approvals for this file
    (when-let ((cache (plist-get session :file-content-cache))
               (file-path (gethash "file_path" input)))
      (let ((remaining-files (mapcar (lambda (a)
                                       (gethash "file_path" (plist-get a :input)))
                                     (plist-get session :pending-approvals))))
        (unless (member file-path remaining-files)
          (remhash file-path cache))))
    ;; Check if there are more pending approvals
    (if (plist-get session :pending-approvals)
        ;; Show next approval
        (claude-native--show-next-approval session)
      ;; No more approvals, return to running state
      (plist-put session :state 'running))))

(provide 'claude-native-approval)
;;; claude-native-approval.el ends here
