;;; claude-native-approval-ui.el --- Tool approval UI for Claude Native -*- lexical-binding: t -*-

;; Author: ave70011
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:
;; Provides the user-facing tool approval interface for Claude Native.
;; Handles the approval prompt display, y/n/a key bindings, and denial
;; message flow.  Works with claude-native-approval.el which handles
;; the socket server and queue management.

;;; Code:

(require 'claude-native-ui)

;;; External Dependencies

;; From claude-native.el
(declare-function claude-native--log "claude-native")
;; From claude-native-buffers.el
(declare-function claude-native--insert-in-history "claude-native-buffers")
(declare-function claude-native--insert-prompt "claude-native-buffers")
(declare-function claude-native--get-input-text "claude-native-buffers")

;; From claude-native-session.el
(declare-function claude-native--get-working-directory "claude-native-session")
(declare-function claude-native--get-session "claude-native-session")
(declare-function claude-native--allow-tool-always "claude-native-session")

;; From claude-native-approval.el
(declare-function claude-native--send-approval-response "claude-native-approval")
(declare-function claude-native--get-current-approval "claude-native-approval")

;; From claude-native-diff.el
(declare-function claude-native--diff-cleanup "claude-native-diff")

;; From claude-native-collapsible.el
(declare-function claude-native--format-tool-input-preview "claude-native-collapsible")

;;; Tool Approval Mode

(defvar claude-native-approval-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "y") #'claude-native-approve-tool)
    (define-key map (kbd "n") #'claude-native-start-deny)
    (define-key map (kbd "a") #'claude-native-always-allow-tool)
    map)
  "Keymap for tool approval in input buffer.")

(define-minor-mode claude-native-approval-mode
  "Minor mode for tool approval prompts in Claude input buffer.
When active, y/n/a keys approve, deny, or always-allow the pending tool."
  :lighter " Approve"
  :keymap claude-native-approval-mode-map)

;;; Approval UI Functions

(defun claude-native--show-approval-ui (session tool-name input)
  "Show tool approval UI in SESSION's input buffer.
TOOL-NAME and INPUT describe the tool awaiting approval."
  (plist-put session :approval-stage 'choosing)
  (let ((input-buf (plist-get session :input-buf))
        (preview (claude-native--format-tool-input-preview tool-name input))
        (queue-count (length (plist-get session :pending-approvals))))
    (with-current-buffer input-buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "[%s] Approve?\n" tool-name)
                            'face 'claude-native-approval-face))
        ;; Show queue count if more than one pending
        (when (> queue-count 1)
          (insert (propertize (format "(+%d more pending)\n" (1- queue-count))
                              'face 'claude-native-prompt-face)))
        (insert (propertize (concat preview "\n\n")
                            'face 'claude-native-tool-input-face))
        (insert (concat (propertize "[y]" 'face 'claude-native-approval-key-face)
                        " approve  "
                        (propertize "[n]" 'face 'claude-native-approval-key-face)
                        " deny  "
                        (propertize "[a]" 'face 'claude-native-approval-key-face)
                        " always allow")))
      (claude-native-approval-mode 1)
      ;; Focus input buffer
      (when-let ((win (get-buffer-window input-buf t)))
        (select-window win)))))

(defun claude-native--finish-approval (session)
  "Clean up after tool approval and restore normal input.
Only performs cleanup if there are no more pending approvals."
  ;; Only clean up and restore input if no more pending approvals
  ;; (send-approval-response already handles diff cleanup and calls
  ;; show-next-approval which sets up the next diff if there are more)
  (unless (plist-get session :pending-approvals)
    ;; Clean up diff view if it was active (final cleanup)
    (claude-native--diff-cleanup session)
    ;; Clear file content cache
    (plist-put session :file-content-cache nil)
    (let ((input-buf (plist-get session :input-buf)))
      (with-current-buffer input-buf
        (claude-native-approval-mode -1)
        (claude-native--insert-prompt)))))

(defun claude-native--send-denial-with-message (session)
  "Send denial with message from input buffer for SESSION."
  (let ((reason (claude-native--get-input-text)))
    (claude-native--insert-in-history
     session
     (format " [denied%s]\n" (if (string-empty-p reason) "" (concat ": " reason)))
     'claude-native-error-face)
    (claude-native--send-approval-response session nil
                                           (unless (string-empty-p reason) reason))
    (claude-native--finish-approval session)))

;;; Tool Approval Commands

(defun claude-native-approve-tool ()
  "Approve the pending tool execution."
  (interactive)
  (let* ((dir (claude-native--get-working-directory))
         (session (claude-native--get-session dir)))
    (unless (and session (plist-get session :current-approval))
      (user-error "No tool awaiting approval"))
    (claude-native--insert-in-history session " [approved]\n" 'claude-native-system-face)
    (claude-native--send-approval-response session t)
    (claude-native--finish-approval session)))

(defun claude-native-start-deny ()
  "Start the denial message stage.
Shows a prompt for the user to enter an optional denial reason."
  (interactive)
  (let* ((dir (claude-native--get-working-directory))
         (session (claude-native--get-session dir))
         (input-buf (plist-get session :input-buf)))
    (unless (and session (plist-get session :current-approval))
      (user-error "No tool awaiting approval"))
    (plist-put session :approval-stage 'deny-message)
    (with-current-buffer input-buf
      (claude-native-approval-mode -1)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Denial reason (optional):\n" 'face 'claude-native-system-face))
        (insert (propertize "> " 'face 'claude-native-prompt-face
                            'read-only t 'rear-nonsticky t)))
      (goto-char (point-max)))))

(defun claude-native-always-allow-tool ()
  "Approve and always allow this tool for the session."
  (interactive)
  (let* ((dir (claude-native--get-working-directory))
         (session (claude-native--get-session dir)))
    (unless (and session (plist-get session :current-approval))
      (user-error "No tool awaiting approval"))
    (let* ((approval (claude-native--get-current-approval session))
           (name (plist-get approval :name)))
      (claude-native--allow-tool-always session name)
      (claude-native--insert-in-history
       session (format " [approved - always allow %s]\n" name) 'claude-native-system-face)
      (claude-native--send-approval-response session t)
      (claude-native--finish-approval session))))

(provide 'claude-native-approval-ui)
;;; claude-native-approval-ui.el ends here
