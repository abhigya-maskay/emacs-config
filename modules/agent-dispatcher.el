;;; agent-dispatcher.el --- Unified AI agent dispatcher -*- lexical-binding: t -*-

;; Author: ave70011
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:
;; Provides unified commands that dispatch to either Claude Code or
;; GitHub Copilot CLI based on per-project preference.

;;; Code:

(require 'init-utils)

(declare-function claude-code-ide "claude-code-ide")
(declare-function claude-code-ide-toggle "claude-code-ide")
(declare-function claude-code-ide-stop "claude-code-ide")
(declare-function claude-code-ide-send-prompt "claude-code-ide")
(declare-function claude-code-ide-insert-at-mentioned "claude-code-ide")
(declare-function claude-code-ide-continue "claude-code-ide")
(declare-function claude-code-ide-resume "claude-code-ide")
(declare-function claude-code-ide-send-escape "claude-code-ide")

(declare-function copilot-cli "copilot-cli")
(declare-function copilot-cli-toggle "copilot-cli")
(declare-function copilot-cli-stop "copilot-cli")
(declare-function copilot-cli-send-prompt "copilot-cli")
(declare-function copilot-cli-send-region "copilot-cli")
(declare-function copilot-cli-continue "copilot-cli")
(declare-function copilot-cli-resume "copilot-cli")
(declare-function copilot-cli-send-escape "copilot-cli")

(defgroup agent-dispatcher nil
  "Unified AI agent dispatcher."
  :group 'tools
  :prefix "agent-dispatcher-")

(defcustom agent-dispatcher-default-agent 'claude
  "Default agent to use when none is set for a project."
  :type '(choice (const :tag "Claude Code" claude)
                 (const :tag "GitHub Copilot CLI" copilot))
  :group 'agent-dispatcher)

;;; Internal Variables

(defvar agent-dispatcher--project-agents (make-hash-table :test 'equal)
  "Hash table mapping project directories to agent preference.
Values are symbols: `claude' or `copilot'.")

;;; Agent Resolution

(defun agent-dispatcher--get-project-dir ()
  "Get current project directory."
  (init-utils-project-root))

(defun agent-dispatcher--read-dir-locals-agent ()
  "Read agent preference from dir-locals if set.
Looks for `agent-preferred-agent' variable."
  (when (boundp 'agent-preferred-agent)
    agent-preferred-agent))

(defun agent-dispatcher--get-agent ()
  "Get the preferred agent for the current project.
Checks: 1) hash table, 2) dir-locals, 3) default."
  (let ((dir (agent-dispatcher--get-project-dir)))
    (or (gethash dir agent-dispatcher--project-agents)
        (agent-dispatcher--read-dir-locals-agent)
        agent-dispatcher-default-agent)))

(defun agent-dispatcher--set-agent (agent)
  "Set AGENT as the preference for the current project."
  (let ((dir (agent-dispatcher--get-project-dir)))
    (puthash dir agent agent-dispatcher--project-agents)))

;;; Dispatch Macro

(defmacro agent-dispatcher--define (name doc claude-fn copilot-fn)
  "Define dispatch function NAME with DOC, dispatching to CLAUDE-FN or COPILOT-FN."
  (declare (indent 1))
  `(defun ,name ()
     ,doc
     (interactive)
     (pcase (agent-dispatcher--get-agent)
       ('claude
        (require 'claude-code-ide)
        (call-interactively #',claude-fn))
       ('copilot
        (require 'copilot-cli)
        (call-interactively #',copilot-fn))
       (_ (user-error "Unknown agent configured")))))

;;; Dispatch Commands

;;;###autoload (autoload 'agent-start "agent-dispatcher" nil t)
(agent-dispatcher--define agent-start
  "Start the configured agent for the current project."
  claude-code-ide copilot-cli)

;;;###autoload (autoload 'agent-stop "agent-dispatcher" nil t)
(agent-dispatcher--define agent-stop
  "Stop the configured agent for the current project."
  claude-code-ide-stop copilot-cli-stop)

;;;###autoload (autoload 'agent-toggle "agent-dispatcher" nil t)
(agent-dispatcher--define agent-toggle
  "Toggle the configured agent window for the current project."
  claude-code-ide-toggle copilot-cli-toggle)

;;;###autoload (autoload 'agent-send-prompt "agent-dispatcher" nil t)
(agent-dispatcher--define agent-send-prompt
  "Send a prompt to the configured agent."
  claude-code-ide-send-prompt copilot-cli-send-prompt)

;;;###autoload (autoload 'agent-send-region "agent-dispatcher" nil t)
(agent-dispatcher--define agent-send-region
  "Send the current region to the configured agent."
  claude-code-ide-insert-at-mentioned copilot-cli-send-region)

;;;###autoload (autoload 'agent-continue "agent-dispatcher" nil t)
(agent-dispatcher--define agent-continue
  "Continue the previous conversation with the configured agent."
  claude-code-ide-continue copilot-cli-continue)

;;;###autoload (autoload 'agent-resume "agent-dispatcher" nil t)
(agent-dispatcher--define agent-resume
  "Resume a specific session with the configured agent."
  claude-code-ide-resume copilot-cli-resume)

;;;###autoload (autoload 'agent-send-escape "agent-dispatcher" nil t)
(agent-dispatcher--define agent-send-escape
  "Send Escape key to the configured agent."
  claude-code-ide-send-escape copilot-cli-send-escape)

;;;###autoload
(defun agent-switch ()
  "Switch the agent for the current project.
Stops current agent if running and starts the new one."
  (interactive)
  (let* ((current (agent-dispatcher--get-agent))
         (choices '(("Claude Code" . claude)
                    ("GitHub Copilot CLI" . copilot)))
         (selection (completing-read
                     (format "Switch agent (current: %s): " current)
                     (mapcar #'car choices)
                     nil t))
         (new-agent (cdr (assoc selection choices))))
    (when (and new-agent (not (eq new-agent current)))
      (condition-case err
          (pcase current
            ('claude
             (require 'claude-code-ide)
             (claude-code-ide-stop))
            ('copilot
             (require 'copilot-cli)
             (copilot-cli-stop)))
        (error
         (message "Warning: Failed to stop %s: %s" current (error-message-string err))))
      (agent-dispatcher--set-agent new-agent)
      (pcase new-agent
        ('claude
         (require 'claude-code-ide)
         (claude-code-ide))
        ('copilot
         (require 'copilot-cli)
         (copilot-cli)))
      (message "Switched to %s" selection))))

(provide 'agent-dispatcher)
;;; agent-dispatcher.el ends here
