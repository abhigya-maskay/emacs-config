;;; agent-dispatcher.el --- Unified AI agent dispatcher -*- lexical-binding: t -*-

;; Author: ave70011
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:
;; Provides unified commands that dispatch to the configured AI agent.
;; Supports GitHub Copilot CLI and Claude Native UI.

;;; Code:

(require 'init-utils)

(declare-function copilot-cli "copilot-cli")
(declare-function copilot-cli-toggle "copilot-cli")
(declare-function copilot-cli-stop "copilot-cli")
(declare-function copilot-cli-send-prompt "copilot-cli")
(declare-function copilot-cli-send-region "copilot-cli")
(declare-function copilot-cli-continue "copilot-cli")
(declare-function copilot-cli-resume "copilot-cli")
(declare-function copilot-cli-send-escape "copilot-cli")

(declare-function claude-native "claude-native")
(declare-function claude-native-toggle "claude-native")
(declare-function claude-native-stop "claude-native")
(declare-function claude-native-send-prompt "claude-native")
(declare-function claude-native-send-region "claude-native")
(declare-function claude-native-continue "claude-native")
(declare-function claude-native-resume "claude-native")
(declare-function claude-native-send-escape "claude-native")

(defgroup agent-dispatcher nil
  "Unified AI agent dispatcher."
  :group 'tools
  :prefix "agent-dispatcher-")

(defcustom agent-dispatcher-default-agent 'copilot
  "Default agent to use when none is set for a project."
  :type '(choice (const :tag "GitHub Copilot CLI" copilot)
                 (const :tag "Claude Native UI" claude-native))
  :group 'agent-dispatcher)

;;; Internal Variables

(defvar agent-dispatcher--project-agents (make-hash-table :test 'equal)
  "Hash table mapping project directories to agent preference.
Values are symbols: `copilot' or `claude-native'.")

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

;;;###autoload
(defun agent-dispatcher-select-agent ()
  "Interactively select the agent for the current project."
  (interactive)
  (let ((agent (intern (completing-read "Select agent: "
                                        '("copilot" "claude-native")
                                        nil t))))
    (agent-dispatcher--set-agent agent)
    (message "Agent set to %s for %s"
             agent
             (file-name-nondirectory
              (directory-file-name (agent-dispatcher--get-project-dir))))))

;;;###autoload
(defun agent-dispatcher-launch-copilot ()
  "Directly launch Copilot CLI and set it as current agent."
  (interactive)
  (agent-dispatcher--set-agent 'copilot)
  (require 'copilot-cli)
  (call-interactively #'copilot-cli))

;;;###autoload
(defun agent-dispatcher-launch-claude ()
  "Directly launch Claude Native UI and set it as current agent."
  (interactive)
  (agent-dispatcher--set-agent 'claude-native)
  (require 'claude-native)
  (call-interactively #'claude-native))

;;; Dispatch Macro

(defmacro agent-dispatcher--define (name doc copilot-fn claude-native-fn)
  "Define dispatch function NAME with DOC.
Routes to COPILOT-FN or CLAUDE-NATIVE-FN based on configured agent."
  (declare (indent 1))
  `(defun ,name ()
     ,doc
     (interactive)
     (pcase (agent-dispatcher--get-agent)
       ('copilot
        (require 'copilot-cli)
        (call-interactively #',copilot-fn))
       ('claude-native
        (require 'claude-native)
        (call-interactively #',claude-native-fn))
       (_ (user-error "Unknown agent: %s" (agent-dispatcher--get-agent))))))

;;; Dispatch Commands

;;;###autoload (autoload 'agent-start "agent-dispatcher" nil t)
(agent-dispatcher--define agent-start
  "Start the configured agent for the current project."
  copilot-cli claude-native)

;;;###autoload (autoload 'agent-stop "agent-dispatcher" nil t)
(agent-dispatcher--define agent-stop
  "Stop the configured agent for the current project."
  copilot-cli-stop claude-native-stop)

;;;###autoload (autoload 'agent-toggle "agent-dispatcher" nil t)
(agent-dispatcher--define agent-toggle
  "Toggle the configured agent window for the current project."
  copilot-cli-toggle claude-native-toggle)

;;;###autoload (autoload 'agent-send-prompt "agent-dispatcher" nil t)
(agent-dispatcher--define agent-send-prompt
  "Send a prompt to the configured agent."
  copilot-cli-send-prompt claude-native-send-prompt)

;;;###autoload (autoload 'agent-send-region "agent-dispatcher" nil t)
(agent-dispatcher--define agent-send-region
  "Send the current region to the configured agent."
  copilot-cli-send-region claude-native-send-region)

;;;###autoload (autoload 'agent-continue "agent-dispatcher" nil t)
(agent-dispatcher--define agent-continue
  "Continue the previous conversation with the configured agent."
  copilot-cli-continue claude-native-continue)

;;;###autoload (autoload 'agent-resume "agent-dispatcher" nil t)
(agent-dispatcher--define agent-resume
  "Resume a specific session with the configured agent."
  copilot-cli-resume claude-native-resume)

;;;###autoload (autoload 'agent-send-escape "agent-dispatcher" nil t)
(agent-dispatcher--define agent-send-escape
  "Send Escape key to the configured agent."
  copilot-cli-send-escape claude-native-send-escape)

(provide 'agent-dispatcher)
;;; agent-dispatcher.el ends here
