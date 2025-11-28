;;; init-agents.el --- AI Agent configuration -*- lexical-binding: t -*-

;;; Code:

;; claude-code-ide.el - has built-in anti-flicker and diff support
(use-package claude-code-ide
  :straight (:host github :repo "manzaltu/claude-code-ide.el")
  :custom
  ;; Use eat for stable terminal emulation
  (claude-code-ide-terminal-backend 'eat)
  ;; Window configuration
  (claude-code-ide-use-side-window t)
  (claude-code-ide-window-side 'right)
  (claude-code-ide-window-width 100)
  (claude-code-ide-focus-on-open t)
  ;; Use IDE diff viewer (built-in ediff integration)
  (claude-code-ide-use-ide-diff t)
  :config
  (claude-code-ide-emacs-tools-setup))

;; copilot-cli (local module)
(require 'copilot-cli)

;; agent-dispatcher (unified switching layer)
(require 'agent-dispatcher)

;; Evil leader keybindings - using dispatcher commands
(with-eval-after-load 'general
  (leader-keys
    ;; Quick toggle under SPC o (open)
    "oa" '(agent-toggle :wk "agent pane")

    ;; Full agent commands under SPC a
    "a" '(:ignore t :wk "agent")
    "aa" '(agent-start :wk "start agent")
    "at" '(agent-toggle :wk "toggle")
    "ac" '(agent-continue :wk "continue")
    "ar" '(agent-resume :wk "resume")
    "as" '(agent-send-prompt :wk "send prompt")
    "aR" '(agent-send-region :wk "send region")
    "ak" '(agent-stop :wk "stop agent")
    "ae" '(agent-send-escape :wk "send escape")
    "aS" '(agent-switch :wk "switch agent")

    ;; Direct access to specific agents
    "aC" '(claude-code-ide :wk "claude direct")
    "aP" '(copilot-cli :wk "copilot direct")))

(provide 'init-agents)
;;; init-agents.el ends here
