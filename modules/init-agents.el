;;; init-agents.el --- AI Agent configuration -*- lexical-binding: t -*-

(defun init-agents--setup-spinner-fonts ()
  "Set up consistent fonts for Claude Code spinner Unicode characters.
Prevents terminal 'dancing' from font height differences.
See: https://github.com/be5invis/Iosevka/issues/2741"
  (when (eq system-type 'darwin)
    (dolist (char '(#x26CF #x2692 #x2694 #x2697))
      (set-fontset-font t char "Apple Symbols" nil 'prepend))))

(init-agents--setup-spinner-fonts)

(defun init-agents--setup-terminal-font ()
  "Set up JuliaMono font for agent terminals if available.
Provides broader Unicode coverage and consistent line height."
  (when (and (display-graphic-p)
             (member "JuliaMono" (font-family-list)))
    (buffer-face-set '(:family "JuliaMono"))))

(with-eval-after-load 'eat
  (add-hook 'eat-mode-hook #'init-agents--setup-terminal-font))

(use-package posframe)

(require 'copilot-cli)
(require 'agent-dispatcher)
(require 'claude-inline-edit)

(with-eval-after-load 'general
  (leader-keys
    "oa" '(agent-toggle :wk "agent pane")

    "a" '(:ignore t :wk "agent")
    "aa" '(agent-start :wk "start agent")
    "at" '(agent-toggle :wk "toggle")
    "ac" '(agent-continue :wk "continue")
    "ar" '(agent-resume :wk "resume")
    "as" '(agent-send-prompt :wk "send prompt")
    "aR" '(agent-send-region :wk "send region")
    "ak" '(agent-stop :wk "stop agent")
    "ae" '(agent-send-escape :wk "send escape")
    "ay" '(eat-enter-copy-mode :wk "copy mode")
    "aS" '(agent-dispatcher-select-agent :wk "select agent")

    "aP" '(agent-dispatcher-launch-copilot :wk "copilot direct")
    "aC" '(agent-dispatcher-launch-claude :wk "claude direct")))

(provide 'init-agents)
;;; init-agents.el ends here
