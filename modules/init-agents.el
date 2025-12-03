;;; init-agents.el --- AI Agent configuration -*- lexical-binding: t -*-

;;; Code:

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

(use-package claude-code-ide
  :straight (:host github :repo "manzaltu/claude-code-ide.el")
  :custom
  (claude-code-ide-terminal-backend 'eat)
  (claude-code-ide-use-side-window t)
  (claude-code-ide-window-side 'right)
  (claude-code-ide-window-width 100)
  (claude-code-ide-focus-on-open t)
  (claude-code-ide-use-ide-diff t)
  :config
  (claude-code-ide-emacs-tools-setup))

(require 'copilot-cli)
(require 'agent-dispatcher)

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
    "aS" '(agent-switch :wk "switch agent")
    "ay" '(eat-enter-copy-mode :wk "copy mode")

    "aC" '(claude-code-ide :wk "claude direct")
    "aP" '(copilot-cli :wk "copilot direct")))

(provide 'init-agents)
;;; init-agents.el ends here
