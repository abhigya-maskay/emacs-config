;;; init-agents.el --- AI Agent configuration -*- lexical-binding: t -*-

;;; Code:

;; Fix for Claude Code spinner "dancing" issue
;; The spinner uses Unicode characters (⛏⚒⚔⚗) that cause line height changes
;; when falling back to fonts with different heights. This sets up consistent
;; font fallbacks for these specific characters.
;; See: https://github.com/be5invis/Iosevka/issues/2741
(defun init-agents--setup-spinner-fonts ()
  "Set up consistent fonts for Claude Code spinner Unicode characters.
This prevents the terminal from 'dancing' due to different font heights."
  ;; The specific Unicode spinner characters used by Claude Code:
  ;; U+26CF (⛏) PICK
  ;; U+2692 (⚒) HAMMER AND PICK
  ;; U+2694 (⚔) CROSSED SWORDS
  ;; U+2697 (⚗) ALEMBIC
  ;; Use Apple Symbols on macOS which has consistent glyph heights
  (when (eq system-type 'darwin)
    (dolist (char '(#x26CF #x2692 #x2694 #x2697))
      (set-fontset-font t char "Apple Symbols" nil 'prepend))))

;; Apply spinner font fix early
(init-agents--setup-spinner-fonts)

;; Set buffer face for agent terminals to use JuliaMono if available
;; This provides broader Unicode coverage and consistent line height for spinners
(defun init-agents--setup-terminal-font ()
  "Set up consistent font for agent terminal buffers.
Uses JuliaMono if available for comprehensive Unicode support."
  (when (and (display-graphic-p)
             (member "JuliaMono" (font-family-list)))
    (buffer-face-set '(:family "JuliaMono"))))

;; Apply JuliaMono to all eat terminals (used by both Claude and Copilot agents)
(with-eval-after-load 'eat
  (add-hook 'eat-mode-hook #'init-agents--setup-terminal-font))

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
