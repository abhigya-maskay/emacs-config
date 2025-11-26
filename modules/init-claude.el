;;; init-claude.el --- Claude Code integration -*- lexical-binding: t -*-

;;; Code:

;; vterm (terminal backend with anti-flicker support)
(use-package vterm
  :commands (vterm vterm-other-window)
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 10000))

;; claude-code-ide.el - has built-in anti-flicker and diff support
(use-package claude-code-ide
  :straight (:host github :repo "manzaltu/claude-code-ide.el")
  :custom
  ;; Use vterm for anti-flicker support
  (claude-code-ide-terminal-backend 'vterm)
  ;; Anti-flicker settings (enabled by default, but being explicit)
  (claude-code-ide-vterm-anti-flicker t)
  (claude-code-ide-vterm-render-delay 0.005)
  (claude-code-ide-prevent-reflow-glitch t)
  ;; Window configuration
  (claude-code-ide-use-side-window t)
  (claude-code-ide-window-side 'right)
  (claude-code-ide-window-width 100)
  (claude-code-ide-focus-on-open t)
  ;; Use IDE diff viewer (built-in ediff integration)
  (claude-code-ide-use-ide-diff t)
  :config
  (claude-code-ide-emacs-tools-setup))

;; Ediff side-by-side configuration
(use-package ediff
  :straight (:type built-in)
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  :config
  ;; Save/restore window config around ediff
  (defvar my/ediff-saved-window-config nil)

  (defun my/ediff-save-window-config ()
    (setq my/ediff-saved-window-config (current-window-configuration)))

  (defun my/ediff-restore-window-config ()
    (when my/ediff-saved-window-config
      (set-window-configuration my/ediff-saved-window-config)
      (setq my/ediff-saved-window-config nil)))

  (add-hook 'ediff-before-setup-hook #'my/ediff-save-window-config)
  (add-hook 'ediff-quit-hook #'my/ediff-restore-window-config)

  ;; Evil-friendly navigation in ediff
  (add-hook 'ediff-keymap-setup-hook
            (lambda ()
              (define-key ediff-mode-map (kbd "j") 'ediff-next-difference)
              (define-key ediff-mode-map (kbd "k") 'ediff-previous-difference))))

;; Use emacs state in vterm
(with-eval-after-load 'evil
  (evil-set-initial-state 'vterm-mode 'emacs))

;; Evil leader keybindings
(with-eval-after-load 'general
  (leader-keys
    ;; Quick toggle under SPC o (open)
    "oa" '(claude-code-ide-toggle :wk "agent pane")

    ;; Full agent commands under SPC a
    "a" '(:ignore t :wk "agent")
    "aa" '(claude-code-ide :wk "start claude")
    "at" '(claude-code-ide-toggle :wk "toggle")
    "ac" '(claude-code-ide-continue :wk "continue")
    "ar" '(claude-code-ide-resume :wk "resume")
    "as" '(claude-code-ide-send-prompt :wk "send prompt")
    "aR" '(claude-code-ide-insert-at-mentioned :wk "send region")
    "ab" '(claude-code-ide-switch-to-buffer :wk "switch buffer")
    "al" '(claude-code-ide-list-sessions :wk "list sessions")
    "ak" '(claude-code-ide-stop :wk "stop session")
    "am" '(claude-code-ide-menu :wk "menu")))

;; Helper: Ediff current file with git HEAD
(defun my/ediff-with-git-head ()
  "Compare current buffer with git HEAD using ediff."
  (interactive)
  (if (and (buffer-file-name) (vc-backend (buffer-file-name)))
      (ediff-vc-internal "" (list (buffer-file-name)))
    (message "Buffer is not under version control")))

(provide 'init-claude)
;;; init-claude.el ends here
