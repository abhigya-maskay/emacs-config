;;; init-evil.el --- Evil mode configuration -*- lexical-binding: t -*-

;; Required for evil-collection
(setq evil-want-integration t)
(setq evil-want-keybinding nil)

;; Use undo-tree for evil undo
(use-package undo-tree
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.config/emacs/undo-tree")))
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode 1))

(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-undo-system 'undo-tree)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Surround text objects (like vim-surround)
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;; Jump between matching tags/brackets with %
(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))

;; Avy - jump to any visible text
(use-package avy
  :config
  (setq avy-all-windows t)  ; Jump across all windows
  ;; Bind 's' in normal mode for quick char jump
  (evil-define-key 'normal 'global (kbd "s") 'avy-goto-char-2))

(provide 'init-evil)
;;; init-evil.el ends here
