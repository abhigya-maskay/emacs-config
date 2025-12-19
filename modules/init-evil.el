;;; init-evil.el --- Evil mode configuration -*- lexical-binding: t -*-

(setq evil-want-integration t)
(setq evil-want-keybinding nil)

(use-package undo-tree
  :demand t
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.config/emacs/undo-tree")))
  (setq undo-tree-auto-save-history t))

(use-package evil
  :demand t
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-undo-system 'undo-tree)
  :config
  (evil-mode 1))

(global-undo-tree-mode 1)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))

(use-package avy
  :config
  (setq avy-all-windows t)
  (evil-define-key 'normal 'global (kbd "s") 'avy-goto-char-2))

(provide 'init-evil)
;;; init-evil.el ends here
