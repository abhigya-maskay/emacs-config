;;; init-dired.el --- Dired and dirvish configuration -*- lexical-binding: t -*-

(use-package dired
  :straight (:type built-in)
  :config
  (setq dired-listing-switches "-alh --group-directories-first")
  (setq dired-dwim-target t)  ; Suggest other dired buffer as target
  (setq dired-kill-when-opening-new-dired-buffer t))  ; Single dired buffer

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :config
  (setq dirvish-attributes '(file-size collapse subtree-state vc-state git-msg))
  (setq dirvish-mode-line-format '(:left (sort file-time symlink) :right (omit yank index)))
  ;; Use q to quit side window
  (setq dirvish-side-follow-mode t))

(provide 'init-dired)
;;; init-dired.el ends here
