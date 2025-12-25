;;; init-treemacs.el --- Treemacs file tree configuration -*- lexical-binding: t -*-

(use-package treemacs
  :defer t
  :config
  (setq treemacs-width 35
        treemacs-position 'left
        treemacs-show-hidden-files t
        treemacs-follow-after-init t
        treemacs-project-follow-cleanup t
        treemacs-persist-file nil)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-git-mode 'deferred))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(provide 'init-treemacs)
;;; init-treemacs.el ends here
