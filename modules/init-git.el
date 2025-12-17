;;; init-git.el --- Git configuration -*- lexical-binding: t -*-

;; Set emacsclient path for with-editor (GUI Emacs doesn't inherit shell PATH)
(use-package with-editor
  :init
  (when (eq system-type 'darwin)
    (setq with-editor-emacsclient-executable "/opt/homebrew/bin/emacsclient")))

(use-package magit
  :commands magit-status
  :config
  ;; Performance: don't refresh status buffer on every file save
  (setq magit-refresh-status-buffer nil))

(use-package diff-hl
  :hook
  ((prog-mode . diff-hl-mode)
   (text-mode . diff-hl-mode)
   (dired-mode . diff-hl-dired-mode)
   (magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (diff-hl-flydiff-mode))

(provide 'init-git)
;;; init-git.el ends here
