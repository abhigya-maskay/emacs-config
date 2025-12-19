;;; init-ediff.el --- Ediff configuration -*- lexical-binding: t -*-

(use-package ediff
  :straight (:type built-in)
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  :config
  (defvar init-ediff--saved-window-config nil
    "Window configuration saved before ediff session.")

  (defun init-ediff--save-window-config ()
    "Save current window configuration before ediff."
    (setq init-ediff--saved-window-config (current-window-configuration)))

  (defun init-ediff--restore-window-config ()
    "Restore window configuration after ediff."
    (when init-ediff--saved-window-config
      (set-window-configuration init-ediff--saved-window-config)
      (setq init-ediff--saved-window-config nil)))

  (add-hook 'ediff-before-setup-hook #'init-ediff--save-window-config)
  (add-hook 'ediff-quit-hook #'init-ediff--restore-window-config)

  (add-hook 'ediff-keymap-setup-hook
            (lambda ()
              (define-key ediff-mode-map (kbd "j") 'ediff-next-difference)
              (define-key ediff-mode-map (kbd "k") 'ediff-previous-difference))))

;;;###autoload
(defun init-ediff-with-git-head ()
  "Compare current buffer with git HEAD using ediff."
  (interactive)
  (if (and (buffer-file-name) (vc-backend (buffer-file-name)))
      (ediff-vc-internal "" (list (buffer-file-name)))
    (user-error "Buffer is not under version control")))

(provide 'init-ediff)
;;; init-ediff.el ends here
