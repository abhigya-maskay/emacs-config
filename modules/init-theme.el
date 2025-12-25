;;; init-theme.el --- Theme configuration -*- lexical-binding: t -*-

(use-package catppuccin-theme
  :straight t
  :demand t
  :custom
  (catppuccin-flavor 'mocha)
  :config
  (load-theme 'catppuccin :no-confirm))

(use-package mood-line
  :config
  (mood-line-mode))

(provide 'init-theme)
;;; init-theme.el ends here
