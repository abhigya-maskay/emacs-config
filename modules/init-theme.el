;;; init-theme.el --- Theme configuration -*- lexical-binding: t -*-

(use-package ef-themes
  :config
  (load-theme 'ef-duo-dark t))

(use-package mood-line
  :config
  (mood-line-mode))

(provide 'init-theme)
;;; init-theme.el ends here
