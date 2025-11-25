;;; init-which-key.el --- Which-key configuration -*- lexical-binding: t -*-

(use-package which-key
  :init
  (setq which-key-idle-delay 0.2)
  (setq which-key-side-window-location 'right)
  (setq which-key-side-window-max-width 0.33)  ; 1/3 of frame width
  (setq which-key-sort-order 'which-key-key-order-alpha)
  :config
  (which-key-mode))

(provide 'init-which-key)
;;; init-which-key.el ends here
