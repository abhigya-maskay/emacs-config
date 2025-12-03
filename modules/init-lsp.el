;;; init-lsp.el --- LSP configuration -*- lexical-binding: t -*-

(use-package eglot
  :straight (:type built-in)
  :hook
  ((python-ts-mode . eglot-ensure)
   (js-ts-mode . eglot-ensure)
   (typescript-ts-mode . eglot-ensure)
   (tsx-ts-mode . eglot-ensure)
   (rust-ts-mode . eglot-ensure)
   (go-ts-mode . eglot-ensure)
   (c-ts-mode . eglot-ensure)
   (c++-ts-mode . eglot-ensure)
   (ruby-ts-mode . eglot-ensure))
  :config
  (setq eglot-events-buffer-size 0)
  (setq eglot-sync-connect nil)
  (setq eglot-autoshutdown t))

(use-package flymake
  :straight (:type built-in)
  :hook (prog-mode . flymake-mode)
  :config
  (setq flymake-no-changes-timeout 0.5))

(use-package eldoc-box
  :hook (eglot-managed-mode . eldoc-box-hover-mode))

(provide 'init-lsp)
;;; init-lsp.el ends here
