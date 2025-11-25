;;; init-lsp.el --- LSP configuration -*- lexical-binding: t -*-

(use-package eglot
  :straight (:type built-in)
  :hook
  ;; Auto-start eglot for these modes
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
  ;; Performance tuning
  (setq eglot-events-buffer-size 0)  ; Disable events logging
  (setq eglot-sync-connect nil)      ; Don't block on connect
  (setq eglot-autoshutdown t))       ; Shutdown server when last buffer closes

;; Flymake - inline error checking (works with eglot)
(use-package flymake
  :straight (:type built-in)
  :hook (prog-mode . flymake-mode)
  :config
  (setq flymake-no-changes-timeout 0.5))  ; Check after 0.5s idle

;; Eldoc-box - show docs in popup at point
(use-package eldoc-box
  :hook (eglot-managed-mode . eldoc-box-hover-mode))

(provide 'init-lsp)
;;; init-lsp.el ends here
