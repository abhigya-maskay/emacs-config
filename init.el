;;; init.el --- Main Emacs configuration -*- lexical-binding: t -*-

;; Add modules to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Load modules
(require 'init-straight)  ; Package management (must be first)
(require 'init-evil)      ; Vim keybindings
(require 'init-which-key) ; Keybinding hints
(require 'init-theme)     ; Theme
(require 'init-completion) ; Completion framework
(require 'init-ui)         ; UI settings
(require 'init-treesitter) ; Treesitter syntax highlighting
(require 'init-keybindings) ; Leader key bindings
(require 'init-git)        ; Git integration
(require 'init-claude)     ; Claude Code AI assistant
(require 'init-lsp)        ; LSP (eglot)
(require 'init-dired)      ; Dired and dirvish

;; Reset GC threshold after startup (was set high in early-init.el)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 16 1024 1024))))

(provide 'init)
;;; init.el ends here
