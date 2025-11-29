;;; init.el --- Main Emacs configuration -*- lexical-binding: t -*-

;;; Configuration Constants

(defconst init--gc-threshold-normal (* 16 1024 1024)
  "Normal GC threshold after startup (16MB).
During startup, GC is set to `most-positive-fixnum' in early-init.el.")

;;; Module Loading

;; Add modules to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Load modules
(require 'init-straight)  ; Package management (must be first)
(require 'init-utils)     ; Shared utilities
(require 'init-evil)      ; Vim keybindings
(require 'init-which-key) ; Keybinding hints
(require 'init-theme)     ; Theme
(require 'init-completion) ; Completion framework
(require 'init-ui)         ; UI settings
(require 'init-treesitter) ; Treesitter syntax highlighting
(require 'init-keybindings) ; Leader key bindings
(require 'init-git)        ; Git integration
(require 'init-ediff)      ; Ediff configuration
(require 'init-terminal)   ; Terminal emulator configuration
(require 'init-agents)     ; AI agents (Claude Code + Copilot CLI)
(require 'init-lsp)        ; LSP (eglot)
(require 'init-treemacs)   ; Treemacs file tree
(require 'init-markdown)   ; Markdown rendering

;; Reset GC threshold after startup (was set high in early-init.el)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold init--gc-threshold-normal)))

(provide 'init)
;;; init.el ends here
