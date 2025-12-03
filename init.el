;;; init.el --- Main Emacs configuration -*- lexical-binding: t -*-

;;; Configuration Constants

(defconst init--gc-threshold-normal (* 16 1024 1024)
  "Normal GC threshold after startup (16MB).
During startup, GC is set to `most-positive-fixnum' in early-init.el.")

;;; Module Loading

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(require 'init-straight)
(require 'init-utils)
(require 'init-evil)
(require 'init-which-key)
(require 'init-theme)
(require 'init-completion)
(require 'init-ui)
(require 'init-treesitter)
(require 'init-keybindings)
(require 'init-git)
(require 'init-ediff)
(require 'init-terminal)
(require 'init-agents)
(require 'init-lsp)
(require 'init-treemacs)
(require 'init-markdown)

(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold init--gc-threshold-normal)))

(provide 'init)
;;; init.el ends here
