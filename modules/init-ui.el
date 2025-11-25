;;; init-ui.el --- UI configuration -*- lexical-binding: t -*-

;; Line numbers - relative with absolute on current line
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Font configuration
(set-face-attribute 'default nil
                    :family "Monaspace Argon Frozen"
                    :height 140)

;; Rainbow delimiters - colored parentheses
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Indent guides - visual indentation lines
(use-package indent-bars
  :hook (prog-mode . indent-bars-mode)
  :custom
  (indent-bars-treesit-support t))

;; Smooth pixel scrolling
(pixel-scroll-precision-mode 1)

;; Pulse current line after jumps
(use-package pulsar
  :config
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.05)
  (setq pulsar-iterations 10)
  (pulsar-global-mode 1))

;; Better help buffers
(use-package helpful)

(provide 'init-ui)
;;; init-ui.el ends here
