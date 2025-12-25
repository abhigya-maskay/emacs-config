;;; init-ui.el --- UI configuration -*- lexical-binding: t -*-

(defconst init-ui--default-font-family "Monaspace Argon Frozen"
  "Default font family for the editor.")

(defconst init-ui--default-font-height 140
  "Default font height in 1/10 pt units (140 = 14pt).")

(setq select-enable-clipboard t)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)
(setq global-hl-line-modes '(not eat-mode term-mode eshell-mode))
(global-hl-line-mode 1)

(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)
(setq auto-revert-check-vc-info t)

(set-face-attribute 'default nil
                    :family init-ui--default-font-family
                    :height init-ui--default-font-height)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package indent-bars
  :hook (prog-mode . indent-bars-mode)
  :custom
  (indent-bars-treesit-support t))

(pixel-scroll-precision-mode 1)

(setq scroll-conservatively 101)
(setq scroll-margin 0)
(setq scroll-step 1)
(setq scroll-preserve-screen-position t)
(setq jit-lock-defer-time 0.05)
(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

(use-package pulsar
  :config
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.05)
  (setq pulsar-iterations 10)
  (setq pulsar-pulse-on-window-change nil)
  (pulsar-global-mode 1))

(use-package helpful)

(provide 'init-ui)
;;; init-ui.el ends here
