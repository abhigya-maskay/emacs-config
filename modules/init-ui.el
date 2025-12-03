;;; init-ui.el --- UI configuration -*- lexical-binding: t -*-

;;; Configuration

(defconst init-ui--default-font-family "Monaspace Argon Frozen"
  "Default font family for the editor.")

(defconst init-ui--default-font-height 140
  "Default font height in 1/10 pt units (140 = 14pt).")

;;; Display Settings

;; Line numbers - relative with absolute on current line
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Auto-revert buffers when files change on disk (essential for external tools like Claude Code)
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)           ; Don't show messages when reverting
(setq auto-revert-check-vc-info t)       ; Also update version control info

;; Font configuration
(set-face-attribute 'default nil
                    :family init-ui--default-font-family
                    :height init-ui--default-font-height)

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

;; Prevent aggressive recentering at buffer boundaries (reduces flickering)
(setq scroll-conservatively 101)  ; Never recenter, scroll line-by-line
(setq scroll-margin 0)
(setq scroll-step 1)
(setq scroll-preserve-screen-position t)  ; Keep cursor position stable during scroll

;; Defer font-lock during rapid scrolling
(setq jit-lock-defer-time 0.05)

;; Fast scrolling for large output (trades precision for speed)
(setq fast-but-imprecise-scrolling t)

;; Reduce redisplay during output (helps with terminal flickering)
(setq redisplay-skip-fontification-on-input t)

;; Pulse current line after jumps
(use-package pulsar
  :config
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.05)
  (setq pulsar-iterations 10)
  ;; Disable pulses that can cause terminal flickering
  (setq pulsar-pulse-on-window-change nil)
  (pulsar-global-mode 1))

;; Better help buffers
(use-package helpful)

(provide 'init-ui)
;;; init-ui.el ends here
