;;; init-terminal.el --- Terminal configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Configuration for terminal emulators (eat, term, eshell) including
;; visual settings and Evil integration.

;;; Code:

;; Disable recenter redisplay globally - major cause of terminal flickering
;; See: https://emacs.stackexchange.com/questions/2254
(setq recenter-redisplay nil)

;; eat (Emulate A Terminal - pure Emacs Lisp terminal)
(use-package eat
  :straight t
  :commands (eat eat-other-window)
  :custom
  (eat-kill-buffer-on-exit t)
  ;; Latency tuning to reduce flickering
  ;; Minimum time to wait for more output before redisplay (reduces flicker from partial updates)
  (eat-minimum-latency 0.008)  ; ~120fps, wait a bit for output chunks to batch
  ;; Maximum time before forcing redisplay (prevents indefinite delays)
  (eat-maximum-latency 0.033)  ; ~30fps cap, ensures responsiveness
  ;; Larger read chunk size for smoother updates
  (eat-term-scrollback-size nil))

;; Disable visual clutter in terminal modes (prevents flickering)
(dolist (mode '(eat-mode-hook term-mode-hook eshell-mode-hook))
  (add-hook mode (lambda ()
                   (display-line-numbers-mode -1)
                   (hl-line-mode -1)
                   (when (fboundp 'pulsar-mode)
                     (pulsar-mode -1)))))

;; Use Emacs state in terminal modes (not vim normal mode)
(with-eval-after-load 'evil
  (evil-set-initial-state 'eat-mode 'emacs))

(provide 'init-terminal)
;;; init-terminal.el ends here
