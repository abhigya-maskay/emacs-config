;;; init-terminal.el --- Terminal configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Configuration for terminal emulators (eat, term, eshell) including
;; visual settings and Evil integration.

;;; Code:

;; eat (Emulate A Terminal - pure Emacs Lisp terminal)
(use-package eat
  :straight t
  :commands (eat eat-other-window)
  :custom
  (eat-kill-buffer-on-exit t))

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
