;;; init-terminal.el --- Terminal configuration -*- lexical-binding: t -*-

;; See: https://emacs.stackexchange.com/questions/2254
(setq recenter-redisplay nil)

(use-package eat
  :straight t
  :commands (eat eat-other-window)
  :custom
  (eat-kill-buffer-on-exit t)
  (eat-term-name "xterm-256color")
  (eat-minimum-latency 0.02)
  (eat-maximum-latency 0.1)
  (eat-term-scrollback-size nil)
)

(defun eat-bottom-panel ()
  "Open eat terminal in a small bottom panel (VS Code style)."
  (interactive)
  (let* ((buf-name "*eat*")
         (existing-buf (get-buffer buf-name))
         (existing-win (and existing-buf (get-buffer-window existing-buf))))
    (cond
     ;; If terminal window is visible, toggle it off
     (existing-win
      (delete-window existing-win))
     ;; If buffer exists but not visible, show it in side window
     (existing-buf
      (let ((win (display-buffer existing-buf
                                 '((display-buffer-in-side-window)
                                   (side . bottom)
                                   (slot . 0)
                                   (window-height . 0.3)
                                   (preserve-size . (nil . t))))))
        (when win (select-window win))))
     ;; Create new eat terminal with display rules
     (t
      (let ((display-buffer-alist
             (cons '("\\*eat\\*"
                     (display-buffer-in-side-window)
                     (side . bottom)
                     (slot . 0)
                     (window-height . 0.3)
                     (preserve-size . (nil . t)))
                   display-buffer-alist)))
        (eat))))))

(dolist (mode '(eat-mode-hook term-mode-hook eshell-mode-hook))
  (add-hook mode (lambda ()
                   (display-line-numbers-mode -1)
                   (setq-local global-hl-line-mode nil)
                   (hl-line-mode -1)
                   (when (bound-and-true-p hl-line-mode)
                     (hl-line-mode -1))
                   (when (fboundp 'pulsar-mode)
                     (pulsar-mode -1)))))

(with-eval-after-load 'evil
  (evil-set-initial-state 'eat-mode 'emacs))

(defun eat-paste-from-clipboard ()
  "Paste from kill ring/system clipboard into eat terminal."
  (interactive)
  (when (eq major-mode 'eat-mode)
    (eat-yank)))

(defvar-local eat-copy-mode-active nil
  "Non-nil when eat copy mode is active.")

(defun eat-enter-copy-mode ()
  "Enter copy mode for selecting and copying text from eat terminal.
Use vim motions to navigate, `v' to start selection, `y' to copy and exit.
Press `q' or `Escape' to exit without copying."
  (interactive)
  (when (eq major-mode 'eat-mode)
    (setq eat-copy-mode-active t)
    (eat-emacs-mode)
    (evil-normal-state)
    (message "Copy mode: navigate with hjkl, v to select, y to copy, q to exit")))

(defun eat-exit-copy-mode ()
  "Exit copy mode and return to normal terminal operation."
  (interactive)
  (when (and (eq major-mode 'eat-mode) eat-copy-mode-active)
    (setq eat-copy-mode-active nil)
    (deactivate-mark)
    (eat-char-mode)
    (evil-emacs-state)
    (message "Exited copy mode")))

(defun eat-copy-mode-yank ()
  "Copy selection to kill ring/clipboard and exit copy mode."
  (interactive)
  (when (and (eq major-mode 'eat-mode) eat-copy-mode-active)
    (if (use-region-p)
        (progn
          (kill-ring-save (region-beginning) (region-end))
          (message "Copied to clipboard"))
      (message "No selection to copy"))
    (eat-exit-copy-mode)))

(with-eval-after-load 'eat
  (define-key eat-char-mode-map (kbd "C-S-v") #'eat-paste-from-clipboard))

(with-eval-after-load 'evil
  (evil-define-key 'normal eat-mode-map
    (kbd "y") #'eat-copy-mode-yank
    (kbd "q") #'eat-exit-copy-mode
    (kbd "<escape>") #'eat-exit-copy-mode))

(provide 'init-terminal)
;;; init-terminal.el ends here
