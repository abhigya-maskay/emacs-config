;;; init-completion.el --- Completion framework -*- lexical-binding: t -*-

;; Vertical completion UI
(use-package vertico
  :init
  (vertico-mode))

;; Flexible matching (space-separated, out-of-order)
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

;; Rich annotations in the minibuffer
(use-package marginalia
  :init
  (marginalia-mode))

;; Enhanced commands (search, buffers, etc.)
(use-package consult
  :bind
  ([remap switch-to-buffer] . consult-buffer)
  ([remap goto-line] . consult-goto-line))

;; Track recent files
(use-package recentf
  :straight (:type built-in)
  :init
  (recentf-mode))

;; Persist minibuffer history across sessions
(use-package savehist
  :straight (:type built-in)
  :init
  (savehist-mode))

;; In-buffer completion popup
(use-package corfu
  :custom
  (corfu-auto t)              ; Auto-show popup
  (corfu-auto-delay 0.2)      ; Delay before showing
  (corfu-auto-prefix 2)       ; Min chars before showing
  (corfu-cycle t)             ; Cycle through candidates
  (corfu-preselect 'prompt)   ; Don't preselect first candidate
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
        ("RET" . corfu-complete))
  :init
  (global-corfu-mode))

;; Add icons to corfu completions
(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Extra completion sources
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(provide 'init-completion)
;;; init-completion.el ends here
