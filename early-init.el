;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;; Disable package.el at startup (using straight.el instead)
(setq package-enable-at-startup nil)

;; Increase GC threshold during startup (will be reset after init)
(setq gc-cons-threshold most-positive-fixnum)

;; Disable UI elements early to prevent flash
(setq default-frame-alist
      '((menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)))

;; Disable startup screen
(setq inhibit-startup-message t)

;; Silence native compilation warnings (log to *Warnings* buffer only)
(setq native-comp-async-report-warnings-errors 'silent)

(provide 'early-init)
;;; early-init.el ends here
