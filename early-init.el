;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;; Disable package.el at startup (using straight.el instead)
(setq package-enable-at-startup nil)

;; Fix native compilation on macOS with Homebrew libgccjit
;; libgccjit needs LIBRARY_PATH to find the GCC driver and runtime libraries
(when (and (eq system-type 'darwin)
           (native-comp-available-p))
  (let* ((base "/opt/homebrew/lib/gcc/current")
         (emutls-dir (car (file-expand-wildcards
                           "/opt/homebrew/Cellar/gcc/*/lib/gcc/current/gcc/aarch64-apple-darwin*/*/libemutls_w.a")))
         (gcc-runtime-dir (when emutls-dir (file-name-directory emutls-dir))))
    (setenv "LIBRARY_PATH"
            (string-join
             (delq nil (list base gcc-runtime-dir (getenv "LIBRARY_PATH")))
             ":"))))

;; Increase GC threshold during startup (will be reset after init)
(setq gc-cons-threshold most-positive-fixnum)

;; Disable UI elements early to prevent flash
(setq default-frame-alist
      '((menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)))

(setq inhibit-startup-message t)

;; Silence native compilation warnings (log to *Warnings* buffer only)
(setq native-comp-async-report-warnings-errors 'silent)

(provide 'early-init)
;;; early-init.el ends here
