;;; init-utils.el --- Shared utility functions -*- lexical-binding: t -*-

;;; Commentary:
;; Common utility functions shared across multiple modules.

;;; Code:

(require 'project)

(defun init-utils-project-root ()
  "Get current project root directory.
Returns project root if in a project, otherwise `default-directory'."
  (or (when-let ((project (project-current)))
        (project-root project))
      default-directory))

(defun init-utils-project-remember-current ()
  "Add the current project to the known projects list."
  (interactive)
  (project-remember-project (project-current t)))

(defun init-utils-reload-config ()
  "Reload the Emacs configuration."
  (interactive)
  (load user-init-file))

(provide 'init-utils)
;;; init-utils.el ends here
