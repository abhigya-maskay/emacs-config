;;; copilot-cli.el --- GitHub Copilot CLI integration -*- lexical-binding: t -*-

;; Author: ave70011
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (eat "0.9"))
;; Keywords: tools, ai

;;; Commentary:
;; Emacs integration for GitHub Copilot CLI (@github/copilot npm package).
;; Provides terminal-based interaction with eat backend.

;;; Code:

(require 'eat)
(require 'init-utils)
(require 'cl-lib)

(defgroup copilot-cli nil
  "GitHub Copilot CLI integration for Emacs."
  :group 'tools
  :prefix "copilot-cli-")

(defcustom copilot-cli-path "copilot"
  "Path to the Copilot CLI executable."
  :type 'string
  :group 'copilot-cli)

(defcustom copilot-cli-window-side 'right
  "Side of frame where Copilot CLI window appears."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Right" right)
                 (const :tag "Top" top)
                 (const :tag "Bottom" bottom))
  :group 'copilot-cli)

(defcustom copilot-cli-window-width 100
  "Width of the Copilot CLI window when on left or right side."
  :type 'integer
  :group 'copilot-cli)

(defcustom copilot-cli-window-height 20
  "Height of the Copilot CLI window when on top or bottom."
  :type 'integer
  :group 'copilot-cli)

(defcustom copilot-cli-focus-on-open t
  "Whether to focus the Copilot CLI window when opened."
  :type 'boolean
  :group 'copilot-cli)

(defcustom copilot-cli-preserve-scroll-position t
  "Maintain terminal scroll position to prevent scrolling through history.
When enabled, prevents the eat terminal from scanning through the entire
buffer history when displaying large output like diffs."
  :type 'boolean
  :group 'copilot-cli)

;;; Internal Variables

;; Eat terminal variables (defined by eat package)
(defvar eat--synchronize-scroll-function)
(defvar eat-terminal)

(defvar copilot-cli--processes (make-hash-table :test 'equal)
  "Hash table mapping project directories to Copilot CLI processes.")

(defvar copilot-cli--cli-available nil
  "Cached result of CLI availability check.")

;;; Utility Functions - CLI Detection

(defun copilot-cli--detect-cli ()
  "Detect if Copilot CLI is available in PATH.
Returns the path if found, nil otherwise."
  (executable-find copilot-cli-path))

(defun copilot-cli--ensure-cli ()
  "Ensure Copilot CLI is available, caching the result.
Signals an error if not found."
  (unless copilot-cli--cli-available
    (setq copilot-cli--cli-available (copilot-cli--detect-cli)))
  (unless copilot-cli--cli-available
    (user-error "Copilot CLI not found. Install with: npm install -g @github/copilot"))
  copilot-cli--cli-available)

;;; Utility Functions - Working Directory

(defun copilot-cli--get-working-directory ()
  "Get the working directory for Copilot CLI.
Returns project root if in a project, otherwise `default-directory'."
  (init-utils-project-root))

;;; Utility Functions - Buffer Naming

(defun copilot-cli--get-buffer-name (&optional directory)
  "Generate buffer name for Copilot CLI.
DIRECTORY defaults to result of `copilot-cli--get-working-directory'.
Returns format: *copilot-cli[project-name]*"
  (let* ((dir (or directory (copilot-cli--get-working-directory)))
         (project-name (file-name-nondirectory (directory-file-name dir))))
    (format "*copilot-cli[%s]*" project-name)))

;;; Terminal Backend - Send Functions

(defun copilot-cli--terminal-send-string (string)
  "Send STRING to the current eat buffer."
  (when (derived-mode-p 'eat-mode)
    (when-let ((proc (get-buffer-process (current-buffer))))
      (process-send-string proc string))))

(defun copilot-cli--terminal-send-return ()
  "Send return key to current eat buffer."
  (copilot-cli--terminal-send-string "\n"))

;;; Window Management

(defun copilot-cli--display-buffer-in-side-window (buffer)
  "Display BUFFER in a side window based on customization.
Returns the window displaying BUFFER."
  (let* ((side copilot-cli-window-side)
         (size (if (memq side '(left right))
                   `(window-width . ,copilot-cli-window-width)
                 `(window-height . ,copilot-cli-window-height))))
    (display-buffer buffer
                    `(display-buffer-in-side-window
                      (side . ,side)
                      (slot . 0)
                      ,size
                      (window-parameters . ((no-delete-other-windows . t)))))))

(defun copilot-cli--toggle-existing-window (buffer)
  "Toggle visibility of window displaying BUFFER.
Returns the window if shown, nil if hidden."
  (if-let ((window (get-buffer-window buffer)))
      (progn
        (delete-window window)
        nil)
    (copilot-cli--display-buffer-in-side-window buffer)))

(defun copilot-cli--show-buffer (buffer)
  "Display BUFFER in side window and optionally focus it."
  (copilot-cli--display-buffer-in-side-window buffer)
  (when copilot-cli-focus-on-open
    (select-window (get-buffer-window buffer))))

;;; Process Management

(defun copilot-cli--get-process (&optional directory)
  "Get Copilot CLI process for DIRECTORY.
DIRECTORY defaults to current working directory."
  (gethash (or directory (copilot-cli--get-working-directory))
           copilot-cli--processes))

(defun copilot-cli--set-process (directory process)
  "Store PROCESS for DIRECTORY in the process hash table."
  (puthash directory process copilot-cli--processes))

(defun copilot-cli--remove-process (directory)
  "Remove process entry for DIRECTORY from hash table."
  (remhash directory copilot-cli--processes))

;;; Process Management - Cleanup

(defun copilot-cli--cleanup-on-exit (directory)
  "Clean up Copilot CLI session for DIRECTORY.
Removes process from tracking table and kills the buffer."
  (let* ((process (copilot-cli--get-process directory))
         (buffer (when process (process-buffer process))))
    (copilot-cli--remove-process directory)
    (when (and buffer (buffer-live-p buffer))
      (let ((window (get-buffer-window buffer t)))
        (kill-buffer buffer)
        (when (and window (window-live-p window) (not (one-window-p t)))
          (delete-window window))))))

(defun copilot-cli--cleanup-all-sessions ()
  "Clean up all Copilot CLI sessions.  For use in `kill-emacs-hook'."
  (maphash (lambda (dir _proc)
             (copilot-cli--cleanup-on-exit dir))
           copilot-cli--processes))

(add-hook 'kill-emacs-hook #'copilot-cli--cleanup-all-sessions)

;;; Scroll Management

(defun copilot-cli--terminal-position-keeper (window-list)
  "Maintain stable terminal view position across window switches.
WINDOW-LIST contains windows requiring position synchronization.
Implements intelligent scroll management to prevent scanning through
entire buffer history when displaying large output."
  (let ((inhibit-redisplay t))
    (dolist (win window-list)
      (if (eq win 'buffer)
          (goto-char (eat-term-display-cursor eat-terminal))
        (unless buffer-read-only
          (let ((terminal-point (eat-term-display-cursor eat-terminal)))
            (set-window-point win terminal-point)
            (cond
             ((>= terminal-point (- (point-max) 2))
              (set-window-point win terminal-point)
              (let ((start (with-selected-window win
                             (save-excursion
                               (goto-char terminal-point)
                               (forward-line (- 1 (window-height win)))
                               (point)))))
                (set-window-start win (max (point-min) start) t)))
             ((not (pos-visible-in-window-p terminal-point win))
              (set-window-point win terminal-point)
              (let ((start (with-selected-window win
                             (save-excursion
                               (goto-char terminal-point)
                               (forward-line (- (/ (window-height win) 2)))
                               (point)))))
                (set-window-start win (max (point-min) start) t))))))))))

;;; Session Management - Command Builder

(defun copilot-cli--build-args (&optional continue resume)
  "Build argument list for Copilot CLI.
If CONTINUE is non-nil, add --continue flag.
If RESUME is non-nil, add --resume flag.
Returns a list of arguments suitable for `eat-exec'."
  (let ((args nil))
    (when resume (push "--resume" args))
    (when continue (push "--continue" args))
    args))

;;; Session Management - Terminal Session Creation

(defun copilot-cli--create-terminal-session (directory &optional continue resume)
  "Create a new eat session for Copilot CLI in DIRECTORY.
CONTINUE and RESUME control CLI flags.
Returns the buffer."
  (let* ((default-directory directory)
         (buffer-name (copilot-cli--get-buffer-name directory))
         (args (copilot-cli--build-args continue resume))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'eat-mode)
        (eat-mode)
        (eat-exec buffer buffer-name copilot-cli-path nil args))
      (when copilot-cli-preserve-scroll-position
        (setq-local eat--synchronize-scroll-function
                    #'copilot-cli--terminal-position-keeper))
      (when-let ((proc (get-buffer-process buffer)))
        (set-process-sentinel proc
          (lambda (_proc event)
            (when (string-match-p "\\(finished\\|exited\\|killed\\|terminated\\)" event)
              (copilot-cli--cleanup-on-exit directory)))))
      (add-hook 'kill-buffer-hook
                (lambda () (copilot-cli--cleanup-on-exit directory))
                nil t))
    (copilot-cli--set-process directory (get-buffer-process buffer))
    buffer))

;;; Session Management - Main Entry

(defun copilot-cli--start-session (&optional continue resume)
  "Start a new Copilot CLI session.
CONTINUE and RESUME control CLI flags.
Returns the session buffer."
  (copilot-cli--ensure-cli)
  (let* ((directory (copilot-cli--get-working-directory))
         (existing-process (copilot-cli--get-process directory)))
    (if (and existing-process (process-live-p existing-process))
        (process-buffer existing-process)
      (copilot-cli--create-terminal-session directory continue resume))))

;;; Interactive Commands

;;;###autoload
(defun copilot-cli ()
  "Start GitHub Copilot CLI for the current project.
If a session already exists, switch to it."
  (interactive)
  (copilot-cli--show-buffer (copilot-cli--start-session)))

;;;###autoload
(defun copilot-cli-continue ()
  "Start Copilot CLI and continue the previous conversation."
  (interactive)
  (copilot-cli--show-buffer (copilot-cli--start-session t nil)))

;;;###autoload
(defun copilot-cli-resume ()
  "Start Copilot CLI and resume a specific session."
  (interactive)
  (copilot-cli--show-buffer (copilot-cli--start-session nil t)))

;;;###autoload
(defun copilot-cli-toggle ()
  "Toggle Copilot CLI window visibility.
If no session exists, start one."
  (interactive)
  (let* ((directory (copilot-cli--get-working-directory))
         (process (copilot-cli--get-process directory)))
    (if (and process (process-live-p process))
        (copilot-cli--toggle-existing-window (process-buffer process))
      (copilot-cli))))

;;;###autoload
(defun copilot-cli-stop ()
  "Stop the Copilot CLI session for the current project."
  (interactive)
  (let* ((directory (copilot-cli--get-working-directory))
         (process (copilot-cli--get-process directory)))
    (if (and process (process-live-p process))
        (progn
          (kill-process process)
          (message "Copilot CLI stopped"))
      (message "No Copilot CLI session running"))))

;;;###autoload
(defun copilot-cli-send-prompt (&optional prompt)
  "Send PROMPT to Copilot CLI.
If PROMPT is nil, read from minibuffer."
  (interactive)
  (let* ((directory (copilot-cli--get-working-directory))
         (process (copilot-cli--get-process directory))
         (text (or prompt (read-string "Prompt: "))))
    (if (and process (process-live-p process))
        (with-current-buffer (process-buffer process)
          (copilot-cli--terminal-send-string text)
          (copilot-cli--terminal-send-return))
      (user-error "No Copilot CLI session running"))))

;;;###autoload
(defun copilot-cli-send-region (start end)
  "Send region from START to END to Copilot CLI."
  (interactive "r")
  (let ((text (buffer-substring-no-properties start end)))
    (copilot-cli-send-prompt text)))

;;;###autoload
(defun copilot-cli-send-escape ()
  "Send Escape key to Copilot CLI."
  (interactive)
  (let* ((directory (copilot-cli--get-working-directory))
         (process (copilot-cli--get-process directory)))
    (if (and process (process-live-p process))
        (process-send-string process "\e")
      (user-error "No Copilot CLI session running"))))

;;;###autoload
(defun copilot-cli-switch-to-buffer ()
  "Switch to the Copilot CLI buffer for current project."
  (interactive)
  (let* ((directory (copilot-cli--get-working-directory))
         (process (copilot-cli--get-process directory)))
    (if (and process (process-live-p process))
        (pop-to-buffer (process-buffer process))
      (user-error "No Copilot CLI session running"))))

(provide 'copilot-cli)
;;; copilot-cli.el ends here
