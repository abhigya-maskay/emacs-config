;;; claude-native-session.el --- Session management for Claude Native -*- lexical-binding: t -*-

;; Author: ave70011
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:
;; Provides session state management for the Claude Native UI.
;; Sessions are keyed by project directory and track conversation state,
;; buffers, processes, and tool permissions.

;;; Code:

(require 'cl-lib)
(require 'init-utils)
(require 'claude-native-ui)

;;; Internal State

(defvar claude-native--sessions (make-hash-table :test 'equal)
  "Hash table mapping project directories to session state.
Each session is a plist with keys:
  :current-proc        - Active process for current turn
  :history-buf         - History buffer for this session
  :input-buf           - Input buffer for this session
  :session-id          - From CLI result message (for --resume)
  :state               - Current state (idle, running, etc.)
  :line-buffer         - Accumulator for incomplete output lines
  :pending-approvals   - Queue of pending approval requests
  :current-approval    - ID of the approval currently displayed
  :allowed-tools       - List of tool names always-allowed
  :spinner-timer       - Timer for spinner animation
  :spinner-overlay     - Overlay displaying spinner
  :spinner-frame-index - Current spinner animation frame index
  :tool-start-time     - Timestamp when current tool started
  :tool-timer          - Timer for tool elapsed time updates
  :tool-overlay        - Overlay showing tool spinner + time
  :tool-name           - Name of currently running tool
  :last-tool-input     - Plist with :name and :input
  :tool-overlays       - List of tool block overlay plists
  :current-tool-region - Plist with :start :tool-name :input
  :scroll-pending      - Non-nil when scroll is pending
  :scroll-timer        - Timer for deferred scroll execution")

(defvar claude-native--cli-available nil
  "Cached result of CLI availability check.")

;;; Working Directory

(defun claude-native--get-working-directory ()
  "Get the working directory for Claude session.
Returns project root if in a project, otherwise `default-directory'."
  (init-utils-project-root))

;;; CLI Detection

(defun claude-native--detect-cli ()
  "Detect if Claude CLI is available in PATH."
  (executable-find claude-native-cli-path))

(defun claude-native--ensure-cli ()
  "Ensure Claude CLI is available, caching the result.
Signals an error if not found."
  (unless claude-native--cli-available
    (setq claude-native--cli-available (claude-native--detect-cli)))
  (unless claude-native--cli-available
    (user-error "Claude CLI not found. Install from: https://claude.ai/download"))
  claude-native--cli-available)

;;; Session CRUD Operations

(defun claude-native--get-session (&optional directory)
  "Get session state for DIRECTORY.
DIRECTORY defaults to current working directory."
  (gethash (or directory (claude-native--get-working-directory))
           claude-native--sessions))

(defun claude-native--set-session (directory session)
  "Store SESSION for DIRECTORY."
  (puthash directory session claude-native--sessions))

(defun claude-native--remove-session (directory)
  "Remove session for DIRECTORY."
  (remhash directory claude-native--sessions))

(defun claude-native--get-or-create-session (&optional directory)
  "Get or create session state for DIRECTORY."
  (let* ((dir (or directory (claude-native--get-working-directory)))
         (session (gethash dir claude-native--sessions)))
    (unless session
      (setq session (list :current-proc nil
                          :history-buf nil
                          :input-buf nil
                          :session-id nil
                          :state 'idle
                          :line-buffer ""
                          :pending-approvals nil
                          :current-approval nil
                          :allowed-tools '()
                          :approval-stage nil
                          :spinner-timer nil
                          :spinner-overlay nil
                          :spinner-frame-index 0
                          :tool-start-time nil
                          :tool-timer nil
                          :tool-overlay nil
                          :tool-name nil
                          :last-tool-input nil
                          :block-count 0
                          :diff-buffer nil
                          :diff-proposed-buffer nil
                          :diff-window-config nil
                          :tool-overlays nil
                          :current-tool-region nil))
      (puthash dir session claude-native--sessions))
    session))

;;; Turn Arguments

(defun claude-native--build-turn-args (prompt &optional session-id)
  "Build CLI arguments for a conversation turn.
PROMPT is the user message to send.
SESSION-ID, if provided, enables conversation continuation via --resume.
Note: -p (print) mode is required for JSON output but disables AskUserQuestion."
  (let ((args (list "-p"
                    "--verbose"
                    "--output-format" "stream-json")))
    (when session-id
      (setq args (append args (list "--resume" session-id))))
    ;; Use -- to separate options from the prompt
    (append args (list "--" prompt))))

;;; Permission Tracking

(defun claude-native--tool-allowed-p (session tool-name)
  "Return non-nil if TOOL-NAME is in SESSION's always-allowed list."
  (member tool-name (plist-get session :allowed-tools)))

(defun claude-native--allow-tool-always (session tool-name)
  "Add TOOL-NAME to SESSION's always-allowed list."
  (unless (claude-native--tool-allowed-p session tool-name)
    (plist-put session :allowed-tools
               (cons tool-name (plist-get session :allowed-tools)))))

(provide 'claude-native-session)
;;; claude-native-session.el ends here
