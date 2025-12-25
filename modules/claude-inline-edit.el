;;; claude-inline-edit.el --- Inline AI editing with Claude -*- lexical-binding: t -*-

;; Author: ave70011
;; Package-Requires: ((emacs "29.1") (posframe "1.0"))
;; Keywords: tools, ai

;;; Commentary:
;; Provides inline AI-assisted editing using Claude's haiku model.
;; Triggered via SPC SPC, shows posframe for input, displays diffs
;; for approval, and supports iterative refinement.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'posframe)
(require 'claude-native-ui)
(require 'claude-native-log)
(require 'claude-native-diff)
(require 'claude-native-approval)

;;; Customization

(defgroup claude-inline-edit nil
  "Inline AI editing with Claude."
  :group 'claude-native
  :prefix "claude-inline-edit-")

(defcustom claude-inline-edit-model "haiku"
  "Model to use for inline edits.
Use `haiku' for fast responses, `sonnet' for more complex edits."
  :type 'string
  :group 'claude-inline-edit)

(defcustom claude-inline-edit-posframe-border-color "#585b70"
  "Border color for the input posframe.
Default matches Catppuccin Macchiato surface2."
  :type 'string
  :group 'claude-inline-edit)

(defcustom claude-inline-edit-posframe-background "#24273a"
  "Background color for input posframe.
Default matches Catppuccin Macchiato base."
  :type 'string
  :group 'claude-inline-edit)

(defcustom claude-inline-edit-posframe-foreground "#cad3f5"
  "Foreground color for input posframe.
Default matches Catppuccin Macchiato text."
  :type 'string
  :group 'claude-inline-edit)

(defcustom claude-inline-edit-posframe-width 60
  "Minimum width of the input posframe."
  :type 'integer
  :group 'claude-inline-edit)

;;; Faces

(defface claude-inline-edit-header-face
  '((t :inherit mode-line-emphasis :height 0.9))
  "Face for posframe header line."
  :group 'claude-inline-edit)

(defface claude-inline-edit-iteration-face
  '((t :inherit warning :weight bold))
  "Face for iteration count display."
  :group 'claude-inline-edit)

(defface claude-inline-edit-selection-face
  '((t :inherit font-lock-string-face))
  "Face for selection info display."
  :group 'claude-inline-edit)

(defcustom claude-inline-edit-timeout 30
  "Timeout in seconds for inline edit response."
  :type 'integer
  :group 'claude-inline-edit)

;;; Modeline Spinner

(defvar claude-inline-edit--spinner-frames '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Animation frames for the modeline spinner.")

(defvar claude-inline-edit--spinner-index 0
  "Current frame index in spinner animation.")

(defvar claude-inline-edit--spinner-timer nil
  "Timer for updating spinner animation.")

(defvar claude-inline-edit--spinner-start-time nil
  "Time when spinner started, for elapsed time display.")

(defvar claude-inline-edit--modeline-string ""
  "String displayed in modeline during inline edit.")

(defun claude-inline-edit--start-spinner ()
  "Start modeline spinner animation."
  (setq claude-inline-edit--spinner-start-time (current-time))
  (setq claude-inline-edit--spinner-index 0)
  (claude-inline-edit--update-spinner)
  (setq claude-inline-edit--spinner-timer
        (run-with-timer 0.1 0.1 #'claude-inline-edit--update-spinner)))

(defun claude-inline-edit--stop-spinner ()
  "Stop modeline spinner and clear display."
  (when claude-inline-edit--spinner-timer
    (cancel-timer claude-inline-edit--spinner-timer)
    (setq claude-inline-edit--spinner-timer nil))
  (setq claude-inline-edit--modeline-string "")
  (force-mode-line-update t))

(defun claude-inline-edit--update-spinner ()
  "Update spinner frame and elapsed time in modeline."
  (let* ((frame (nth claude-inline-edit--spinner-index
                     claude-inline-edit--spinner-frames))
         (elapsed (float-time (time-subtract (current-time)
                                             claude-inline-edit--spinner-start-time))))
    (setq claude-inline-edit--spinner-index
          (mod (1+ claude-inline-edit--spinner-index)
               (length claude-inline-edit--spinner-frames)))
    (setq claude-inline-edit--modeline-string
          (propertize (format " %s %.1fs " frame elapsed)
                      'face 'claude-native-spinner-face))
    (force-mode-line-update t)))

(unless (member '(:eval claude-inline-edit--modeline-string) global-mode-string)
  (push '(:eval claude-inline-edit--modeline-string) global-mode-string))

;;; Session State

(defvar claude-inline-edit--current-session nil
  "The currently active inline edit session plist.

When non-nil, contains:
  :source-buffer     - Buffer being edited
  :source-file       - File path (real file or temp file for non-file buffers)
  :temp-file         - Temp file path if created, nil for file-backed buffers
  :original-content  - Buffer content at edit start
  :cursor-pos        - Point position when edit started
  :cursor-line       - Line number when edit started
  :selection         - Cons cell (start . end) or nil
  :selection-text    - Text of selection (if any)
  :prompt            - User's instruction
  :iteration         - Current iteration count (0-based)
  :feedback-history  - List of rejection reasons (newest first)
  :process           - Claude CLI process object
  :line-buffer       - Accumulator for incomplete JSON lines
  :state             - Symbol: input, waiting, approving, done, error
  :pending-edits     - List of edit plists awaiting approval
  :current-edit      - Current edit plist being shown for approval
  :connection        - TCP connection for sending approval response")

;;; Input Mode

(defvar claude-inline-edit-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'claude-inline-edit--submit-prompt)
    (define-key map (kbd "C-c C-c") #'claude-inline-edit--submit-prompt)
    (define-key map (kbd "C-g") #'claude-inline-edit--cancel-input)
    (define-key map (kbd "<escape>") #'claude-inline-edit--cancel-input)
    map)
  "Keymap for inline edit input buffer.")

(define-derived-mode claude-inline-edit-input-mode fundamental-mode "InlineEdit"
  "Major mode for inline edit prompt input."
  (setq-local header-line-format
              '(:eval (claude-inline-edit--input-header-line))))

(defun claude-inline-edit--input-header-line ()
  "Generate styled header line for input buffer."
  (let* ((session claude-inline-edit--current-session)
         (iteration (when session (plist-get session :iteration)))
         (sel (when session (plist-get session :selection-text))))
    (concat
     (propertize " Inline Edit " 'face 'claude-inline-edit-header-face)
     (when (and iteration (> iteration 0))
       (propertize (format " #%d " iteration) 'face 'claude-inline-edit-iteration-face))
     (when sel
       (propertize (format " %d chars selected " (length sel)) 'face 'claude-inline-edit-selection-face))
     (propertize " RET submit | ESC cancel " 'face 'shadow))))

;;; Approval Mode

(defvar claude-inline-edit-approval-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "y") #'claude-inline-edit-accept)
    (define-key map (kbd "n") #'claude-inline-edit-reject)
    (define-key map (kbd "a") #'claude-inline-edit-accept-all)
    (define-key map (kbd "q") #'claude-inline-edit-quit)
    map)
  "Keymap active during inline edit approval.")

(define-minor-mode claude-inline-edit-approval-mode
  "Minor mode for inline edit approval keybindings.
When active, y/n/q keys accept, reject, or quit the inline edit."
  :lighter " [InlineApproval]"
  :keymap claude-inline-edit-approval-mode-map
  :global t)

;;; Helper Functions

(defun claude-inline-edit--mode-extension (mode)
  "Return file extension for MODE."
  (pcase mode
    ('emacs-lisp-mode ".el")
    ('lisp-interaction-mode ".el")
    ('python-mode ".py")
    ('python-ts-mode ".py")
    ('js-mode ".js")
    ('js-ts-mode ".js")
    ('javascript-mode ".js")
    ('typescript-mode ".ts")
    ('typescript-ts-mode ".ts")
    ('tsx-ts-mode ".tsx")
    ('rust-mode ".rs")
    ('rust-ts-mode ".rs")
    ('go-mode ".go")
    ('go-ts-mode ".go")
    ('c-mode ".c")
    ('c-ts-mode ".c")
    ('c++-mode ".cpp")
    ('c++-ts-mode ".cpp")
    ('java-mode ".java")
    ('java-ts-mode ".java")
    ('ruby-mode ".rb")
    ('ruby-ts-mode ".rb")
    ('sh-mode ".sh")
    ('bash-ts-mode ".sh")
    ('html-mode ".html")
    ('mhtml-mode ".html")
    ('css-mode ".css")
    ('css-ts-mode ".css")
    ('json-mode ".json")
    ('json-ts-mode ".json")
    ('yaml-mode ".yaml")
    ('yaml-ts-mode ".yaml")
    ('markdown-mode ".md")
    ('org-mode ".org")
    (_ ".txt")))

(defun claude-inline-edit--normalize-quotes (str)
  "Normalize quote characters in STR.
Converts curly quotes to straight quotes for consistent matching."
  (let ((result str))
    ;; Single quotes: curly → straight (using explicit Unicode code points)
    (setq result (string-replace "\u2018" "'" result))  ; LEFT SINGLE QUOTATION MARK
    (setq result (string-replace "\u2019" "'" result))  ; RIGHT SINGLE QUOTATION MARK
    (setq result (string-replace "\u201B" "'" result))  ; SINGLE HIGH-REVERSED-9 QUOTATION MARK
    ;; Double quotes: curly → straight
    (setq result (string-replace "\u201C" "\"" result)) ; LEFT DOUBLE QUOTATION MARK
    (setq result (string-replace "\u201D" "\"" result)) ; RIGHT DOUBLE QUOTATION MARK
    (setq result (string-replace "\u201F" "\"" result)) ; DOUBLE HIGH-REVERSED-9 QUOTATION MARK
    ;; Backticks/graves
    (setq result (string-replace "\u0060" "`" result))  ; GRAVE ACCENT
    (setq result (string-replace "\u2018" "`" result))  ; Sometimes used as backtick
    result))

(defun claude-inline-edit--find-and-replace (old-string new-string)
  "Find OLD-STRING in buffer and replace with NEW-STRING.
Tries exact match first, then normalized quotes.
Returns t if replacement was made, nil otherwise."
  (save-excursion
    (goto-char (point-min))
    (if (search-forward old-string nil t)
        (progn
          (replace-match new-string t t)
          t)
      ;; Fallback: normalize curly quotes to straight quotes for matching
      (let* ((norm-old (claude-inline-edit--normalize-quotes old-string))
             (buffer-content (buffer-substring-no-properties (point-min) (point-max)))
             (norm-buffer (claude-inline-edit--normalize-quotes buffer-content)))
        ;; Position maps 1:1 since Emacs counts characters, not bytes
        (when-let ((pos (string-match (regexp-quote norm-old) norm-buffer)))
          (goto-char (+ (point-min) pos))
          (delete-region (point) (+ (point) (length old-string)))
          (insert new-string)
          t)))))

;;; Entry Point

;;;###autoload
(defun claude-inline-edit ()
  "Start an inline edit at point.

Shows a posframe popup for entering an edit instruction.
The current buffer content is sent as context along with cursor
position and any selected text.

Works with both file-backed buffers and non-file buffers like *scratch*."
  (interactive)
  (when claude-inline-edit--current-session
    (user-error "Inline edit already in progress. Use `q' to cancel"))
  (let* ((buf (current-buffer))
         (content (buffer-substring-no-properties (point-min) (point-max)))
         (pos (point))
         (line (line-number-at-pos pos))
         (selection (when (use-region-p)
                      (cons (region-beginning) (region-end))))
         (sel-text (when selection
                     (buffer-substring-no-properties
                      (car selection) (cdr selection))))
         (temp-file (unless buffer-file-name
                      (let ((ext (claude-inline-edit--mode-extension major-mode)))
                        (make-temp-file "claude-inline-" nil ext))))
         (file (or buffer-file-name temp-file))
         (session (list :source-buffer buf
                        :source-file file
                        :temp-file temp-file
                        :original-content content
                        :cursor-pos pos
                        :cursor-line line
                        :selection selection
                        :selection-text sel-text
                        :prompt nil
                        :iteration 0
                        :feedback-history nil
                        :process nil
                        :line-buffer ""
                        :state 'input
                        :pending-edits nil
                        :current-edit nil
                        :connection nil
                        :diff-buffer nil
                        :diff-window-config nil)))
    (when temp-file
      (with-temp-file temp-file
        (insert content)))
    (setq claude-inline-edit--current-session session)
    (deactivate-mark)
    (claude-inline-edit--show-prompt-posframe)
    (message "Inline edit: Enter instruction at cursor position")))

;;; Posframe UI

(defun claude-inline-edit--show-prompt-posframe ()
  "Show posframe for prompt input."
  (let* ((session claude-inline-edit--current-session)
         (buf (get-buffer-create "*claude-inline-prompt*"))
         (pos (plist-get session :cursor-pos))
         (source-buf (plist-get session :source-buffer)))
    (with-current-buffer buf
      (claude-inline-edit-input-mode)
      (erase-buffer))
    (let ((frame (with-current-buffer source-buf
                   (posframe-show buf
                                  :position pos
                                  :poshandler #'posframe-poshandler-point-bottom-left-corner
                                  :internal-border-width 2
                                  :internal-border-color claude-inline-edit-posframe-border-color
                                  :background-color claude-inline-edit-posframe-background
                                  :foreground-color claude-inline-edit-posframe-foreground
                                  :left-fringe 8
                                  :right-fringe 8
                                  :min-width claude-inline-edit-posframe-width
                                  :min-height 1
                                  :accept-focus t))))
      (when (framep frame)
        (select-frame-set-input-focus frame)
        (select-window (frame-selected-window frame))
        (when (bound-and-true-p evil-local-mode)
          (evil-insert-state))))))

(defun claude-inline-edit--submit-prompt ()
  "Submit the current prompt and start the edit process."
  (interactive)
  (let* ((prompt (string-trim (buffer-string)))
         (session claude-inline-edit--current-session))
    (if (string-empty-p prompt)
        (message "Please enter an instruction")
      (plist-put session :prompt prompt)
      (plist-put session :iteration (1+ (plist-get session :iteration)))
      (posframe-hide "*claude-inline-prompt*")
      (let ((source-buf (plist-get session :source-buffer)))
        (if-let ((win (get-buffer-window source-buf)))
            (select-window win)
          (pop-to-buffer source-buf)))
      (message "Inline edit: Sending to Claude (%s)..." claude-inline-edit-model)
      (claude-inline-edit--spawn))))

(defun claude-inline-edit--cancel-input ()
  "Cancel the inline edit input."
  (interactive)
  (posframe-hide "*claude-inline-prompt*")
  (when claude-inline-edit--current-session
    (let ((source-buf (plist-get claude-inline-edit--current-session :source-buffer)))
      (if-let ((win (get-buffer-window source-buf)))
          (select-window win)
        (pop-to-buffer source-buf))))
  (claude-inline-edit--cleanup)
  (message "Inline edit cancelled"))

;;; Prompt Construction

(defun claude-inline-edit--build-prompt ()
  "Build the full prompt string for the current session.
Passes minimal context and lets Claude Code handle it as a regular edit."
  (let* ((session claude-inline-edit--current-session)
         (source-file (plist-get session :source-file))
         (line-num (plist-get session :cursor-line))
         (sel-text (plist-get session :selection-text))
         (instruction (plist-get session :prompt))
         (feedback-list (plist-get session :feedback-history)))
    (concat
     ;; Basic context
     (format "Edit %s" source-file)
     (if sel-text
         (format " (selected text at line %d):\n```\n%s\n```\n\n" line-num sel-text)
       (format " at line %d:\n\n" line-num))
     ;; User's instruction
     instruction
     ;; Previous rejection context if any
     (when feedback-list
       (concat "\n\n(Previous attempts were rejected: "
               (mapconcat (lambda (fb) (format "\"%s\"" fb))
                          (reverse feedback-list) ", ")
               ")")))))

;;; Process Management

(defun claude-inline-edit--spawn ()
  "Spawn Claude CLI process for inline edit."
  (claude-native--start-approval-server)
  (let* ((session claude-inline-edit--current-session)
         (source-buf (plist-get session :source-buffer))
         (file-path (plist-get session :source-file))
         (default-directory (file-name-directory file-path))
         (prompt (claude-inline-edit--build-prompt))
         (args (list "--model" claude-inline-edit-model
                     "-p"
                     "--verbose"
                     "--output-format" "stream-json"
                     "--" prompt))
         (process-environment (cons "CLAUDE_EMACS_UI=1" process-environment))
         (output-buf (get-buffer-create "*claude-inline-edit-output*"))
         (proc (make-process
                :name "claude-inline-edit"
                :buffer output-buf
                :command (cons "claude" args)
                :noquery t
                :connection-type 'pty
                :coding 'utf-8-emacs-unix
                :filter (lambda (proc output)
                          (with-current-buffer (process-buffer proc)
                            (goto-char (point-max))
                            (insert output))
                          (claude-inline-edit--process-filter proc output))
                :sentinel #'claude-inline-edit--process-sentinel)))
    (plist-put session :process proc)
    (plist-put session :state 'waiting)
    (plist-put session :line-buffer "")
    (claude-inline-edit--start-spinner)
    (run-with-timer claude-inline-edit-timeout nil
      (lambda ()
        (when (and claude-inline-edit--current-session
                   (eq (plist-get claude-inline-edit--current-session :state) 'waiting))
          (message "Inline edit: Request timed out after %ds. Cancelling..."
                   claude-inline-edit-timeout)
          (claude-inline-edit--cleanup))))))

(defun claude-inline-edit--process-filter (_process output)
  "Handle streaming output from Claude CLI.
OUTPUT is the received chunk."
  (let ((session claude-inline-edit--current-session))
    (when session
      (let* ((buffer (concat (plist-get session :line-buffer) output))
             (lines (split-string buffer "\n"))
             (complete-lines (butlast lines))
             (remainder (car (last lines))))
        (plist-put session :line-buffer (or remainder ""))
        (dolist (line complete-lines)
          (unless (string-empty-p line)
            (claude-inline-edit--handle-json-line line)))))))

(defun claude-inline-edit--process-sentinel (process event)
  "Handle Claude CLI process termination with detailed errors.
PROCESS is the CLI process, EVENT describes what happened."
  (claude-inline-edit--stop-spinner)
  (let ((session claude-inline-edit--current-session))
    (when (and session (eq (plist-get session :process) process))
      (cond
       ((string-match-p "finished" event)
        ;; Only cleanup if no edits were received
        (unless (or (plist-get session :pending-edits)
                    (plist-get session :current-edit))
          (message "Inline edit: No changes proposed. Try rephrasing your instruction.")
          (claude-inline-edit--cleanup)))
       ((string-match-p "\\(killed\\|terminated\\)" event)
        (message "Inline edit: Cancelled")
        (claude-inline-edit--cleanup))
       ((string-match-p "exited abnormally" event)
        (message "Inline edit error: Claude CLI failed. Check ~/.claude/logs for details.")
        (plist-put session :state 'error)
        (claude-inline-edit--cleanup))
       ((string-match-p "connection broken" event)
        (message "Inline edit error: Lost connection to Claude. Please try again.")
        (plist-put session :state 'error)
        (claude-inline-edit--cleanup))
       (t
        (message "Inline edit error: %s" (string-trim event))
        (plist-put session :state 'error)
        (claude-inline-edit--cleanup))))))

(defun claude-inline-edit--handle-json-line (line)
  "Parse and handle a JSON LINE from Claude CLI."
  (condition-case nil
      (let* ((json-object-type 'hash-table)
             (json-array-type 'list)
             (msg (json-read-from-string line))
             (msg-type (gethash "type" msg)))
        ;; Log for debugging
        (when (equal msg-type "tool_use")
          (let ((tool-name (gethash "name" msg)))
            (when (equal tool-name "Edit")
              (message "Inline edit: received edit proposal")))))
    (error nil)))

;;; Approval Handling

(defun claude-inline-edit--handle-approval (tool-name input connection)
  "Handle approval request for inline edit.
TOOL-NAME should be \"Edit\" or \"Write\".
INPUT is the hash table with file_path, old_string/new_string (Edit)
or file_path/content (Write).
CONNECTION is the TCP connection to respond on.

Queues edits and shows the first/current one for approval.
Other tools (Bash, Read, etc.) are auto-denied during inline edit."
  (let ((session claude-inline-edit--current-session))
    (when session
      (cond
       ((or (equal tool-name "Edit") (equal tool-name "Write"))
        (claude-inline-edit--stop-spinner)
        ;; Write tool: old_string is empty, content becomes new_string
        (let* ((is-write (equal tool-name "Write"))
               (old-string (if is-write "" (gethash "old_string" input)))
               (new-string (if is-write (gethash "content" input) (gethash "new_string" input)))
               (edit (list :tool-name tool-name
                           :file-path (gethash "file_path" input)
                           :old-string old-string
                           :new-string new-string
                           :connection connection)))
          (plist-put session :pending-edits
                     (append (plist-get session :pending-edits) (list edit)))
          (when (= 1 (length (plist-get session :pending-edits)))
            (claude-inline-edit--show-next-approval session))))
       (t
        (claude-native--log "Inline edit: denying non-edit tool %s" tool-name)
        (let ((response (json-encode `((behavior . "deny")
                                       (message . "Only Edit/Write allowed during inline edit")))))
          (when (and connection (process-live-p connection))
            (process-send-string connection (concat response "\n"))
            (delete-process connection))))))))

(defun claude-inline-edit--show-next-approval (session)
  "Show the next pending edit for approval in SESSION.
If no edits pending, cleanup the session."
  (let ((pending (plist-get session :pending-edits)))
    (if (null pending)
        (progn
          (message "Inline edit: All edits processed")
          (claude-inline-edit--cleanup))
      (let* ((edit (car pending))
             (old-string (plist-get edit :old-string))
             (new-string (plist-get edit :new-string))
             (old-lines (length (split-string old-string "\n" t)))
             (new-lines (length (split-string new-string "\n" t)))
             (diff (- new-lines old-lines))
             (pending-count (length pending)))
        (plist-put session :current-edit edit)
        (plist-put session :connection (plist-get edit :connection))
        (plist-put session :state 'approving)
        (claude-inline-edit--show-diff edit)
        (claude-inline-edit-approval-mode 1)
        (when (bound-and-true-p evil-local-mode)
          (evil-emacs-state))
        (if (> pending-count 1)
            (message "Inline edit: Proposal (%s%d lines, %d pending) %s accept  %s reject  %s accept-all  %s quit"
                     (if (>= diff 0) "+" "") diff
                     (1- pending-count)
                     (propertize "[y]" 'face 'claude-native-approval-key-face)
                     (propertize "[n]" 'face 'claude-native-approval-key-face)
                     (propertize "[a]" 'face 'claude-native-approval-key-face)
                     (propertize "[q]" 'face 'claude-native-approval-key-face))
          (message "Inline edit: Proposal (%s%d lines) %s accept  %s reject  %s quit"
                   (if (>= diff 0) "+" "") diff
                   (propertize "[y]" 'face 'claude-native-approval-key-face)
                   (propertize "[n]" 'face 'claude-native-approval-key-face)
                   (propertize "[q]" 'face 'claude-native-approval-key-face)))))))

(defun claude-inline-edit--show-diff (edit)
  "Show diff for EDIT using claude-native-diff infrastructure."
  (let* ((session claude-inline-edit--current-session)
         (file-path (plist-get edit :file-path))
         (old-string (plist-get edit :old-string))
         (new-string (plist-get edit :new-string)))
    (plist-put session :diff-window-config (current-window-configuration))
    (claude-native--show-diff-approval session file-path old-string new-string)))

;;; Approval Commands

(defun claude-inline-edit-accept ()
  "Accept the current inline edit and show next if available."
  (interactive)
  (unless claude-inline-edit--current-session
    (user-error "No inline edit in progress"))
  (let* ((session claude-inline-edit--current-session)
         (edit (plist-get session :current-edit)))
    (unless edit
      (user-error "No edit pending"))
    (claude-inline-edit--send-response t)
    (claude-inline-edit--apply-edit edit)
    (claude-native--diff-cleanup session)
    (plist-put session :pending-edits (cdr (plist-get session :pending-edits)))
    (plist-put session :current-edit nil)
    (let ((remaining (length (plist-get session :pending-edits))))
      (if (> remaining 0)
          (progn
            (message "Inline edit: Applied. %d more pending..." remaining)
            (claude-inline-edit--show-next-approval session))
        (message "Inline edit: All changes applied successfully")
        (claude-inline-edit--cleanup)))))

(defun claude-inline-edit-accept-all ()
  "Accept all pending inline edits."
  (interactive)
  (unless claude-inline-edit--current-session
    (user-error "No inline edit in progress"))
  (let* ((session claude-inline-edit--current-session)
         (pending (plist-get session :pending-edits))
         (count (length pending)))
    (unless pending
      (user-error "No edits pending"))
    (claude-native--diff-cleanup session)
    (dolist (edit pending)
      (plist-put session :current-edit edit)
      (plist-put session :connection (plist-get edit :connection))
      (claude-inline-edit--send-response t)
      (claude-inline-edit--apply-edit edit))
    (plist-put session :pending-edits nil)
    (plist-put session :current-edit nil)
    (claude-inline-edit--cleanup)
    (message "Inline edit: Applied all %d edits" count)))

(defun claude-inline-edit-reject ()
  "Reject all pending edits and prompt for a new instruction.
The previous instruction is saved as context for Claude."
  (interactive)
  (unless claude-inline-edit--current-session
    (user-error "No inline edit in progress"))
  (let* ((session claude-inline-edit--current-session)
         (prev-prompt (plist-get session :prompt))
         (pending (plist-get session :pending-edits)))
    (when prev-prompt
      (plist-put session :feedback-history
                 (cons prev-prompt (plist-get session :feedback-history))))
    (claude-inline-edit--send-response nil "User rejected, will retry")
    (dolist (edit (cdr pending))
      (let ((conn (plist-get edit :connection)))
        (when (and conn (process-live-p conn))
          (process-send-string conn
            (concat (json-encode '((behavior . "deny")
                                   (message . "User rejected all"))) "\n"))
          (delete-process conn))))
    (when-let ((proc (plist-get session :process)))
      (when (process-live-p proc)
        (kill-process proc)))
    (claude-native--diff-cleanup session)
    (claude-inline-edit-approval-mode -1)
    (plist-put session :pending-edits nil)
    (plist-put session :current-edit nil)
    (plist-put session :connection nil)
    (plist-put session :process nil)
    (plist-put session :state 'input)
    (claude-inline-edit--show-prompt-posframe)
    (message "Inline edit: Rejected. Enter new instruction...")))

(defun claude-inline-edit-quit ()
  "Quit inline edit session, rejecting all pending edits."
  (interactive)
  (when claude-inline-edit--current-session
    (let* ((session claude-inline-edit--current-session)
           (pending (plist-get session :pending-edits)))
      (dolist (edit pending)
        (let ((conn (plist-get edit :connection)))
          (when (and conn (process-live-p conn))
            (process-send-string conn
              (concat (json-encode '((behavior . "deny")
                                     (message . "User cancelled"))) "\n"))
            (delete-process conn))))
      (when-let ((proc (plist-get session :process)))
        (when (process-live-p proc)
          (kill-process proc))))
    (claude-inline-edit--cleanup)
    (message "Inline edit cancelled")))

(defun claude-inline-edit--send-response (allow &optional reason)
  "Send approval response for the current edit.
ALLOW is non-nil to approve, REASON is denial message."
  (let* ((session claude-inline-edit--current-session)
         (edit (plist-get session :current-edit))
         (conn (plist-get session :connection))
         (tool-name (plist-get edit :tool-name))
         (is-write (equal tool-name "Write"))
         (response (if allow
                       (if is-write
                           (json-encode `((behavior . "allow")
                                          (updatedInput . ((file_path . ,(plist-get edit :file-path))
                                                           (content . ,(plist-get edit :new-string))))))
                         (json-encode `((behavior . "allow")
                                        (updatedInput . ((file_path . ,(plist-get edit :file-path))
                                                         (old_string . ,(plist-get edit :old-string))
                                                         (new_string . ,(plist-get edit :new-string)))))))
                     (json-encode `((behavior . "deny")
                                    (message . ,(or reason "User rejected")))))))
    (when (and conn (process-live-p conn))
      (process-send-string conn (concat response "\n"))
      (delete-process conn))))

(defun claude-inline-edit--apply-edit (edit)
  "Apply EDIT to the source buffer.
Returns t if edit was applied, nil otherwise."
  (let* ((session claude-inline-edit--current-session)
         (source-buf (plist-get session :source-buffer))
         (tool-name (plist-get edit :tool-name))
         (old-string (plist-get edit :old-string))
         (new-string (plist-get edit :new-string)))
    (with-current-buffer source-buf
      (let ((inhibit-read-only t))
        (if (and (equal tool-name "Write") (string-empty-p old-string))
            (progn
              (erase-buffer)
              (insert new-string)
              t)
          (if (claude-inline-edit--find-and-replace old-string new-string)
              t
            (claude-native--log "Inline edit: old-string not found in buffer")
            (message "Inline edit: Warning - could not find text to replace in buffer")
            nil))))))

;;; Cleanup

(defun claude-inline-edit--cleanup ()
  "Clean up the current inline edit session."
  (when claude-inline-edit--current-session
    (let ((session claude-inline-edit--current-session))
      (claude-inline-edit--stop-spinner)
      (when-let ((proc (plist-get session :process)))
        (when (process-live-p proc)
          (kill-process proc)))
      (when-let ((temp-file (plist-get session :temp-file)))
        (when (file-exists-p temp-file)
          (delete-file temp-file)))
      (posframe-hide "*claude-inline-prompt*")
      (claude-native--diff-cleanup session)
      (claude-inline-edit-approval-mode -1)
      (setq claude-inline-edit--current-session nil))))

(provide 'claude-inline-edit)
;;; claude-inline-edit.el ends here
