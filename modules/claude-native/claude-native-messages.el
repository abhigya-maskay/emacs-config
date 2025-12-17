;;; claude-native-messages.el --- Message handling for Claude Native -*- lexical-binding: t -*-

;; Author: ave70011
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:
;; Handles parsing and dispatching of JSON messages from the Claude CLI.
;; Message types: system.init, assistant, user, tool_use, tool_result, result

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'claude-native-ui)
(require 'claude-native-scroll)
(require 'claude-native-spinner)
(require 'claude-native-collapsible)

;;; External Dependencies
;; Forward declarations for other modules
(declare-function claude-native--insert-in-history "claude-native-buffers")
(declare-function claude-native--insert-block-separator "claude-native-buffers")
(declare-function claude-native--log "claude-native")

;;; Utility Functions

(defun claude-native--hash-to-alist (obj)
  "Recursively convert hash tables to alists for JSON encoding.
If OBJ is a hash table, convert it to an alist.
If OBJ is a list, recurse into each element.
Otherwise return OBJ unchanged."
  (cond
   ((hash-table-p obj)
    (let (result)
      (maphash (lambda (k v)
                 (push (cons (if (symbolp k) k (intern k))
                             (claude-native--hash-to-alist v))
                       result))
               obj)
      (nreverse result)))
   ((and (listp obj) (not (null obj)))
    (mapcar #'claude-native--hash-to-alist obj))
   (t obj)))

(defun claude-native--format-tool-input-preview (tool-name input)
  "Format INPUT preview for TOOL-NAME, truncated if necessary."
  (let ((max-len 200))
    (cond
     ((equal tool-name "Bash")
      (let ((cmd (or (gethash "command" input) "")))
        (format "  Command: %s"
                (if (> (length cmd) max-len)
                    (concat (substring cmd 0 max-len) "...")
                  cmd))))
     ((member tool-name '("Read" "Write" "Edit"))
      (format "  File: %s" (or (gethash "file_path" input) "")))
     ((member tool-name '("Glob" "Grep"))
      (format "  Pattern: %s" (or (gethash "pattern" input) "")))
     (t
      (let ((json-str (json-encode input)))
        (format "  Input: %s"
                (if (> (length json-str) max-len)
                    (concat (substring json-str 0 max-len) "...")
                  json-str)))))))

(defun claude-native--truncate-output (output &optional max-len)
  "Truncate OUTPUT string to MAX-LEN characters (default 200).
Returns the truncated string with ellipsis, or OUTPUT if shorter."
  (let ((limit (or max-len 200)))
    (if (and output (> (length output) limit))
        (concat (substring output 0 limit) "...")
      output)))

;;; JSON Parsing

(defun claude-native--extract-json-lines (accumulated-string)
  "Extract complete JSON lines from ACCUMULATED-STRING.
Returns (COMPLETE-LINES . REMAINDER) cons cell where COMPLETE-LINES
is a list of complete JSON strings and REMAINDER is any trailing partial."
  (let* ((lines (split-string accumulated-string "\n"))
         (last-idx (1- (length lines)))
         (complete (cl-loop for i from 0 below last-idx
                            for line = (nth i lines)
                            when (not (string-empty-p line))
                            collect line))
         (remainder (nth last-idx lines)))
    (cons complete (or remainder ""))))

(defun claude-native--parse-json-line (json-string)
  "Parse JSON-STRING into a hash table.
Returns the parsed object or nil on error (with message logged)."
  (condition-case err
      (json-parse-string json-string :object-type 'hash-table)
    (json-parse-error
     (claude-native--log "JSON parse error: %s\nInput: %s"
                         (error-message-string err)
                         (substring json-string 0 (min 500 (length json-string))))
     nil)))

;;; Session Header Formatting

(defun claude-native--format-session-header (session-id model)
  "Format session header with unicode box styling.
SESSION-ID and MODEL are displayed in a boxed format."
  (let* ((session-line (concat "Session: " (or session-id "new")))
         (model-line (concat "Model: " (or model "unknown")))
         (max-len (max (length session-line) (length model-line)))
         (hline (make-string (+ max-len 2) ?─)))
    (concat "╭" hline "╮\n"
            "│ " session-line (make-string (- max-len (length session-line)) ? ) " │\n"
            "│ " model-line (make-string (- max-len (length model-line)) ? ) " │\n"
            "╰" hline "╯")))

;;; Message Handlers

(defun claude-native--handle-init (session message)
  "Handle system.init message from CLI.
SESSION is the session plist. MESSAGE is the parsed JSON hash table.
Stores session-id and sets state to idle."
  (when-let ((session-id (gethash "session_id" message)))
    (plist-put session :session-id session-id))
  (plist-put session :state 'idle)
  (claude-native--insert-in-history
   session
   (claude-native--format-session-header
    (gethash "session_id" message)
    (gethash "model" message))
   'claude-native-system-face))

(defun claude-native--handle-assistant (session message)
  "Handle assistant message from CLI.
SESSION is the session plist. MESSAGE is the parsed JSON hash table.
Renders assistant content blocks with appropriate styling.
Handles both text blocks and tool_use blocks embedded in content."
  (claude-native--stop-spinner session)
  (plist-put session :state 'thinking)
  (when-let* ((msg (gethash "message" message))
              (content (gethash "content" msg)))
    (seq-doseq (block content)
      (let ((block-type (gethash "type" block)))
        (cond
         ((equal block-type "text")
          ;; Finalize any previous tool region before text block
          (when (plist-get session :current-tool-region)
            (claude-native--finalize-tool-region session)
            (claude-native--collapse-old-tools session))
          ;; Insert separator before block (if not first), then bullet + text
          (claude-native--insert-block-separator session)
          (claude-native--insert-in-history
           session (concat "\n" claude-native-text-bullet (gethash "text" block)) nil t t))
         ((equal block-type "tool_use")
          ;; Tool use blocks are embedded in assistant message content
          (claude-native--handle-tool-use-block session block))
         ((equal block-type "thinking")
          ;; Finalize any previous tool region before thinking block
          (when (plist-get session :current-tool-region)
            (claude-native--finalize-tool-region session)
            (claude-native--collapse-old-tools session))
          ;; Insert separator before block (if not first), then bullet + thinking
          (claude-native--insert-block-separator session)
          (claude-native--insert-in-history
           session (concat "\n" claude-native-thinking-bullet "[Thinking]\n" (gethash "thinking" block))
           'claude-native-thinking-face t)))))))

(defun claude-native--handle-tool-use-block (session block)
  "Handle a tool_use content block from an assistant message.
SESSION is the session plist. BLOCK is the tool_use hash table with
name, id, and input fields."
  ;; Finalize any previous tool region and trigger auto-collapse
  (when (plist-get session :current-tool-region)
    (claude-native--finalize-tool-region session)
    (claude-native--collapse-old-tools session))
  (claude-native--insert-block-separator session)
  (plist-put session :state 'tool-running)
  (let* ((name (gethash "name" block))
         (input (gethash "input" block))
         ;; Try to get input preview from the block itself
         (input-preview (when input
                          (claude-native--format-tool-input-short name input)))
         (header (if input-preview
                     (format "\n%s[%s]%s" claude-native-tool-bullet name input-preview)
                   (format "\n%s[%s]" claude-native-tool-bullet name))))
    (plist-put session :tool-name name)
    ;; Start tracking the collapsible region
    (claude-native--start-tool-region session name input)
    (claude-native--insert-in-history session header 'claude-native-tool-face)
    (claude-native--start-tool-timer session)))

(defun claude-native--handle-tool-use (session message)
  "Handle tool_use event from CLI - informational only.
SESSION is the session plist.  MESSAGE is the parsed JSON hash table.
Shows tool name with input preview (if available) and starts timer.
Note: Top-level events; embedded blocks use handle-tool-use-block."
  ;; Finalize any previous tool region and trigger auto-collapse
  (when (plist-get session :current-tool-region)
    (claude-native--finalize-tool-region session)
    (claude-native--collapse-old-tools session))
  (claude-native--insert-block-separator session)
  (plist-put session :state 'tool-running)
  (let* ((name (gethash "name" message))
         (last-input (plist-get session :last-tool-input))
         (input (when last-input (plist-get last-input :input)))
         (input-preview (when (and last-input
                                   (equal name (plist-get last-input :name)))
                          (claude-native--format-tool-input-short
                           name input)))
         (header (if input-preview
                     (format "\n%s[%s]%s" claude-native-tool-bullet name input-preview)
                   (format "\n%s[%s]" claude-native-tool-bullet name))))
    (plist-put session :tool-name name)
    ;; Start tracking the collapsible region
    (claude-native--start-tool-region session name input)
    (claude-native--insert-in-history session header 'claude-native-tool-face)
    (claude-native--start-tool-timer session)))

(defun claude-native--process-tool-result (session)
  "Common processing after a tool result is received.
Stops the tool timer, clears tool input, and restarts thinking spinner."
  (claude-native--stop-tool-timer session)
  (plist-put session :last-tool-input nil)
  (claude-native--start-spinner session))

(defun claude-native--handle-tool-result (session message)
  "Handle tool_result event from CLI.
SESSION is the session plist. MESSAGE is the parsed JSON hash table.
Stops the tool timer, shows truncated tool output (max 200 chars),
and restarts the thinking spinner since the model continues processing."
  ;; Display truncated output
  (when-let ((display (claude-native--truncate-output (gethash "output" message))))
    (claude-native--insert-in-history session display 'claude-native-system-face))
  ;; Common tool result processing
  (claude-native--process-tool-result session))

(defun claude-native--extract-tool-result-content (block)
  "Extract displayable content from a tool_result BLOCK.
Handles both string content and array-of-text-blocks format."
  (let ((output (gethash "content" block)))
    (cond
     ((stringp output) output)
     ((and (listp output) (> (length output) 0))
      ;; Content might be an array of text blocks
      (gethash "text" (car output)))
     (t nil))))

(defun claude-native--handle-user-message (session message)
  "Handle user message from CLI containing tool_result blocks.
SESSION is the session plist. MESSAGE is the parsed JSON hash table."
  (when-let* ((msg (gethash "message" message))
              (content (gethash "content" msg)))
    (seq-doseq (block content)
      (when (equal (gethash "type" block) "tool_result")
        ;; Extract and display truncated output
        (when-let ((display (claude-native--truncate-output
                             (claude-native--extract-tool-result-content block))))
          ;; Insert newline to separate timer from output, render as markdown
          (claude-native--insert-in-history session "\n" nil t)
          (claude-native--insert-in-history session display nil t t))))
    ;; Common tool result processing (stops timer, clears input, restarts spinner)
    (claude-native--process-tool-result session)))

(defun claude-native--handle-result (session message)
  "Handle result event from CLI.
SESSION is the session plist. MESSAGE is the parsed JSON hash table.
Extracts session_id for --resume, shows status/duration, sets state to idle."
  ;; Stop spinner (safety net if no assistant message was received)
  (claude-native--stop-spinner session)
  ;; Finalize any open tool region at end of turn
  (when (plist-get session :current-tool-region)
    (claude-native--finalize-tool-region session)
    (claude-native--collapse-old-tools session))
  ;; Extract session_id for conversation continuity
  (when-let ((session-id (gethash "session_id" message)))
    (plist-put session :session-id session-id))
  ;; Mark turn as complete
  (plist-put session :state 'idle)
  (plist-put session :current-proc nil)
  ;; Display result info
  (let ((status (gethash "subtype" message))
        (duration (gethash "duration_ms" message))
        (cost (gethash "total_cost_usd" message)))
    (claude-native--insert-in-history
     session
     (format "\n[%s%s%s]"
             (or status "done")
             (if duration (format " %.1fs" (/ duration 1000.0)) "")
             (if cost (format " $%.4f" cost) ""))
     'claude-native-system-face))
  ;; Force immediate scroll at turn end to ensure result is visible
  (claude-native--scroll-immediately session))

;;; Message Dispatcher

(defun claude-native--dispatch-message (session message)
  "Dispatch MESSAGE to appropriate handler based on type.
SESSION is the session plist. MESSAGE is a parsed JSON hash table.
Note: control_request messages are no longer handled here - tool approval
is now handled via the MCP permission server."
  (let ((msg-type (gethash "type" message))
        (msg-subtype (gethash "subtype" message)))
    (cond
     ((and (equal msg-type "system") (equal msg-subtype "init"))
      (claude-native--handle-init session message))
     ((equal msg-type "assistant")
      (claude-native--handle-assistant session message))
     ((equal msg-type "user")
      ;; User messages contain tool_result blocks
      (claude-native--handle-user-message session message))
     ((equal msg-type "tool_use")
      (claude-native--handle-tool-use session message))
     ((equal msg-type "tool_result")
      (claude-native--handle-tool-result session message))
     ((equal msg-type "result")
      (claude-native--handle-result session message)))))

(provide 'claude-native-messages)
;;; claude-native-messages.el ends here
