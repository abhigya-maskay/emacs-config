;;; claude-native-ui.el --- UI definitions for Claude Native -*- lexical-binding: t -*-

;; Author: ave70011
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:
;; Provides UI definitions for the Claude Native interface including:
;; - Customization group and variables
;; - Face definitions for syntax highlighting
;; - Buffer name constants
;; - Visual element constants (bullets, separators)

;;; Code:

;;; Customization Group

(defgroup claude-native nil
  "Native Emacs UI for Claude Code."
  :group 'tools
  :prefix "claude-native-")

;;; Customization Variables

(defcustom claude-native-cli-path "claude"
  "Path to the Claude Code CLI executable."
  :type 'string
  :group 'claude-native)

(defcustom claude-native-window-side 'right
  "Side of frame where Claude windows appear."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Right" right))
  :group 'claude-native)

(defcustom claude-native-window-width 100
  "Width of the Claude window."
  :type 'integer
  :group 'claude-native)

(defcustom claude-native-input-height 8
  "Height of the input buffer (in lines)."
  :type 'integer
  :group 'claude-native)

(defcustom claude-native-focus-on-open t
  "Whether to focus the input buffer when opened."
  :type 'boolean
  :group 'claude-native)

(defcustom claude-native-use-diff-for-edit t
  "Whether to show unified diff for Edit tool approvals.
When non-nil, Edit tool approvals will display a unified diff comparison
of the original file vs proposed changes. When nil, uses the default
truncated text preview."
  :type 'boolean
  :group 'claude-native)

(defcustom claude-native-auto-collapse-tools t
  "Automatically collapse tool output after moving past it.
When non-nil, tool call output is collapsed once subsequent blocks
are inserted into the conversation."
  :type 'boolean
  :group 'claude-native)

(defcustom claude-native-collapsed-tool-max-lines 2
  "Maximum lines to show in collapsed tool output preview."
  :type 'integer
  :group 'claude-native)

(defcustom claude-native-collapse-after-blocks 1
  "Collapse tool output after this many subsequent blocks are inserted."
  :type 'integer
  :group 'claude-native)

;;; Face Definitions

(defface claude-native-user-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for user messages in Claude history."
  :group 'claude-native)

(defface claude-native-assistant-face
  '((t :inherit font-lock-function-name-face))
  "Face for assistant messages in Claude history."
  :group 'claude-native)

(defface claude-native-tool-face
  '((t :inherit font-lock-type-face :slant italic))
  "Face for tool calls in Claude history."
  :group 'claude-native)

(defface claude-native-error-face
  '((t :inherit error))
  "Face for error messages in Claude history."
  :group 'claude-native)

(defface claude-native-system-face
  '((t :inherit font-lock-comment-face))
  "Face for system messages (cost, session info)."
  :group 'claude-native)

(defface claude-native-prompt-face
  '((t :inherit minibuffer-prompt))
  "Face for the input prompt."
  :group 'claude-native)

(defface claude-native-approval-face
  '((t :inherit warning :weight bold))
  "Face for tool approval prompts."
  :group 'claude-native)

(defface claude-native-approval-key-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for keybinding hints in approval prompts."
  :group 'claude-native)

(defface claude-native-tool-input-face
  '((t :inherit font-lock-string-face :slant italic))
  "Face for tool input preview."
  :group 'claude-native)

(defface claude-native-spinner-face
  '((t :inherit font-lock-comment-face))
  "Face for the thinking spinner."
  :group 'claude-native)

(defface claude-native-thinking-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for thinking/reasoning traces."
  :group 'claude-native)

(defface claude-native-tool-timer-face
  '((t :inherit font-lock-comment-face))
  "Face for tool elapsed time display."
  :group 'claude-native)

(defface claude-native-separator-face
  '((t :inherit shadow))
  "Face for block separator lines (subtle, blends into background)."
  :group 'claude-native)

(defface claude-native-collapsed-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for collapsed tool output indicator."
  :group 'claude-native)

;;; Font-Lock Keywords for Meta Blocks

(defvar claude-native--font-lock-keywords
  '(("^[╭╮╰╯│─].*$" . 'claude-native-system-face)
    ("^You: .*$" . 'claude-native-user-face)
    ("^\\[\\(done\\|success\\|error\\|cancelled\\)[^]]*\\]$" . 'claude-native-system-face)
    ("^\\[Thinking\\]$" . 'claude-native-thinking-face)
    ("^\\[[A-Za-z_]+\\].*$" . 'claude-native-tool-face))
  "Font-lock keywords for Claude meta blocks in history buffer.")

;;; Buffer Name Constants

(defconst claude-native-history-buffer-name "*claude-history*"
  "Name of the Claude history buffer.")

(defconst claude-native-input-buffer-name "*claude-input*"
  "Name of the Claude input buffer.")

(defconst claude-native-debug-buffer-name "*claude-debug*"
  "Name of the Claude debug buffer.")

;;; Block Bullet and Separator Constants

(defconst claude-native-text-bullet "◆ "
  "Bullet marker for assistant text blocks.")

(defconst claude-native-tool-bullet "◇ "
  "Bullet marker for tool use blocks.")

(defconst claude-native-thinking-bullet "◌ "
  "Bullet marker for thinking blocks.")

(defconst claude-native-block-separator
  "·········································"
  "Dotted separator between blocks.")

(provide 'claude-native-ui)
;;; claude-native-ui.el ends here
