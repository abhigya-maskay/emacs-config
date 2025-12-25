;;; claude-native-modes.el --- Major modes for Claude Native -*- lexical-binding: t -*-

;; Author: ave70011
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:
;; Provides major mode definitions for the Claude Native UI:
;; - `claude-native-history-mode' for the read-only conversation history
;; - `claude-native-input-mode' for composing messages to Claude

;;; Code:

(require 'markdown-mode)
(require 'mixed-pitch)
(require 'claude-native-ui)

;;; External Dependencies
(declare-function claude-native-toggle "claude-native")
(declare-function claude-native-focus-input "claude-native")
(declare-function claude-native-toggle-tool-at-point "claude-native")
(declare-function claude-native-send-input "claude-native")

;;; History Mode

(defvar claude-native-history-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'claude-native-toggle)
    (define-key map (kbd "i") #'claude-native-focus-input)
    (define-key map (kbd "TAB") #'claude-native-toggle-tool-at-point)
    map)
  "Keymap for `claude-native-history-mode'.")

(define-derived-mode claude-native-history-mode gfm-view-mode "Claude-History"
  "Major mode for Claude conversation history.

Read-only buffer displaying the conversation with Claude.
Uses GFM markdown rendering for assistant responses.
Use `q' to toggle visibility, `i' to focus input buffer,
and TAB to toggle collapse/expand of tool output at point.

\\{claude-native-history-mode-map}"
  :group 'claude-native
  (setq-local markdown-hide-markup t)
  (setq-local markdown-hide-urls t)
  (setq-local markdown-header-scaling t)
  (setq-local markdown-fontify-code-blocks-natively t)
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local line-spacing 0.1)
  (visual-line-mode 1)
  (mixed-pitch-mode 1)
  (font-lock-add-keywords nil claude-native--font-lock-keywords 'append)
  ;; Prevent recentering during auto-scroll
  (setq-local scroll-conservatively 10000)
  (setq-local scroll-margin 0)
  (setq-local scroll-step 1)
  (setq-local auto-window-vscroll nil))

;;; Input Mode

(defvar claude-native-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'claude-native-send-input)
    (define-key map (kbd "S-<return>") #'newline)
    (define-key map (kbd "C-j") #'newline)
    (define-key map (kbd "C-c C-c") #'claude-native-send-input)
    map)
  "Keymap for `claude-native-input-mode'.")

(define-derived-mode claude-native-input-mode text-mode "Claude-Input"
  "Major mode for Claude input.

Editable buffer for composing messages to Claude.
Press RET (in insert mode) or C-c C-c to send.

\\{claude-native-input-mode-map}"
  :group 'claude-native
  (setq-local truncate-lines nil)
  (setq-local word-wrap t))

(provide 'claude-native-modes)
;;; claude-native-modes.el ends here
