;;; claude-native.el --- Native Emacs UI for Claude Code -*- lexical-binding: t -*-

;; Author: ave70011
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:
;; Native Emacs interface for Claude Code CLI using streaming JSON.
;; Provides a two-buffer layout: history (read-only) and input (editable).

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'markdown-mode)
(require 'mixed-pitch)
(require 'init-utils)

;; Load submodules
(require 'claude-native-ui)
(require 'claude-native-log)
(require 'claude-native-session)
(require 'claude-native-spinner)
(require 'claude-native-collapsible)
(require 'claude-native-diff)
(require 'claude-native-approval)
(require 'claude-native-approval-ui)
(require 'claude-native-messages)
(require 'claude-native-process)
(require 'claude-native-window)
(require 'claude-native-scroll)
(require 'claude-native-modes)
(require 'claude-native-buffers)
(require 'claude-native-commands)
(require 'claude-native-input)
(require 'claude-native-public)

(provide 'claude-native)
;;; claude-native.el ends here
