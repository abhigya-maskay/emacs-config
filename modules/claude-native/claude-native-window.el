;;; claude-native-window.el --- Window management for Claude Native -*- lexical-binding: t -*-

;; Author: ave70011
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:
;; Provides window management for the Claude Native UI.
;; Handles displaying and hiding the Claude side windows with
;; history buffer on top and input buffer below.

;;; Code:

(require 'claude-native-ui)

;;; Internal State

(defvar claude-native--windows-visible nil
  "Non-nil when Claude windows are currently displayed.")

;;; External Dependencies

(declare-function claude-native--get-history-buffer "claude-native-buffers")
(declare-function claude-native--get-input-buffer "claude-native-buffers")

;;; Window Functions

(defun claude-native--get-claude-windows ()
  "Return list of windows displaying Claude buffers."
  (let ((history-buf (get-buffer claude-native-history-buffer-name))
        (input-buf (get-buffer claude-native-input-buffer-name))
        windows)
    (when history-buf
      (let ((win (get-buffer-window history-buf t)))
        (when win (push win windows))))
    (when input-buf
      (let ((win (get-buffer-window input-buf t)))
        (when win (push win windows))))
    windows))

(defun claude-native--display-windows ()
  "Display Claude windows in a side window layout.
Creates a side window with history buffer on top and input buffer below."
  (let* ((history-buf (claude-native--get-history-buffer))
         (input-buf (claude-native--get-input-buffer))
         (_history-win (display-buffer-in-side-window
                       history-buf
                       `((side . ,claude-native-window-side)
                         (slot . 0)
                         (window-width . ,claude-native-window-width)
                         (dedicated . t)
                         (preserve-size . (t . nil)))))
         (input-win (display-buffer-in-side-window
                     input-buf
                     `((side . ,claude-native-window-side)
                       (slot . 1)
                       (window-height . ,claude-native-input-height)
                       (dedicated . t)
                       (preserve-size . (t . t))))))
    (when (and claude-native-focus-on-open input-win)
      (select-window input-win)
      (goto-char (point-max)))
    (setq claude-native--windows-visible t)))

(defun claude-native--hide-windows ()
  "Hide all Claude windows without killing buffers."
  (dolist (win (claude-native--get-claude-windows))
    (when (window-live-p win)
      (delete-window win)))
  (setq claude-native--windows-visible nil))

(provide 'claude-native-window)
;;; claude-native-window.el ends here
