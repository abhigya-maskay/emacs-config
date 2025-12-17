;;; claude-native-log.el --- Debug logging for Claude Native -*- lexical-binding: t -*-

;;; Commentary:
;; Provides debug logging functionality for Claude Native.

;;; Code:

(require 'claude-native-ui)

;;; Debug Logging

(defun claude-native--log (format-string &rest args)
  "Log a debug message to the Claude debug buffer.
FORMAT-STRING and ARGS are passed to `format'."
  (let ((buf (get-buffer-create claude-native-debug-buffer-name))
        (msg (apply #'format format-string args))
        (timestamp (format-time-string "%H:%M:%S")))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (format "[%s] %s\n" timestamp msg)))))

(provide 'claude-native-log)
;;; claude-native-log.el ends here
