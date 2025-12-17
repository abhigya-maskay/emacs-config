;;; claude-native-scroll.el --- Scroll management for Claude Native -*- lexical-binding: t -*-

;; Author: ave70011
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:
;; Provides scroll management for the Claude Native UI.
;; Handles auto-scrolling the history buffer to keep the latest
;; content visible, with deferred scrolling to coalesce rapid updates.

;;; Code:

;;; Scroll Configuration

(defvar claude-native--scroll-delay 0.02
  "Delay in seconds before executing deferred scroll.
Small delay allows multiple rapid insertions to coalesce into one scroll.")

;;; Scroll Functions

(defun claude-native--scroll-history-to-bottom (session)
  "Scroll SESSION's history window to show the absolute bottom.
Uses a robust approach that handles wrapped lines correctly."
  (when-let* ((history-buf (plist-get session :history-buf))
              (win (get-buffer-window history-buf t)))
    (when (and (buffer-live-p history-buf) (window-live-p win))
      (with-selected-window win
        (goto-char (point-max))
        ;; First move point to end, then scroll to ensure it's visible
        (set-window-point win (point-max))
        ;; Use scroll-up with error handling to reach absolute bottom
        ;; This handles wrapped lines better than recenter
        (condition-case nil
            (scroll-up (window-body-height win))
          (end-of-buffer nil))
        ;; Final adjustment: ensure point-max is visible
        (goto-char (point-max))
        (recenter -1)))))

(defun claude-native--execute-pending-scroll (session)
  "Execute pending scroll for SESSION if still needed.
Called by timer after scroll delay has elapsed."
  (plist-put session :scroll-timer nil)
  (when (plist-get session :scroll-pending)
    (plist-put session :scroll-pending nil)
    (claude-native--scroll-history-to-bottom session)))

(defun claude-native--schedule-scroll (session)
  "Schedule a deferred scroll to bottom for SESSION.
Cancels any existing pending scroll and schedules a new one.
Multiple rapid calls coalesce into a single scroll execution."
  (plist-put session :scroll-pending t)
  ;; Cancel existing timer if any
  (when-let ((old-timer (plist-get session :scroll-timer)))
    (cancel-timer old-timer))
  ;; Schedule new deferred scroll
  (plist-put session :scroll-timer
             (run-at-time claude-native--scroll-delay nil
                          #'claude-native--execute-pending-scroll session)))

(defun claude-native--scroll-immediately (session)
  "Scroll SESSION's history to bottom immediately, bypassing timer.
Use this at turn boundaries to ensure content is visible."
  ;; Cancel any pending scroll since we're doing it now
  (when-let ((timer (plist-get session :scroll-timer)))
    (cancel-timer timer)
    (plist-put session :scroll-timer nil))
  (plist-put session :scroll-pending nil)
  (claude-native--scroll-history-to-bottom session))

(provide 'claude-native-scroll)
;;; claude-native-scroll.el ends here
