;;; simple-timer.el --- A simple countdown timer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Alex Recker
;; Author: Alex Recker <alex@reckerfamily.com>
;; Keywords: timer, countdown, clock
;; URL: https://www.github.com/arecker/simple-timer.el
;; Version: 0.0.0
;; Package-Requires: ((emacs "30.1"))

;;; Code:
(defgroup simple-timer nil
  "Simple settings for simple-timer."
  :group 'external)

(defcustom simple-timer-sound-file nil
  "Path to a sound file to play when the timer finishes.
If nil, the default system beep will be used."
  :type '(choice (const :tag "System Beep" nil)
                 (file :tag "Sound File"))
  :group 'simple-timer)

(defvar simple-timer/current-duration 0 "Seconds left in current timer.")
(defvar simple-timer/modeline-string "" "Display in the modeline.")
(defvar simple-timer/timer nil "Current timer object.")

(defun simple-timer (minutes seconds)
  "Start a simple timer in the modeline."
  (interactive
   (let ((minutes (read-number "Minutes: " 0))
         (seconds (read-number "Seconds: " 0)))
     (list minutes seconds)))
  (add-to-list 'global-mode-string '(:eval simple-timer/modeline-string))
  (simple-timer-cancel)
  (simple-timer/create-timer (+ seconds (* 60 minutes))))

(defun simple-timer/ding ()
  (if simple-timer-sound-file
      (let ((path (file-truename simple-timer-sound-file)))
        (play-sound-file path))
    (ding))
  (message "Timer finished!"))

(defun simple-timer/create-timer (seconds)
  (setq simple-timer/current-duration seconds)
  (simple-timer/update-modeline)
  (setq simple-timer/timer (run-at-time 1 1 #'simple-timer/tick)))

(defun simple-timer-cancel ()
  "Cancel any simple timers if they are currently running."
  (interactive)
  (unless (not simple-timer/timer)
    (cancel-timer simple-timer/timer))
  (setq simple-timer/timer nil)
  (setq simple-timer/current-duration 0)
  (simple-timer/update-modeline))

(defun simple-timer/tick ()
  (if (<= simple-timer/current-duration 0)
      (progn
        (simple-timer/ding)
        (simple-timer-cancel))
    (setq simple-timer/current-duration (1- simple-timer/current-duration)))
  (simple-timer/update-modeline))

(defun simple-timer/update-modeline ()
  (setq simple-timer/modeline-string
        (if (>= 0 simple-timer/current-duration) ""
          (format "[%03d]" simple-timer/current-duration)))
  (force-mode-line-update))

(provide 'simple-timer)
;;; simple-timer.el ends here
