;;; syncthing-common.el --- Client for Syncthing -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'syncthing-custom)
(require 'syncthing-state)

(defsubst syncthing-trace ()
  "Simple tracing inline func to dump caller and its args into a buffer."
  (when syncthing-debug
    (with-current-buffer
        (get-buffer-create (format syncthing-trace-format-buffer
                                   (syncthing-server-name syncthing-server)))
      (insert (format "%S\n" (syncthing--previous-func))))))

(defun syncthing--previous-func (&optional name)
  "Retrieve previous function from `backtrace-frame'.
Optional argument NAME Caller's name if called by other than `syncthing-trace'."
  (let* ((idx 0) current)
    (setq current (backtrace-frame idx))
    ;; Trace from the current frame, find *this* func and get previous one
    (while (and (not (string= "syncthing--previous-func"
                              (format "%s" (car (cdr current)))))
                (< idx 30)) ; shouldn't get larger or inf
      (setq idx (1+ idx))
      (setq current (backtrace-frame idx)))
    (while (and (not (string= (or name "syncthing-trace")
                              (format "%s" (car (cdr current)))))
                (< idx 30)) ; shouldn't get larger or inf
      (setq idx (1+ idx))
      (setq current (backtrace-frame idx)))
    (cdr (backtrace-frame idx))))

(defun syncthing--get-widget (pos)
  "Try to find an Emacs Widget at POS."
  (syncthing-trace)
  (let ((button (get-char-property pos 'button)))
    (or button
        (setq button (get-char-property (line-beginning-position) 'button)))
    button))

(defun syncthing--flat-string-sort (key left right)
  "Generic value sort func for flat Syncthing data.

[{\"key\": value9}, {\"key\": value5}]
                 |
                 v
[{\"key\": value5}, {\"key\": value9}]

Argument KEY to sort with.
Argument LEFT first object to compare.
Argument RIGHT second object to compare."
  (syncthing-trace)
  (let ((lname "")
        (rname ""))
    (dolist (litem left)
      (when (string-equal key (car litem))
        (setq lname (cdr litem))))
    (dolist (ritem right)
      (when (string-equal key (car ritem))
        (setq rname (cdr ritem))))
    (string< lname rname)))

(defun syncthing--sort-folders (left right)
  "Sort folders by `label' value.
Argument LEFT first object to compare.
Argument RIGHT second object to compare."
  (syncthing-trace)
  (syncthing--flat-string-sort "label" left right))

(defun syncthing--sort-devices (left right)
  "Sort devices by `name' value.
Argument LEFT first object to compare.
Argument RIGHT second object to compare."
  (syncthing-trace)
  (syncthing--flat-string-sort "name" left right))

(defun syncthing--color-perc (perc)
  "Colorize PERC float."
  (syncthing-trace)
  (propertize
   (format syncthing-format-perc perc)
   'face
   (cond ((< perc 25)
          'syncthing-progress-0)
         ((and (>= perc 25) (< perc 50))
          'syncthing-progress-25)
         ((and (>= perc 50) (< perc 75))
          'syncthing-progress-50)
         ((and (>= perc 75) (< perc 100))
          'syncthing-progress-75)
         ((>= perc 100)
          'syncthing-progress-100))))

(cl-defun syncthing--sec-to-uptime (sec &key (full nil) (pad nil))
  "Convert SEC number to DDd HHh MMm SSs uptime string.
Optional argument FULL Show all available uptime parts.
Optional argument PAD Pad parts to their max expected digit length."
  (syncthing-trace)
  (let* ((dig-format (if pad "%02d" "%d"))
         (days  (/ sec syncthing-day-seconds))
         (hours (/ (- sec
                      (* days syncthing-day-seconds))
                   syncthing-hour-seconds))
         (minutes (/ (- sec
                        (* days syncthing-day-seconds)
                        (* hours syncthing-hour-seconds))
                     syncthing-min-seconds))
         (seconds (- sec
                     (* days syncthing-day-seconds)
                     (* hours syncthing-hour-seconds)
                     (* minutes syncthing-min-seconds)))
         (out ""))
    (when (or (and (= 0 days) full) (< 0 days))
      (setq out (if (eq 0 (length out))
                    (format (format "%sd" (replace-regexp-in-string
                                           "2" "3" dig-format)) days)
                  (format (format "%%s %sd" dig-format) out days))))
    (when (or (and (= 0 hours) full) (< 0 hours))
      (setq out (if (eq 0 (length out))
                    (format (format "%sh" dig-format) hours)
                  (format (format "%%s %sh" dig-format) out hours))))
    (when (or (and (= 0 minutes) full) (< 0 minutes))
      (setq out (if (eq 0 (length out))
                    (format (format "%sm" dig-format) minutes)
                  (format (format "%%s %sm" dig-format) out minutes))))
    (when (or (and (= 0 seconds) full) (< 0 seconds))
      (setq out (if (eq 0 (length out))
                    (format (format "%ss" dig-format) seconds)
                  (format (format "%%s %ss" dig-format) out seconds))))
    out))

(defun syncthing--maybe-float (num places)
  "Convert NUM to float if decimal PLACES are > 0."
  (syncthing-trace)
  (if (> places 0) (float num) num))

(defun syncthing--scale-bytes (bytes places)
  "Convert BYTES to highest reached 1024 exponent with decimal PLACES."
  (syncthing-trace)
  (let* ((gigs  (/ bytes (syncthing--maybe-float
                          syncthing-gibibyte places)))
         (megs (/ bytes (syncthing--maybe-float
                         syncthing-mibibyte places)))
         (kilos (/ bytes (syncthing--maybe-float
                          syncthing-kibibyte places)))
         (out ""))
    (when (and (eq 0 (length out)) (< 0 (floor gigs)))
      (setq out (format (format "%%.%dfGiB" places) gigs)))
    (when (and (eq 0 (length out)) (< 0 (floor megs)))
      (setq out (format (format "%%.%dfMiB" places) megs)))
    (when (and (eq 0 (length out)) (< 0 (floor kilos)))
      (setq out (format (format "%%.%dfKiB" places) kilos)))
    (when (eq 0 (length out))
      (setq out (format (format "%%.%dfB" places) bytes)))
    out))

(defun syncthing--bytes-to-rate (bytes)
  "Format BYTES to speed rate string."
  (syncthing-trace)
  (format "%s/s" (syncthing--scale-bytes bytes 0)))

(cl-defun syncthing--num-group (num &key (dec-sep ".") (ths-sep " "))
  "Group NUM's digits with decimal and thousands separators.
Optional argument DEC-SEP custom decimal separator or default of `.'.
Optional argument THS-SEP custom thousands separator or default of ` '."
  (if (not num) ""
    (let* ((stringified (format "%s" num))
           (integer-part
            (string-to-list
             (car (split-string
                   stringified (regexp-quote (or dec-sep "."))))))
           (fraction-part
            (string-to-list
             (cadr (split-string
                    stringified (regexp-quote (or dec-sep "."))))))
           (idx 0) out)
      (when fraction-part
        (dolist (char fraction-part)
          (when (and (not (eq 0 idx)) (eq 0 (% idx 3)))
            (push (or ths-sep " ") out))
          (push (string char) out)
          (setq idx (1+ idx))))
      (setq idx 0)
      (setq out (reverse out))
      (when fraction-part
        (push (or dec-sep ".") out))
      (dolist (char (reverse integer-part))
        (when (and (not (eq 0 idx)) (eq 0 (% idx 3)))
          (push (or ths-sep " ") out))
        (push (string char) out)
        (setq idx (1+ idx)))
      (string-join out ""))))

(provide 'syncthing-common)
;;; syncthing-common.el ends here
