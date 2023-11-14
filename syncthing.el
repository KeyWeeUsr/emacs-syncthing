;;; syncthing.el --- Emacs client for Syncthing -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Peter Badida

;; Author: Peter Badida <keyweeusr@gmail.com>
;; Keywords: convenience, syncthing, sync, client, view
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))
;; Homepage: https://github.com/KeyWeeUsr/emacs-syncthing

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package attempts to port the browser client functionality into Emacs.

;;; Code:

(require 'widget)
(require 'wid-edit)

(defconst syncthing-buffer
  "*syncthing*"
  "Syncthing output destination.")

(defconst syncthing-token
  nil
  "Syncthing REST API token.")

(defconst syncthing-start-collapsed
  t
  "Start all items collapsed.")

(defconst syncthing-format-perc
  "%6.2f%%"
  "Format for displaying process percentage.")

(defun syncthing--request (method url &rest data)
  "Send authenticated HTTP request to Syncthing REST API.
Argument METHOD HTTP method/verb.
Argument URL API to call.
Optional argument DATA Data to send."
  (when (not syncthing-token)
    (setq syncthing-token (read-string "Synchting REST API token: ")))

  (let ((url-request-method method)
        (url-request-data data)
        (url-request-extra-headers
         `(("X-Api-Key" . ,syncthing-token))))
    (ignore url-request-method method
            url-request-data data
            url-request-extra-headers)
    (with-temp-buffer
      (url-insert-file-contents url)
      (json-parse-buffer :object-type 'alist))))

(defun syncthing--setup-buffer ()
  "Create a dedicated buffer for Syncthing client."
  (save-window-excursion
    (switch-to-buffer (get-buffer-create syncthing-buffer))
    (syncthing--clean-buffer)
    (local-set-key (kbd "RET") #'syncthing--newline)))

(defun syncthing--get-widget (pos)
  "Try to find an Emacs Widget at POS."
  (let ((button (get-char-property pos 'button)))
    (or button
        (setq button (get-char-property (line-beginning-position) 'button)))
    button))

(defun syncthing--newline (pos &optional event)
  "RET/Enter/newline-keypress handler.
Argument POS Incoming EVENT position."
  (interactive "@d")
  (let ((button (syncthing--get-widget pos)))
    (if button
	    (widget-apply-action button event)
      (error "You can't edit this part of the Syncthing buffer"))))

(defun syncthing--clean-buffer ()
  "Reset buffer to its default state."
  (let ((inhibit-read-only t))
    (erase-buffer))
  (kill-all-local-variables))

(defun syncthing--url (path)
  "Assemble full API url from PATH."
  (format "https://127.0.0.1:8384/%s" path))

(defun syncthing--title (text)
  "Format TEXT as title."
  (propertize text
              'face '("bold" :height 150)))

(defun syncthing--prop (text)
  "Format TEXT as property."
  (propertize text
              'face '(:height 80)))

(defun syncthing--bold (text)
  "Format TEXT as bold."
  (propertize text 'face 'bold))

(defun syncthing--italic (text)
  "Format TEXT as italic."
  (propertize text 'face 'italic))

(defun syncthing--green (text)
  "Format TEXT as green."
  (propertize text 'face '(:foreground "green")))

(defun syncthing--light-green (text)
  "Format TEXT as light-green."
  (propertize text 'face '(:foreground "lightgreen")))

(defun syncthing--yellow (text)
  "Format TEXT as yellow."
  (propertize text 'face '(:foreground "yellow")))

(defun syncthing--orange (text)
  "Format TEXT as orange."
  (propertize text 'face '(:foreground "orange")))

(defun syncthing--red (text)
  "Format TEXT as red."
  (propertize text 'face '(:foreground "red")))

(defvar syncthing--fold-folders
  (list)
  "Tmp to hold IDs of folds.")

(defvar syncthing--fold-devices
  (list)
  "Tmp to hold IDs of folds.")

(defun syncthing--list ()
  "List all resources."
  (let-alist (syncthing--request
              "GET" (syncthing--url "rest/config"))
    (cond ((eq .version 37)
           (save-window-excursion
             (switch-to-buffer (get-buffer-create syncthing-buffer))
             (widget-insert (syncthing--title "Folders\n")))
           (mapc
            #'syncthing--list-37-folder
            (sort .folders
                  (lambda (left right)
                    (let ((lname "")
                          (rname ""))
                      (dolist (litem left)
                        (when (string-equal "label" (car litem))
                          (setq lname (cdr litem))))
                      (dolist (ritem right)
                        (when (string-equal "label" (car ritem))
                          (setq rname (cdr ritem))))
                      (string< lname rname)))))
           (save-window-excursion
             (switch-to-buffer (get-buffer-create syncthing-buffer))
             (widget-insert (syncthing--title "\n"))
             (widget-insert (syncthing--title "Devices\n")))
           (mapc
            #'syncthing--list-37-device
            (sort .devices
                  (lambda (left right)
                    (let ((lname "")
                          (rname ""))
                      (dolist (litem left)
                        (when (string-equal "name" (car litem))
                          (setq lname (cdr litem))))
                      (dolist (ritem right)
                        (when (string-equal "name" (car ritem))
                          (setq rname (cdr ritem))))
                      (string< lname rname)))))))))

(defun syncthing--progress (device-id folder-id)
  "Get progress for DEVICE-ID and FOLDER-ID."
  (let-alist (syncthing--request
              "GET" (syncthing--url
                     (format "rest/db/completion?device=%s&folder=%s"
                             device-id folder-id)))
    .completion))

(defun syncthing--list-37-folder (folder)
  "Render single FOLDER item in a widget."
  (let ((name "")
        (id "")
        (type "")
        (path "")
        (devices nil)
        (perc 0))
    (dolist (item folder)
      (cond ((string-equal "label" (car item))
             (setq name (cdr item)))
            ((string-equal "id" (car item))
             (setq id (cdr item))
             (when syncthing-start-collapsed
               (push id syncthing--fold-folders)))
            ((string-equal "type" (car item))
             (setq type (cdr item)))
            ((string-equal "path" (car item))
             (setq path (cdr item)))
            ((string-equal "devices" (car item))
             (setq devices (cdr item)))))
    (dolist (item (syncthing--request
                   "GET" (syncthing--url
                          (format "rest/db/completion?folder=%s" id))))
      (cond ((string-equal "completion" (car item))
             (setq perc (cdr item)))))
    (save-window-excursion
      (switch-to-buffer (get-buffer-create syncthing-buffer))
      (widget-create
       'push-button
       :button-face "menu"
       :button-suffix
       (concat
        " "
        (syncthing--bold "(")
        (syncthing--color-perc perc)
        (syncthing--bold (format ") %s\n" name))
        (when (not (member id syncthing--fold-folders))
          (syncthing--prop (format "\t%s\n\t%s\n\t%s\n\t%s\n"
                                   id type path devices))))
       :action
       (lambda (&rest _event)
         (if (member id syncthing--fold-folders)
             (progn
               (setq syncthing--fold-folders
                     (delete id syncthing--fold-folders))
               (save-excursion
                 (widget-delete (syncthing--get-widget (point)))
                 (syncthing--list-37-folder folder)))
           (progn
             (if syncthing--fold-folders
                 (push id syncthing--fold-folders)
               (setq syncthing--fold-folders (list id)))
             (save-excursion
               (widget-delete (syncthing--get-widget (point)))
               (syncthing--list-37-folder folder)))))
       (if (member id syncthing--fold-folders)
           (syncthing--bold ">")
         (syncthing--bold "v"))))))

(defun syncthing--color-perc (perc)
  "Colorize PERC float."
  (cond ((< perc 25)
         (syncthing--red (format syncthing-format-perc perc)))
        ((and (>= perc 25) (< perc 50))
         (syncthing--orange (format syncthing-format-perc perc)))
        ((and (>= perc 50) (< perc 75))
         (syncthing--yellow (format syncthing-format-perc perc)))
        ((and (>= perc 75) (< perc 100))
         (syncthing--light-green (format syncthing-format-perc perc)))
        ((>= perc 100)
         (syncthing--green (format syncthing-format-perc perc)))))

(defun syncthing--list-37-device (device)
  "Render single DEVICE item in a widget."
  (let ((name "")
        (id "")
        (paused t)
        (addresses '())
        (perc 0))
    (dolist (item device)
      (cond ((string-equal "name" (car item))
             (setq name (format "%s" (cdr item))))
            ((string-equal "deviceID" (car item))
             (setq id (format "%s" (cdr item)))
             (when syncthing-start-collapsed
               (push id syncthing--fold-devices)))
            ((string-equal "paused" (car item))
             (setq paused (if (eq (cdr item) :false) "active" "paused")))
            ((string-equal "addresses" (car item))
             (setq addresses (format "%s" (cdr item))))))
    (dolist (item (syncthing--request
                   "GET" (syncthing--url
                          (format "rest/db/completion?device=%s" id))))
      (cond ((string-equal "completion" (car item))
             (setq perc (cdr item)))))
    (save-window-excursion
      (switch-to-buffer (get-buffer-create syncthing-buffer))
      (widget-create
       'push-button
       :button-face "menu"
       :button-suffix
       (concat
        " "
        (syncthing--bold "(")
        (syncthing--color-perc perc)
        (syncthing--bold (format ") %s\n" name))
        (when (not (member id syncthing--fold-devices))
          (syncthing--prop (format "\t%s\n\t%s\n\t%s\n"
                                   id paused addresses))))
       :action
       (lambda (&rest _event)
         (if (member id syncthing--fold-devices)
             (progn
               (setq syncthing--fold-devices
                     (delete id syncthing--fold-devices))
               (save-excursion
                 (widget-delete (syncthing--get-widget (point)))
                 (syncthing--list-37-device device)))
           (progn
             (if syncthing--fold-devices
                 (push id syncthing--fold-devices)
               (setq syncthing--fold-devices (list id)))
             (save-excursion
               (widget-delete (syncthing--get-widget (point)))
               (syncthing--list-37-device device)))))
       (if (member id syncthing--fold-devices)
           (syncthing--bold ">")
         (syncthing--bold "v"))))))

(defun syncthing--draw ()
  "Setup buffer and draw widgets."
  (syncthing--setup-buffer)
  (save-window-excursion
    (switch-to-buffer (get-buffer-create syncthing-buffer))
    (widget-setup))
  (syncthing--list))

(defun syncthing ()
  "Launch Syncthing client in the current window."
  (interactive)
  (setq syncthing--fold-folders (list))
  (setq syncthing--fold-devices (list))
  (syncthing--draw)
  (setq syncthing-start-collapsed nil)
  (switch-to-buffer syncthing-buffer))

(provide 'syncthing)
;;; syncthing.el ends here
