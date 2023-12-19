;;; syncthing.el --- Client for Syncthing -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Peter Badida

;; Author: Peter Badida <keyweeusr@gmail.com>
;; Keywords: convenience, syncthing, sync, client, view
;; Version: 1.2.2
;; Package-Requires: ((emacs "27.1"))
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
;; The client requires Syncthing (server) obtainable from https://syncthing.net

;;; Code:

(require 'widget)
(require 'wid-edit)
(require 'subr-x)

(defgroup syncthing
  nil
  "Customization group for `syncthing'."
  :group 'external
  :group 'communication)

(defgroup syncthing-startup
  nil
  "Customization sub-group for `syncthing' start-up stage."
  :group 'syncthing)

(defgroup syncthing-faces
  nil
  "Customization group for `syncthing' faces."
  :group 'syncthing)

;; customization values
(defcustom syncthing-format-buffer
  "*syncthing(%s)*"
  "Syncthing's buffer name with a =%s= placeholder for address."
  :group 'syncthing-startup
  :type 'string)

(defcustom syncthing-default-name
  "Default Localhost"
  "Base URL for Syncthing REST API endpoint."
  :group 'syncthing
  :type 'string)

(defcustom syncthing-base-url
  "https://127.0.0.1:8384"
  "Base URL for Syncthing REST API endpoint."
  :group 'syncthing
  :type 'string)

(defcustom syncthing-format-perc
  "%6.2f%%"
  "Format for displaying process percentage."
  :group 'syncthing
  :type 'string)

(defcustom syncthing-format-rate-download
  " %s"
  "Format for displaying download rate in header line."
  :group 'syncthing
  :type 'string)

(defcustom syncthing-format-rate-upload
  " %s"
  "Format for displaying upload rate in header line."
  :group 'syncthing
  :type 'string)

(defcustom syncthing-format-count-local-files
  " %d"
  "Format for displaying local files count in header line."
  :group 'syncthing
  :type 'string)

(defcustom syncthing-format-count-local-folders
  " %d"
  "Format for displaying local folders count in header line."
  :group 'syncthing
  :type 'string)

(defcustom syncthing-format-count-local-bytes
  " ~%s"
  "Format for displaying local size in header line."
  :group 'syncthing
  :type 'string)

(defcustom syncthing-format-count-listeners
  " %s"
  "Format for displaying listeners count in header line."
  :group 'syncthing
  :type 'string)

(defcustom syncthing-format-count-discovery
  " %s"
  "Format for displaying discovery count in header line."
  :group 'syncthing
  :type 'string)

(defcustom syncthing-format-uptime
  " %s"
  "Format for displaying Syncthing server uptime in header line."
  :group 'syncthing
  :type 'string)

(defcustom syncthing-format-my-id
  " %s"
  "Format for displaying current device's ID in header line."
  :group 'syncthing
  :type 'string)

(defcustom syncthing-format-version
  " %s"
  "Format for displaying version in header line."
  :group 'syncthing
  :type 'string)

(defcustom syncthing-cleanup-priority
  0
  "`add-hook' priority."
  :group 'syncthing
  :type 'number)

(defcustom syncthing-default-server-token
  ""
  "Syncthing REST API token."
  :group 'syncthing
  :type 'string)

(defcustom syncthing-start-collapsed
  t
  "Start all items collapsed."
  :group 'syncthing-startup
  :type 'boolean)

(defcustom syncthing-start-with-auto-refresh
  t
  "Start with auto-refresh enabled."
  :group 'syncthing-startup
  :type 'boolean)

(defcustom syncthing-auto-refresh-interval
  10
  "Number of seconds to wait before refreshing client buffer."
  :group 'syncthing
  :type 'number)

;; customization faces/colors/fonts
(defface syncthing-title
  '((((class color) (background dark))
     (:inherit 'info-title-1))
    (((class color) (background light))
     (:inherit 'info-title-1))
    (t :inherit 'info-title-1))
  "Face for section titles."
  :group 'syncthing-faces)

(defface syncthing-prop
  '((((class color) (background dark))
     (:foreground "white" :height 0.75))
    (((class color) (background light))
     (:foreground "black" :height 0.75))
    (t (:height 0.75)))
  "Face for item properties."
  :group 'syncthing-faces)

(defface syncthing-bold
  '((((class color) (background dark))
     (:foreground "white" :bold t))
    (((class color) (background light))
     (:foreground "black" :bold t))
    (t (:bold t)))
  "Face for bold."
  :group 'syncthing-faces)

(defface syncthing-italic
  '((((class color) (background dark))
     (:foreground "white" :italic t))
    (((class color) (background light))
     (:foreground "black" :italic t))
    (t (:italic t)))
  "Face for italic."
  :group 'syncthing-faces)

(defface syncthing-progress-100
  '((((class color) (background dark))
     (:foreground "green"))
    (((class color) (background light))
     (:foreground "green"))
    (t (:foreground "green")))
  "Face for 100% progress."
  :group 'syncthing-faces)

(defface syncthing-progress-75
  '((((class color) (background dark))
     (:foreground "lightgreen"))
    (((class color) (background light))
     (:foreground "lightgreen"))
    (t (:foreground "lightgreen")))
  "Face for 75%-100% progress."
  :group 'syncthing-faces)

(defface syncthing-progress-50
  '((((class color) (background dark))
     (:foreground "yellow"))
    (((class color) (background light))
     (:foreground "yellow"))
    (t (:foreground "yellow")))
  "Face for 50%-75% progress."
  :group 'syncthing-faces)

(defface syncthing-progress-25
  '((((class color) (background dark))
     (:foreground "orange"))
    (((class color) (background light))
     (:foreground "orange"))
    (t (:foreground "orange")))
  "Face for 25%-50% progress."
  :group 'syncthing-faces)

(defface syncthing-progress-0
  '((((class color) (background dark))
     (:foreground "red"))
    (((class color) (background light))
     (:foreground "red"))
    (t (:foreground "red")))
  "Face for 0%-25% progress."
  :group 'syncthing-faces)

(defface syncthing-rate-download
  '((((class color) (background dark))
     (:foreground "deep sky blue"))
    (((class color) (background light))
     (:foreground "deep sky blue"))
    (t (:foreground "deep sky blue")))
  "Face for current download rate."
  :group 'syncthing-faces)

(defface syncthing-rate-upload
  '((((class color) (background dark))
     (:foreground "lightgreen"))
    (((class color) (background light))
     (:foreground "lightgreen"))
    (t (:foreground "lightgreen")))
  "Face for current upload rate."
  :group 'syncthing-faces)

(defface syncthing-count-local-files
  '((((class color) (background dark))
     (:foreground "white"))
    (((class color) (background light))
     (:foreground "white"))
    (t (:foreground "white")))
  "Face for local files counter."
  :group 'syncthing-faces)

(defface syncthing-count-local-folders
  '((((class color) (background dark))
     (:foreground "yellow"))
    (((class color) (background light))
     (:foreground "yellow"))
    (t (:foreground "yellow")))
  "Face for local folder counter."
  :group 'syncthing-faces)

(defface syncthing-count-local-bytes
  '((((class color) (background dark))
     (:foreground "light sea green"))
    (((class color) (background light))
     (:foreground "light sea green"))
    (t (:foreground "light sea green")))
  "Face for local bytes counter."
  :group 'syncthing-faces)

(defface syncthing-count-listeners
  '((((class color) (background dark))
     (:foreground "green"))
    (((class color) (background light))
     (:foreground "green"))
    (t (:foreground "green")))
  "Face for listeners' counter."
  :group 'syncthing-faces)

(defface syncthing-count-discovery
  '((((class color) (background dark))
     (:foreground "steel blue"))
    (((class color) (background light))
     (:foreground "steel blue"))
    (t (:foreground "steel blue")))
  "Face for discovery counter."
  :group 'syncthing-faces)

(defface syncthing-uptime
  '((((class color) (background dark))
     (:foreground "orchid"))
    (((class color) (background light))
     (:foreground "orchid"))
    (t (:foreground "orchid")))
  "Face for uptime counter."
  :group 'syncthing-faces)

(defface syncthing-my-id
  '((((class color) (background dark))
     (:foreground "#3498db"))
    (((class color) (background light))
     (:foreground "#3498db"))
    (t (:foreground "#3498db")))
  "Face for this device's ID."
  :group 'syncthing-faces)

;; constants
(defconst syncthing-gibibyte (expt 1024 3))
(defconst syncthing-mibibyte (expt 1024 2))
(defconst syncthing-kibibyte (expt 1024 1))
(defconst syncthing-day-seconds (* 1 60 60 24))
(defconst syncthing-hour-seconds (* 1 60 60))
(defconst syncthing-min-seconds (* 1 60))

;; local/state variables
(defvar syncthing--servers nil
  "List of currently active Syncthing servers.")

(defvar-local syncthing-server nil
  "Buffer-local instance of Syncthing server.")

(cl-defstruct (syncthing-server
               (:copier nil) (:named nil) (:constructor syncthing--server))
  "Local state holder for Syncthing buffer client."
  ;; on slot order change or on new slot basically restart Emacs because
  ;; "args-out-of-range" even though it's present and cl-defstruct is called
  ;; via e.g. eval-buffer
  name url token data connections-total last-speed-date)

(defvar-local syncthing-buffer nil
  "Buffer-local instance for all drawables and other buffer states.")

(cl-defstruct (syncthing-buffer
               (:copier nil) (:named nil) (:constructor syncthing--buffer))
  "Local state holder for Syncthing buffer drawables and state."
  name collapse-after-start fold-folders fold-devices)

;; keyboard
(defvar-local syncthing-mode-map
  (let ((map (make-keymap)))
    ;; custom handler for RET / widget input handler
    (define-key map (kbd "RET") #'syncthing--newline)
    (define-key map (kbd "SPC") #'syncthing--newline)
    (define-key map (kbd "?") #'describe-bindings)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    map))

;; private/helper funcs
(defun syncthing--ping (server)
  "Check whether we can use the API at SERVER with TOKEN."
  (let ((url-request-method "GET")
        (url-request-extra-headers
         `(("X-Api-Key" . ,(syncthing-server-token server)))))
    (ignore url-request-method url-request-extra-headers)
    (condition-case-unless-debug nil
        (with-temp-buffer
          (url-insert-file-contents
           (format "%s/rest/system/ping" (syncthing-server-url server))))
      (file-error (error "Failed to authenticate, check the token!")))))

(defun syncthing--request (method url token &rest data)
  "Send authenticated HTTP request to Syncthing REST API.
Argument METHOD HTTP method/verb.
Argument URL API to call.
Optional argument DATA Data to send.
Argument TOKEN API token."
  (let ((url-request-method method)
        (url-request-data data)
        (url-request-extra-headers `(("X-Api-Key" . ,token))))
    (ignore url-request-method method
            url-request-data data
            url-request-extra-headers)
    (condition-case-unless-debug nil
        (with-temp-buffer
          (url-insert-file-contents url)
          (json-parse-buffer :object-type 'alist
                             :array-type 'list
                             :null-object nil
                             :false-object nil))
      (file-error (error "Failed to handle response for %s" url)))))

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

(defun syncthing--url (path)
  "Assemble full API url from PATH."
  (format "%s/%s" syncthing-base-url path))

(defun syncthing--title (text)
  "Format TEXT as title."
  (propertize text 'face 'syncthing-title))

(defun syncthing--prop (text)
  "Format TEXT as property."
  (propertize text 'face 'syncthing-prop))

(defun syncthing--bold (text)
  "Format TEXT as bold."
  (propertize text 'face 'syncthing-bold))

(defun syncthing--italic (text)
  "Format TEXT as italic."
  (propertize text 'face 'syncthing-italic))

(defun syncthing--rate-download (text)
  "Format TEXT as download rate."
  (propertize text 'face 'syncthing-rate-download))

(defun syncthing--rate-upload (text)
  "Format TEXT as upload rate."
  (propertize text 'face 'syncthing-progress-75))

(defun syncthing--count-local-files (text)
  "Format TEXT as local files count."
  (propertize text 'face 'syncthing-count-local-files))

(defun syncthing--count-local-folders (text)
  "Format TEXT as local folders count."
  (propertize text 'face 'syncthing-count-local-folders))

(defun syncthing--count-local-bytes (text)
  "Format TEXT as local bytes count."
  (propertize text 'face 'syncthing-count-local-bytes))

(defun syncthing--count-listeners (text)
  "Format TEXT as listeners count."
  (propertize text 'face 'syncthing-count-listeners))

(defun syncthing--count-discovery (text)
  "Format TEXT as discovery count."
  (propertize text 'face 'syncthing-count-discovery))

(defun syncthing--uptime (text)
  "Format TEXT as uptime."
  (propertize text 'face 'syncthing-uptime))

(defun syncthing--my-id (text)
  "Format TEXT as Syncthing ID."
  (propertize text 'face 'syncthing-my-id))

(defun syncthing--draw-folders-header (&optional &key before after)
  "Draw folder header with optional BEFORE and AFTER separator."
  (save-window-excursion
    (switch-to-buffer
     (get-buffer-create (syncthing-buffer-name syncthing-buffer)))
    (when before
      (widget-insert (syncthing--title "\n")))
    (widget-insert (syncthing--title " Folders\n"))
    (when after
      (widget-insert (syncthing--title "\n")))))

(defun syncthing--draw-devices-header (&optional &key before after)
  "Draw device header with optional BEFORE and AFTER separator."
  (save-window-excursion
    (switch-to-buffer
     (get-buffer-create (syncthing-buffer-name syncthing-buffer)))
    (when before
      (widget-insert (syncthing--title "\n")))
    (widget-insert (syncthing--title " Devices\n"))
    (when after
      (widget-insert (syncthing--title "\n")))))

(defun syncthing--draw-folders (server)
  "Draw folder widget in buffer from `syncthing-server'."
  (let-alist (syncthing-server-data server)
    (syncthing--draw-folders-header)
    (cond ((>= .version 37)
           (mapc #'syncthing--list-37-folder
                 (sort .folders #'syncthing--sort-folders))))))

(defun syncthing--draw-devices (server)
  "Draw device widget in buffer from `syncthing-server'."
  (let-alist (syncthing-server-data server)
    (syncthing--draw-devices-header :before t)
    (cond ((>= .version 37)
           (mapc #'syncthing--list-37-device
                 (sort .devices #'syncthing--sort-devices))))))

(defun syncthing--flat-string-sort (key left right)
  "Generic value sort func for flat Syncthing data.

[{\"key\": value9}, {\"key\": value5}] -> [value5, value9]"
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
  "Sort folders by `label' value."
  (syncthing--flat-string-sort "label" left right))

(defun syncthing--sort-devices (left right)
  "Sort devices by `name' value."
  (syncthing--flat-string-sort "name" left right))

(defun syncthing--progress (device-id folder-id)
  "Get progress for DEVICE-ID and FOLDER-ID."
  (let-alist (syncthing-request
              syncthing-server "GET"
              (format "rest/db/completion?device=%s&folder=%s"
                      device-id folder-id))
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
             (when (syncthing-buffer-collapse-after-start syncthing-buffer)
               (push id (syncthing-buffer-fold-folders syncthing-buffer))))
            ((string-equal "type" (car item))
             (setq type (cdr item)))
            ((string-equal "path" (car item))
             (setq path (cdr item)))
            ((string-equal "devices" (car item))
             (setq devices (cdr item)))))
    (dolist (item (syncthing-request
                   syncthing-server "GET"
                   (format "rest/db/completion?folder=%s" id)))
      (cond ((string-equal "completion" (car item))
             (setq perc (cdr item)))))
    (save-window-excursion
      (switch-to-buffer
       (get-buffer-create (syncthing-buffer-name syncthing-buffer)))
      (widget-create
       'push-button
       :button-face "menu"
       :button-suffix
       (concat
        " "
        (syncthing--color-perc perc)
        (syncthing--bold (format " %s\n" name))
        (unless (member id (syncthing-buffer-fold-folders syncthing-buffer))
          (syncthing--prop (format "\t%s\n\t%s\n\t%s\n\t%s\n"
                                   id type path devices))))
       :action
       (lambda (&rest _event)
         (if (member id (syncthing-buffer-fold-folders syncthing-buffer))
             (progn
               (setf (syncthing-buffer-fold-folders syncthing-buffer)
                     (delete id
                             (syncthing-buffer-fold-folders syncthing-buffer)))
               (save-excursion
                 (widget-delete (syncthing--get-widget (point)))
                 (syncthing--list-37-folder folder)))
           (progn
             (if (syncthing-buffer-fold-folders syncthing-buffer)
                 (push id (syncthing-buffer-fold-folders syncthing-buffer))
               (setf (syncthing-buffer-fold-folders syncthing-buffer)
                     (list id)))
             (save-excursion
               (widget-delete (syncthing--get-widget (point)))
               (syncthing--list-37-folder folder)))))
       (if (member id (syncthing-buffer-fold-folders syncthing-buffer))
           (syncthing--bold ">")
         (syncthing--bold "v"))))))

(defun syncthing--color-perc (perc)
  "Colorize PERC float."
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
             (when (syncthing-buffer-collapse-after-start syncthing-buffer)
               (push id (syncthing-buffer-fold-devices syncthing-buffer))))
            ((string-equal "paused" (car item))
             (setq paused (if (eq (cdr item) :false) "active" "paused")))
            ((string-equal "addresses" (car item))
             (setq addresses (format "%s" (cdr item))))))
    (dolist (item (syncthing-request
                   syncthing-server "GET"
                   (format "rest/db/completion?device=%s" id)))
      (cond ((string-equal "completion" (car item))
             (setq perc (cdr item)))))
    (save-window-excursion
      (switch-to-buffer
       (get-buffer-create (syncthing-buffer-name syncthing-buffer)))
      (widget-create
       'push-button
       :button-face "menu"
       :button-suffix
       (concat
        " "
        (syncthing--color-perc perc)
        (syncthing--bold (format " %s\n" name))
        (unless (member id (syncthing-buffer-fold-devices syncthing-buffer))
          (syncthing--prop (format "\t%s\n\t%s\n\t%s\n"
                                   id paused addresses))))
       :action
       (lambda (&rest _event)
         (if (member id (syncthing-buffer-fold-devices syncthing-buffer))
             (progn
               (setf (syncthing-buffer-fold-devices syncthing-buffer)
                     (delete id
                             (syncthing-buffer-fold-devices syncthing-buffer)))
               (save-excursion
                 (widget-delete (syncthing--get-widget (point)))
                 (syncthing--list-37-device device)))
           (progn
             (if (syncthing-buffer-fold-devices syncthing-buffer)
                 (push id (syncthing-buffer-fold-devices syncthing-buffer))
               (setf (syncthing-buffer-fold-devices syncthing-buffer)
                     (list id)))
             (save-excursion
               (widget-delete (syncthing--get-widget (point)))
               (syncthing--list-37-device device)))))
       (if (member id (syncthing-buffer-fold-devices syncthing-buffer))
           (syncthing--bold ">")
         (syncthing--bold "v"))))))

(defun syncthing--header-line (server)
  "Return SERVER `header-line-format' string."
  (let* ((data (syncthing-server-data server))
         (uptime
          (alist-get 'uptime (alist-get 'system-status data))))
    (string-join
     (list
      (syncthing--rate-download
       (format syncthing-format-rate-download
               (syncthing--bytes-to-rate
                (or (alist-get 'rate-download data) -1))))
      (syncthing--rate-upload
       (format syncthing-format-rate-upload
               (syncthing--bytes-to-rate
                (or (alist-get 'rate-upload data) -1))))
      (syncthing--count-local-files
       (format
        syncthing-format-count-local-files
        (cl-reduce
         #'+ (alist-get 'folders data)
         :key (lambda (folder)
                (alist-get 'localFiles (alist-get 'status folder) 0)))))
      (syncthing--count-local-folders
       (format
        syncthing-format-count-local-folders
        (cl-reduce
         #'+ (alist-get 'folders data)
         :key (lambda (folder)
                (alist-get 'localDirectories (alist-get 'status folder) 0)))))
      (syncthing--count-local-bytes
       (format
        syncthing-format-count-local-bytes
        (syncthing--scale-bytes
         (cl-reduce
          #'+ (alist-get 'folders data)
          :key (lambda (folder)
                 (alist-get 'localBytes (alist-get 'status folder) 0)))
         1)))
      (syncthing--count-listeners
       (format syncthing-format-count-listeners "3/3"))
      (syncthing--count-discovery
       (format syncthing-format-count-discovery "4/5"))
      (syncthing--uptime
       (format syncthing-format-uptime (syncthing--sec-to-uptime uptime)))
      (format syncthing-format-my-id
              (substring
               (alist-get 'myID (alist-get 'system-status data) "n/a") 0 6))
      (format syncthing-format-version
              (alist-get 'system-version data "n/a"))) " ")))

(defun syncthing--sec-to-uptime (sec)
  "Convert SEC number to DDd HHh MMm SSs uptime string."
  (let* ((days  (/ sec syncthing-day-seconds))
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
    (when (< 0 days)
      (setq out (if (eq 0 (length out))
                    (format "%dd" days)
                  (format "%s %dd" out days))))
    (when (< 0 hours)
      (setq out (if (eq 0 (length out))
                    (format "%dh" hours)
                  (format "%s %dh" out hours))))
    (when (< 0 minutes)
      (setq out (if (eq 0 (length out))
                    (format "%dm" minutes)
                  (format "%s %dm" out minutes))))
    (when (< 0 seconds)
      (setq out (if (eq 0 (length out))
                    (format "%ds" seconds)
                  (format "%s %ds" out seconds))))
    out))

(defun syncthing--maybe-float (num places)
  "Convert NUM to float if decimal PLACES are > 0."
  (if (> places 0) (float num) num))

(defun syncthing--scale-bytes (bytes places)
  "Convert BYTES to highest reached 1024 exponent with decimal PLACES."
  (let* ((gigs  (/ bytes (syncthing--maybe-float
                          syncthing-gibibyte places)))
         (megs (/ bytes (syncthing--maybe-float
                         syncthing-mibibyte places)))
         (kilos (/ bytes (syncthing--maybe-float
                          syncthing-kibibyte places)))
         (out ""))
    (when (and (eq 0 (length out)) (< 0 gigs))
      (setq out (format (format "%%.%dfGiB" places) gigs)))
    (when (and (eq 0 (length out)) (< 0 megs))
      (setq out (format (format "%%.%dfMiB" places) megs)))
    (when (and (eq 0 (length out)) (< 0 kilos))
      (setq out (format (format "%%.%dfKiB" places) kilos)))
    (when (eq 0 (length out))
      (setq out (format (format "%%.%dfB" places) bytes)))
    out))

(defun syncthing--bytes-to-rate (bytes)
  "Format BYTES to speed rate string."
  (format "%s/s" (syncthing--scale-bytes bytes 0)))

(defun syncthing--draw-buffer (server)
  (save-window-excursion
    (switch-to-buffer
     (get-buffer-create (syncthing-buffer-name syncthing-buffer)))
    (widget-setup)
    (setq header-line-format (syncthing--header-line server))
    ;; messes up with cursor position, reset to 0,0
    (goto-char 0)))

(defun syncthing--draw (server)
  "Setup buffer and draw widgets."
  (syncthing--draw-folders server)
  (syncthing--draw-devices server)
  (syncthing--draw-buffer server))

(defun syncthing--init-state ()
  "Reset all variables holding initial state.
Optional argument SKIP-CANCEL Skip removing auto-refresh."
  ;; everything += or appendable has to reset in each update
  (setf (syncthing-buffer-collapse-after-start syncthing-buffer)
        syncthing-start-collapsed
        (syncthing-buffer-fold-folders syncthing-buffer) (list))
  (setf (syncthing-buffer-fold-devices syncthing-buffer) (list)))

(defun syncthing--update (&rest _)
  "Update function for every refresh iteration."
  (save-window-excursion
    (switch-to-buffer
     (get-buffer-create (syncthing-buffer-name syncthing-buffer)))
    (let ((inhibit-read-only t))
      (erase-buffer))
    (syncthing--ping syncthing-server)
    (syncthing--server-update syncthing-server)
    (syncthing--init-state)
    (syncthing--draw syncthing-server)
    (setf (syncthing-buffer-collapse-after-start syncthing-buffer) nil)))

(defun syncthing--interactive-common (name url token)
  "Shared behavior for `syncthing' and `syncthing-with-base'.
Argument NAME Display name for Syncthing buffer.
Argument URL API server URL.
Argument TOKEN API server token."
  (unless token
    (user-error "Syncthing REST API token not configured"))
  (let ((buff (syncthing--buffer
              :name (generate-new-buffer (format syncthing-format-buffer name))
              :collapse-after-start syncthing-start-collapsed))
        (server (car (or syncthing--servers
                         (push (syncthing--server
                                :name name :url url :token token)
                               syncthing--servers)))))
    (with-current-buffer (get-buffer-create (syncthing-buffer-name buff))
      (setq-local syncthing-buffer buff)
      (put 'syncthing-buffer 'permanent-local t)
      (setq-local syncthing-server server)
      (put 'syncthing-server 'permanent-local t)
      (unless (derived-mode-p 'syncthing-mode)
        (syncthing-mode))
      (pop-to-buffer (current-buffer)
                     '((display-buffer-reuse-window
                        display-buffer-same-window))))))

(defun syncthing--server-update (server)
  "Update SERVER data."
  ;; TODO: handle version change: >= current + branches for each <
  ;;       via rest/config's '{"version": 37}' key
  (cl-loop with data = (syncthing-request server "GET" "rest/config")
           initially do
           (setf (alist-get 'system-version data)
                 (alist-get
                  'version (syncthing-request
                            server "GET" "rest/system/version"))
                 (alist-get 'system-status data)
                 (syncthing-request server "GET" "rest/system/status"))

           with conns = (syncthing-request
                         server "GET" "rest/system/connections")
           initially do
           (let* ((last-speed-date
                   (or (syncthing-server-last-speed-date server) 0))
                  (now (time-convert nil 'integer))
                  (now-total (alist-get 'total conns))
                  (conns-total (syncthing-server-connections-total server))
                  (td (- now last-speed-date)))
             (setf (syncthing-server-last-speed-date server)
                   now
                   (alist-get 'rate-download data)
                   (max 0 (/ (- (alist-get 'inBytesTotal now-total)
                                (or (alist-get 'inBytesTotal conns-total) 0))
                             td))
                   (alist-get 'rate-upload data)
                   (max 0 (/ (- (alist-get 'outBytesTotal now-total)
                                (or (alist-get 'outBytesTotal conns-total) 0))
                             td))
                   (syncthing-server-connections-total server)
                   now-total))

           with folders = (alist-get 'folders data)
           for idx below (length folders)
           for folder = (nth idx folders)
           for folder-id = (alist-get 'id folder)
           do (setf (alist-get 'completion folder)
                    (syncthing-request
                     server "GET" (format "rest/db/completion?folder=%s"
                                          folder-id))
                    (alist-get 'status folder)
                    (syncthing-request
                     server "GET" (format "rest/db/status?folder=%s"
                                          folder-id))
                    (nth idx folders) folder)

           finally return (setf (syncthing-server-data server) data)))

;; public funcs
(defun syncthing-request (server method endpoint &rest data)
  "Return SERVER response for METHOD at ENDPOINT for request with DATA."
  (apply #'syncthing--request
         (append (list method
                       (format "%s/%s" (syncthing-server-url server) endpoint)
                       (syncthing-server-token server))
                 data)))

;; modes for client's session buffer(s)
(define-derived-mode syncthing-mode special-mode "Syncthing"
  "Major mode for Syncthing client.

Activating this mode will launch Syncthing client in the current window.

\\{syncthing-mode-map}"
  :group 'syncthing
  (use-local-map syncthing-mode-map)

  ;; Hook to auto-revert mode for refreshing
  (setq-local revert-buffer-function #'syncthing--update)

  (when syncthing-start-with-auto-refresh
    (syncthing-auto-refresh-mode 1)))

(define-minor-mode syncthing-auto-refresh-mode
  "Refresh client view every `syncthing-auto-refresh-interval' seconds."
  :lighter " Auto-refresh"
  (unless (derived-mode-p 'syncthing-mode)
    (user-error "Buffer not in `syncthing-mode'"))
  (setq-local
   buffer-stale-function
   (when syncthing-auto-refresh-mode (lambda (&rest _) t))
   auto-revert-interval
   (when syncthing-auto-refresh-mode syncthing-auto-refresh-interval))
  (auto-revert-mode (if syncthing-auto-refresh-mode 1 -1)))

(defun syncthing-with-base (name base-url token)
  "Launch Syncthing instance NAME for BASE-URL and TOKEN in a new buffer."
  (interactive
   (string-join
    (list
     "sName: "
     "sSyncthing REST API base URL: "
     "sSynchting REST API token: ") "\n"))
  (syncthing--interactive-common name base-url token))

(defun syncthing ()
  "Launch Syncthing client's instance in a new buffer."
  (interactive)
  ;; switch first, assign later, buffer-local variable gets cleared otherwise
  (syncthing--interactive-common
   syncthing-default-name
   syncthing-base-url
   (if (and syncthing-default-server-token
            (not (string= "" syncthing-default-server-token)))
       syncthing-default-server-token
     (customize-variable 'syncthing-default-server-token)
     syncthing-default-server-token)))

(provide 'syncthing)
;; TODO keep drawing in --draw, move update/fetch to --update
;;; syncthing.el ends here
