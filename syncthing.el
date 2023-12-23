;;; syncthing.el --- Client for Syncthing -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Peter Badida

;; Author: Peter Badida <keyweeusr@gmail.com>
;; Keywords: convenience, syncthing, sync, client, view
;; Version: 1.5.1
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
(require 'org-table)
(require 'autorevert)

(defgroup syncthing
  nil
  "Customization group for `syncthing'."
  :group 'external
  :group 'communication)

(defgroup syncthing-startup
  nil
  "Customization sub-group for `syncthing' start-up stage."
  :group 'syncthing)

(defgroup syncthing-debug
  nil
  "Customization sub-group for `syncthing' debugging."
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

(defcustom syncthing-trace-format-buffer
  "*syncthing trace(%s)*"
  "Syncthing's buffer name with a =%s= placeholder for address."
  :group 'syncthing-debug
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
  " %s"
  "Format for displaying local files count in header line."
  :group 'syncthing
  :type 'string)

(defcustom syncthing-format-count-local-folders
  " %s"
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

(defcustom syncthing-header-items
  '("rate-download" "rate-upload" "count-local-files" "count-local-folders"
    "count-local-bytes" "count-listeners" "count-discovery" "uptime" "my-id"
    "version")
  "Items to render at `header-line-format'.

Special meaning for empty list / nil to skip rendering the header line."
  :group 'syncthing
  :type '(repeat
          (choice :tag "Item"
                  (const :tag "Download rate" "rate-download")
                  (const :tag "Upload rate" "rate-upload")
                  (const :tag "Files" "count-local-files")
                  (const :tag "Folders" "count-local-folders")
                  (const :tag "Size" "count-local-bytes")
                  (const :tag "Listeners" "count-listeners")
                  (const :tag "Discovery" "count-discovery")
                  (const :tag "Uptime" "uptime")
                  (const :tag "ID" "my-id")
                  (const :tag "Version" "version"))))

(defcustom syncthing-display-logs
  nil
  "Display logs in `syncthing-buffer'."
  :group 'syncthing
  :type 'boolean)

(defcustom syncthing-display-changes
  nil
  "Display recent-changes in `syncthing-buffer'."
  :group 'syncthing
  :type 'boolean)

(defcustom syncthing-limit-changes
  25
  "Limit of items for recent changes."
  :group 'syncthing
  :type 'number)

(defcustom syncthing-debug
  nil
  "Enable debugging logs in special buffer."
  :group 'syncthing-debug
  :type 'boolean)

(defcustom syncthing-decimal-separator
  "."
  "Stylize number with custom decimal separator."
  :group 'syncthing
  :type 'string)

(defcustom syncthing-thousands-separator
  " "
  "Stylize number with custom thousands separator."
  :group 'syncthing
  :type 'string)

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
  (syncthing-trace)
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
  (syncthing-trace)
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
  (syncthing-trace)
  (let ((button (get-char-property pos 'button)))
    (or button
        (setq button (get-char-property (line-beginning-position) 'button)))
    button))

(defun syncthing--newline (pos &optional event)
  "RET/Enter/newline-keypress handler.
Argument POS Incoming EVENT position."
  (interactive "@d")
  (syncthing-trace)
  (let ((button (syncthing--get-widget pos)))
    (if button
	    (widget-apply-action button event)
      (error "You can't edit this part of the Syncthing buffer"))))

(defun syncthing--title (text)
  "Format TEXT as title."
  (syncthing-trace)
  (propertize text 'face 'syncthing-title))

(defun syncthing--prop (text)
  "Format TEXT as property."
  (syncthing-trace)
  (propertize text 'face 'syncthing-prop))

(defun syncthing--bold (text)
  "Format TEXT as bold."
  (syncthing-trace)
  (propertize text 'face 'syncthing-bold))

(defun syncthing--italic (text)
  "Format TEXT as italic."
  (syncthing-trace)
  (propertize text 'face 'syncthing-italic))

(defun syncthing--rate-download (text)
  "Format TEXT as download rate."
  (syncthing-trace)
  (propertize text 'face 'syncthing-rate-download))

(defun syncthing--rate-upload (text)
  "Format TEXT as upload rate."
  (syncthing-trace)
  (propertize text 'face 'syncthing-progress-75))

(defun syncthing--count-local-files (text)
  "Format TEXT as local files count."
  (syncthing-trace)
  (propertize text 'face 'syncthing-count-local-files))

(defun syncthing--count-local-folders (text)
  "Format TEXT as local folders count."
  (syncthing-trace)
  (propertize text 'face 'syncthing-count-local-folders))

(defun syncthing--count-local-bytes (text)
  "Format TEXT as local bytes count."
  (syncthing-trace)
  (propertize text 'face 'syncthing-count-local-bytes))

(defun syncthing--count-listeners (text)
  "Format TEXT as listeners count."
  (syncthing-trace)
  (propertize text 'face 'syncthing-count-listeners))

(defun syncthing--count-discovery (text)
  "Format TEXT as discovery count."
  (syncthing-trace)
  (propertize text 'face 'syncthing-count-discovery))

(defun syncthing--uptime (text)
  "Format TEXT as uptime."
  (syncthing-trace)
  (propertize text 'face 'syncthing-uptime))

(defun syncthing--my-id (text)
  "Format TEXT as Syncthing ID."
  (syncthing-trace)
  (propertize text 'face 'syncthing-my-id))

(defun syncthing--draw-folders-header (&optional &key before after)
  "Draw folder header with optional BEFORE and AFTER separator."
  (syncthing-trace)
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
  (syncthing-trace)
  (save-window-excursion
    (switch-to-buffer
     (get-buffer-create (syncthing-buffer-name syncthing-buffer)))
    (when before
      (widget-insert (syncthing--title "\n")))
    (widget-insert (syncthing--title " Devices\n"))
    (when after
      (widget-insert (syncthing--title "\n")))))

(defun syncthing--draw-logs-header (&optional &key before after)
  "Draw log header with optional BEFORE and AFTER separator."
  (syncthing-trace)
  (save-window-excursion
    (switch-to-buffer
     (get-buffer-create (syncthing-buffer-name syncthing-buffer)))
    (when before
      (widget-insert (syncthing--title "\n")))
    (widget-insert (syncthing--title "✉ Logs\n"))
    (when after
      (widget-insert (syncthing--title "\n")))))

(defun syncthing--draw-changes-header (&optional &key before after)
  "Draw log header with optional BEFORE and AFTER separator."
  (syncthing-trace)
  (save-window-excursion
    (switch-to-buffer
     (get-buffer-create (syncthing-buffer-name syncthing-buffer)))
    (when before
      (widget-insert (syncthing--title "\n")))
    (widget-insert (syncthing--title "⌛ Recent Changes\n"))
    (when after
      (widget-insert (syncthing--title "\n")))))

(defun syncthing--draw-folders (server)
  "Draw folder widget in buffer from SERVER."
  (syncthing-trace)
  (let-alist (syncthing-server-data server)
    (syncthing--draw-folders-header)
    (cond ((>= .version 37)
           (mapc #'syncthing--list-37-folder
                 (sort (copy-alist .folders) #'syncthing--sort-folders))))))

(defun syncthing--draw-devices (server)
  "Draw device widget in buffer from SERVER."
  (syncthing-trace)
  (let-alist (syncthing-server-data server)
    (syncthing--draw-devices-header :before t)
    (let (filtered)
      (dolist (dev .devices)
        (unless (string=
                 (alist-get 'deviceID dev)
                 (alist-get 'myID .system-status))
          (push dev filtered)))
      (cond ((>= .version 37)
             (mapc #'syncthing--list-37-device
                   (sort (copy-alist filtered)
                         #'syncthing--sort-devices)))))))

(defun syncthing--draw-logs (server)
  "Draw logs widget in buffer from SERVER."
  (syncthing-trace)
  (let-alist (syncthing-server-data server)
    (syncthing--draw-logs-header :before t)
    (cond ((>= .version 37)
           (syncthing--list-logs .logs)))))

(defun syncthing--draw-changes (server)
  "Draw recent-changes widget in buffer from SERVER."
  (syncthing-trace)
  (let-alist (syncthing-server-data server)
    (syncthing--draw-changes-header :before t)
    (cond ((>= .version 37)
           (syncthing--list-changes .changes)))))

(defun syncthing--list-logs (logs)
  "Render LOGS as a widget."
  (syncthing-trace)
  (save-window-excursion
    (switch-to-buffer
     (get-buffer-create (syncthing-buffer-name syncthing-buffer)))
    (let (text)
      (dolist (item (alist-get 'messages logs))
        (let-alist item
          (push (format "%s\t%s" .when .message) text)))
      (widget-insert (string-join (reverse text) "\n")))))

(defun syncthing--list-changes (change)
  "Render CHANGE as a widget."
  (syncthing-trace)
  (save-window-excursion
    (switch-to-buffer
     (get-buffer-create (syncthing-buffer-name syncthing-buffer)))
    (widget-insert
     (with-temp-buffer
       (let (text)
         (push "|Device|Action|Type|Folder|Path|Time|" text)
         (push "|-|-|-|-|-|-|" text)
         (dolist (item change)
           (let-alist (alist-get 'data item)
             (push (format "|%s|%s|%s|%s|%s|%s|"
                           .modifiedBy .action .type .label .path
                           ;; TODO: proper date parse + trim
                           (replace-regexp-in-string
                            "T" " "
                            (substring (alist-get 'time item) 0 19)))
                   text)))
         (insert (string-join (reverse text) "\n")))
       (org-mode)
       (org-table-align)
       (substring-no-properties (buffer-string))))))

(defun syncthing--flat-string-sort (key left right)
  "Generic value sort func for flat Syncthing data.

[{\"key\": value9}, {\"key\": value5}] -> [value5, value9]

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

(defun syncthing--list-37-folder (folder)
  "Render single FOLDER item in a widget."
  (syncthing-trace)
  (let* ((name (alist-get 'label folder))
        (id (alist-get 'id folder))
        (paused (alist-get 'paused folder))
        (type (alist-get 'type folder))
        (path (alist-get 'path folder))
        (devices (alist-get 'devices folder))
        (completion (alist-get 'completion folder))
        (perc (or (alist-get 'completion completion) 0))
        (order (alist-get 'order folder))
        (stats (alist-get 'stats folder))
        (text ""))
    (when (syncthing-buffer-collapse-after-start syncthing-buffer)
      (push id (syncthing-buffer-fold-folders syncthing-buffer)))

    (setq text
          (format "%s \tFolder ID\t\t\t%s\n \tFolder Path\t\t\t%s\n"
                  text id path))
    (unless paused
      (setq text
            (format "%s \tGlobal State\t\t%s\n \tLocal State\t\t\t%s\n"
                    text
                    (format "%s %s %s"
                          (format
                           syncthing-format-count-local-files
                           (syncthing--num-group
                            (alist-get 'globalFiles (alist-get 'status folder))
                            :dec-sep syncthing-decimal-separator
                            :ths-sep syncthing-thousands-separator))
                          (format
                           syncthing-format-count-local-folders
                           (syncthing--num-group
                            (alist-get 'globalDirectories
                                       (alist-get 'status folder))
                            :dec-sep syncthing-decimal-separator
                            :ths-sep syncthing-thousands-separator))
                          (format
                           syncthing-format-count-local-bytes
                           (syncthing--scale-bytes
                            (alist-get 'globalBytes
                                       (alist-get 'status folder)) 2)))
                    (format "%s %s %s"
                          (format
                           syncthing-format-count-local-files
                           (syncthing--num-group
                            (alist-get 'localFiles (alist-get 'status folder))
                            :dec-sep syncthing-decimal-separator
                            :ths-sep syncthing-thousands-separator))
                          (format
                           syncthing-format-count-local-folders
                           (syncthing--num-group
                            (alist-get 'localDirectories
                                       (alist-get 'status folder))
                            :dec-sep syncthing-decimal-separator
                            :ths-sep syncthing-thousands-separator))
                          (format
                           syncthing-format-count-local-bytes
                           (syncthing--scale-bytes
                            (alist-get 'localBytes
                                       (alist-get 'status folder)) 2))))))
    (setq text
          (format
           "%s \tFolder Type\t\t\t%s\n"
           text
           (cond ((string= type "sendreceive") "Send & Receive")
                 ((string= type "sendonly") "Send Only")
                 ((string= type "receiveonly") "Receive Only")
                 ((string= type "receiveencrypted") "Receive Encrypted"))))
    (setq text
          (format "%s \tRescans\t\t\t\t%s\n"
                  text
                  (format " %s  %s"
                          (format (syncthing--sec-to-uptime
                                   (alist-get 'rescanIntervalS folder)))
                          (if (alist-get 'fsWatcherEnabled folder)
                              "Enabled" "Disabled"))))
    (setq text
          (format "%s \tFile Pull Order\t\t%s\n"
                  text
                  (cond ((string= order "random") "Random")
                        ((string= order "alphabetic") "Alphabetic")
                        ((string= order "smallestFirst") "Smallest First")
                        ((string= order "largestFirst") "Largest First")
                        ((string= order "oldestFirst") "Oldest First")
                        ((string= order "newestFirst") "Newest First"))))
    (setq text
          (format "%s \tShared With\t\t\t%s\n"
                  text
                  (string-join
                   (mapcar (lambda (dev) (alist-get 'name dev)) devices)
                   ", ")))
    (setq text
          (format "%s \tLast Scan\t\t\t%s\n"
                  text
                  ;; TODO: proper date parse + trim
                  (replace-regexp-in-string
                   "T" " "
                   (substring
                    (alist-get 'stateChanged
                               (alist-get 'status folder)) 0 19))))
    (setq text
          (format "%s ⇄\tLatest Change\t\t%s\n"
                  text
                  (when stats
                    (concat
                     (if (alist-get 'deleted (alist-get 'lastFile stats))
                         "Deleted " "Updated ")
                     (if (file-name-extension
                          (alist-get 'filename
                                     (alist-get 'lastFile stats)))

                         (let ((extn
                                (string-remove-prefix
                                 "."
                                 (file-name-extension
                                  (alist-get 'filename
                                             (alist-get 'lastFile stats))))))
                           (concat (file-name-sans-extension
                                    (file-name-base
                                     (alist-get 'filename
                                                (alist-get 'lastFile stats))))
                                   "." extn))
                       (file-name-base
                        (alist-get 'filename
                                   (alist-get 'lastFile stats))))))))
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
        (format "%s%s\n"
                (syncthing--bold
                 (format " %s" name))
                (if paused (syncthing--italic " (Paused)") ""))
        (unless (member id (syncthing-buffer-fold-folders syncthing-buffer))
          (syncthing--prop text)))
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

(defun syncthing--list-37-device (device)
  "Render single DEVICE item in a widget."
  (syncthing-trace)
  (let* ((name (alist-get 'name device))
        (id (alist-get 'deviceID device))
        (paused (alist-get 'paused device))
        (addresses (alist-get 'addresses device))
        (conns (alist-get
                'connections
                (alist-get 'connections
                           (syncthing-server-data syncthing-server))))
        (stats (alist-get 'stats device))
        (compression (alist-get 'compression device))
        (completion (alist-get 'completion device))
        (text "")
        (conn-type "Disconnected")
        (sync-state 0)
        dev-conn
        connected
        folders)
    (dolist (item conns)
      (when (string= id (car item))
        (setq dev-conn (cdr item))))
    (when dev-conn
      (setq connected (alist-get 'connected dev-conn))
      (when connected
        (alist-get 'connected dev-conn)
        (cond ((string-match "relay" (alist-get 'type dev-conn))
               (setq conn-type "Relay"))
              ((string-match "quic" (alist-get 'type dev-conn))
               (setq conn-type "QUIC"))
              ((string-match "tcp" (alist-get 'type dev-conn))
               (setq conn-type "TCP")))
        (setq conn-type
              (concat conn-type
                      (if (alist-get 'isLocal dev-conn) " LAN" " WAN")))))
    (setq sync-state
          (* 100 (- 1 (/ (alist-get 'needBytes completion)
                         (syncthing--maybe-float
                          (if (not (eq 0 (alist-get 'globalBytes completion)))
                              (alist-get 'globalBytes completion)
                            1) 1)))))
    (when (syncthing-buffer-collapse-after-start syncthing-buffer)
      (push id (syncthing-buffer-fold-devices syncthing-buffer)))
    (dolist (folder-cfg (alist-get 'folders
                                   (syncthing-server-data syncthing-server)))
      (dolist (dev-cfg (alist-get 'devices folder-cfg))
        (when (string= id (alist-get 'deviceID dev-cfg))
          (push (alist-get 'label folder-cfg) folders))))

    (if connected
        (setq text
              (concat text
                      (format " \tDownload Rate\t\t\t%s\n" "down")
                      (format " \tUpload Rate\t\t\t\t%s\n" "up")))
      (setq text
            (format "%s \tLast seen\t\t\t\t%s\n \tSync Status\t\t\t\t%s\n"
                    text
                    ;; TODO: proper date parse + trim
                    (replace-regexp-in-string
                     "T" " "
                     (substring (alist-get 'lastSeen stats) 0 19))
                    (if (< (floor sync-state) 100)
                        (format "Out of Sync (%.2f%%)" sync-state)
                      "Up to Date")))
      (when (< (floor sync-state) 100)
        (setq text
              (format "%s ⇄\tOut of Sync Items\t\t%s items, ~%s\n"
                      text
                      (syncthing--num-group
                       (+ (alist-get 'needItems completion)
                          (alist-get 'needDeletes completion))
                       :dec-sep syncthing-decimal-separator
                       :ths-sep syncthing-thousands-separator)
                      (syncthing--scale-bytes
                       (alist-get 'needBytes completion) 2)))))
    (setq text
          (format "%s \tAddress\t\t\t\t\t%s\n"
                  text
                  (or (unless (string= "" (alist-get 'address dev-conn))
                        (alist-get 'address dev-conn))
                      (string-join addresses "\n\t\t\t\t\t\t\t"))))
    (when connected
      (setq text
            (format
             "%s \tConnection Type\t\t\t%s\n \tNumber of Connections\t%s\n"
             text
             conn-type
             (+ 1 (length (alist-get 'secondary dev-conn))))))
    (setq text
          (format "%s \tCompression\t\t\t\t%s\n \tIdentification\t\t\t%s\n"
                  text
                  (cond ((string= compression "always") "All Data")
                        ((string= compression "metadata") "Metadata Only")
                        ((string= compression "never") "Off"))
                  (substring id 0 6)))
    (when (alist-get 'connected dev-conn)
      (setq text
            (format "%s \tVersion\t\t\t\t\t%s\n"
                    text
                    (alist-get 'clientVersion dev-conn))))
    (setq text
          (format "%s \tFolders\t\t\t\t\t%s\n"
                  text
                  (string-join folders "\n\t\t\t\t\t\t\t")))
    (save-window-excursion
      (switch-to-buffer
       (get-buffer-create (syncthing-buffer-name syncthing-buffer)))
      (widget-create
       'push-button
       :button-face "menu"
       :button-suffix
       (concat
        " "
        (syncthing--color-perc
         (alist-get 'completion (alist-get 'completion device)))
        (format "%s%s\n"
                (syncthing--bold
                 (format " %s" name))
                (if paused (syncthing--italic " (Paused)") ""))
        (unless (member id (syncthing-buffer-fold-devices syncthing-buffer))
          (syncthing--prop text)))
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
  (syncthing-trace)
  (let* ((data (syncthing-server-data server))
         (uptime
          (alist-get 'uptime (alist-get 'system-status data)))
         line)
    (dolist (item syncthing-header-items)
      (when (string= item "rate-download")
        (push (syncthing--rate-download
               (format syncthing-format-rate-download
                       (syncthing--bytes-to-rate
                        (or (alist-get 'rate-download data) -1))))
              line))
      (when (string= item "rate-upload")
        (push (syncthing--rate-upload
               (format syncthing-format-rate-upload
                       (syncthing--bytes-to-rate
                        (or (alist-get 'rate-upload data) -1))))
              line))
      (when (string= item "count-local-files")
        (push (syncthing--count-local-files
               (format
                syncthing-format-count-local-files
                (syncthing--num-group
                 (cl-reduce
                  #'+ (alist-get 'folders data)
                  :key (lambda (folder)
                         (alist-get 'localFiles
                                    (alist-get 'status folder) 0)))
                 :dec-sep syncthing-decimal-separator
                 :ths-sep syncthing-thousands-separator)))
              line))
      (when (string= item "count-local-folders")
        (push (syncthing--count-local-folders
               (format
                syncthing-format-count-local-folders
                (syncthing--num-group
                 (cl-reduce
                  #'+ (alist-get 'folders data)
                  :key (lambda (folder)
                         (alist-get 'localDirectories
                                    (alist-get 'status folder) 0)))
                 :dec-sep syncthing-decimal-separator
                 :ths-sep syncthing-thousands-separator)))
              line))
      (when (string= item "count-local-bytes")
        (push (syncthing--count-local-bytes
               (format
                syncthing-format-count-local-bytes
                (syncthing--scale-bytes
                 (cl-reduce
                  #'+ (alist-get 'folders data)
                  :key (lambda (folder)
                         (alist-get 'localBytes
                                    (alist-get 'status folder) 0)))
                 1)))
              line))
      (when (string= item "count-listeners")
        (push (syncthing--count-listeners
               (format
                syncthing-format-count-listeners
                (format
                 "%s/%s"
                 (cl-reduce
                  #'+ (alist-get 'connectionServiceStatus
                                 (alist-get 'system-status data))
                  :key (lambda (disc) (if (alist-get 'error (cdr disc)) 0 1)))
                 (length (alist-get 'connectionServiceStatus
                                    (alist-get 'system-status data))))))
              line))
      (when (string= item "count-discovery")
        (push (syncthing--count-discovery
               (format
                syncthing-format-count-discovery
                (format
                 "%s/%s"
                 (cl-reduce
                  #'+ (alist-get 'discoveryStatus
                                 (alist-get 'system-status data))
                  :key (lambda (disc) (if (alist-get 'error (cdr disc)) 0 1)))
                 (length (alist-get 'discoveryStatus
                                    (alist-get 'system-status data))))))
              line))
      (when (string= item "uptime")
        (push (syncthing--uptime
               (format syncthing-format-uptime
                       (syncthing--sec-to-uptime uptime)))
              line))
      (when (string= item "my-id")
        (push (syncthing--my-id
               (format syncthing-format-my-id
                       (substring
                        (alist-get 'myID (alist-get 'system-status data)
                                   "n/a") 0 6)))
              line))
      (when (string= item "version")
        (push (format syncthing-format-version
                      (alist-get 'system-version data "n/a"))
              line)))
    (if line (string-join (reverse line) " ") nil)))

(defun syncthing--sec-to-uptime (sec)
  "Convert SEC number to DDd HHh MMm SSs uptime string."
  (syncthing-trace)
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

(defun syncthing--draw-buffer (server)
  "Setup widgets and draw other buffer items for SERVER."
  (syncthing-trace)
  (save-window-excursion
    (switch-to-buffer
     (get-buffer-create (syncthing-buffer-name syncthing-buffer)))
    (widget-setup)
    (setq header-line-format (syncthing--header-line server))
    ;; messes up with cursor position, reset to 0,0
    (goto-char 0)))

(defun syncthing--draw (server)
  "Setup buffer and draw widgets for SERVER."
  (syncthing-trace)
  (syncthing--draw-folders server)
  (syncthing--draw-devices server)
  (when syncthing-display-changes
    (syncthing--draw-changes server))
  (when syncthing-display-logs
    (syncthing--draw-logs server))
  (syncthing--draw-buffer server))

(defun syncthing--init-state ()
  "Reset all variables holding initial state.
Optional argument SKIP-CANCEL Skip removing auto-refresh."
  (syncthing-trace)
  ;; everything += or appendable has to reset in each update
  (setf (syncthing-buffer-collapse-after-start syncthing-buffer)
        syncthing-start-collapsed
        (syncthing-buffer-fold-folders syncthing-buffer) (list))
  (setf (syncthing-buffer-fold-devices syncthing-buffer) (list)))

(defun syncthing--update (&rest _)
  "Update function for every refresh iteration."
  (syncthing-trace)
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
  (when syncthing-debug
    (with-current-buffer
        (get-buffer-create (format syncthing-trace-format-buffer name))
      (insert (format "%S\n" (syncthing--previous-func
                              "syncthing--interactive-common")))))
  (when (or (not token) (string= token ""))
    (user-error "Syncthing REST API token not configured"))
  (let ((buff (syncthing--buffer
              :name (generate-new-buffer (format syncthing-format-buffer name))
              :collapse-after-start syncthing-start-collapsed))
        (server (car (push (syncthing--server :name name :url url :token token)
                           syncthing--servers))))
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

(defun syncthing--calc-speed (server)
  "Calculate upload and download rate for SERVER."
  (syncthing-trace)
  (let* ((data (syncthing-server-data server))
         (conns (alist-get 'connections (syncthing-server-data server)))
         (last-speed-date
          (or (syncthing-server-last-speed-date server) 0))
         (now (floor (float-time)))
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
          now-total
          (syncthing-server-data server) data)))

(defun syncthing--server-update-folder-completion (server data)
  "Update folder completion DATA for SERVER."
  (syncthing-trace)
  (let* ((folders (alist-get 'folders data)))
    (dotimes (idx (length folders))
      (setf (alist-get 'completion (nth idx folders))
            (unless (alist-get 'paused (nth idx folders))
              (syncthing-request
               server "GET" (format "rest/db/completion?folder=%s"
                                    (alist-get 'id (nth idx folders)))))
            (alist-get 'status (nth idx folders))
            (syncthing-request
             server "GET" (format "rest/db/status?folder=%s"
                                  (alist-get 'id (nth idx folders))))))))

(defun syncthing--server-update-folder-stats (server data)
  "Update folder stats DATA for SERVER."
  (syncthing-trace)
  (let* ((folders (alist-get 'folders data))
         (stats (syncthing-request server "GET" "rest/stats/folder")))
    (dotimes (idx (length folders))
      (dolist (stat stats)
        (when (string= (car stat) (alist-get 'id (nth idx folders)))
          (setf (alist-get 'stats (nth idx folders))
                (cdr stat)))))))

(defun syncthing--server-update-device-completion (server data)
  "Update device completion DATA for SERVER."
  (syncthing-trace)
  (let* ((devices (alist-get 'devices data)))
    (dotimes (idx (length devices))
      (setf (alist-get 'completion (nth idx devices))
            (syncthing-request
             server "GET" (format "rest/db/completion?device=%s"
                                  (alist-get 'deviceID (nth idx devices))))))))

(defun syncthing--server-update-device-stats (server data)
  "Update device stats DATA for SERVER."
  (syncthing-trace)
  (let* ((devices (alist-get 'devices data))
         (stats (syncthing-request server "GET" "rest/stats/device")))
    (dotimes (idx (length devices))
      (dolist (stat stats)
        (when (string= (car stat) (alist-get 'deviceID (nth idx devices)))
          (setf (alist-get 'stats (nth idx devices))
                (cdr stat)))))))

(defun syncthing--server-update-device-map (data)
  "Update device completion DATA."
  (syncthing-trace)
  ;; TODO: (const (alist-get 'deviceID item) (alist-get 'device-map data))
  ;;       except it fails with raw key access by always being nil
  ;;       probably something with bad (quote) / ' / `, / etc
  (dolist (folder (alist-get 'folders data))
    (dotimes (foldev-idx (length (alist-get 'devices folder)))
      (dotimes (dev-idx (length (alist-get 'devices data)))
        (when (string= (alist-get 'deviceID (nth foldev-idx
                                                 (alist-get 'devices folder)))
                       (alist-get 'deviceID (nth dev-idx
                                                 (alist-get 'devices data))))
          (setf (alist-get 'name (nth foldev-idx (alist-get 'devices folder)))
                (alist-get 'name (nth dev-idx (alist-get 'devices data)))))))))

(defun syncthing--server-update (server)
  "Update SERVER data."
  (syncthing-trace)
  ;; TODO: handle version change: >= current + branches for each <
  ;;       via rest/config's '{"version": 37}' key
  (let* ((data (syncthing-request server "GET" "rest/config")))
    (setf (alist-get 'system-version data)
          (alist-get 'version
                     (syncthing-request server "GET" "rest/system/version"))
          (alist-get 'system-status data)
          (syncthing-request server "GET" "rest/system/status")
          (alist-get 'connections data)
          (syncthing-request server "GET" "rest/system/connections"))

    (when syncthing-display-logs
      (setf (alist-get 'logs data)
            (syncthing-request server "GET" "rest/system/log")))

    ;; TODO: Syncthing possibly timeouts after 60s with [] which causes Emacs
    ;;       to hang while waiting for the response but can be stopped with C-g
    ;;       Maybe a bug when there are no changes?
    (when syncthing-display-changes
      (setf (alist-get 'changes data)
            (syncthing-request server "GET" (format "rest/events/disk?limit=%s"
                                                    syncthing-limit-changes))))

    (syncthing--server-update-folder-completion server data)
    (syncthing--server-update-folder-stats server data)
    (syncthing--server-update-device-completion server data)
    (syncthing--server-update-device-stats server data)
    (syncthing--server-update-device-map data)

    (setf (syncthing-server-data server) data)
    (syncthing--calc-speed server)))

(defun syncthing--previous-func (&optional name)
  "Retrieve previous function from `backtrace-frame'."
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

(cl-defun syncthing--num-group (num &key (dec-sep ".") (ths-sep " "))
  "Group number's digits with decimal and thousands separators."
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

;; public funcs
(defun syncthing-request (server method endpoint &rest data)
  "Return SERVER response for METHOD at ENDPOINT for request with DATA."
  (syncthing-trace)
  (apply #'syncthing--request
         (append (list method
                       (format "%s/%s" (syncthing-server-url server) endpoint)
                       (syncthing-server-token server))
                 data)))

(defun syncthing-cleanup ()
  "Clean resources when closing the client."
  (interactive)
  (syncthing-trace)
  (message "emacs-syncthing: Cleaning up client %s"
           (syncthing-server-name syncthing-server))
  (setq syncthing--servers
        (delete syncthing-server syncthing--servers))
  (message "emacs-syncthing: Remaining open clients: %s"
           (length syncthing--servers)))

(defsubst syncthing-trace ()
  (when syncthing-debug
    (with-current-buffer
        (get-buffer-create (format syncthing-trace-format-buffer
                                   (syncthing-server-name syncthing-server)))
      (insert (format "%S\n" (syncthing--previous-func))))))

;; modes for client's session buffer(s)
(define-derived-mode syncthing-mode special-mode "Syncthing"
  "Major mode for Syncthing client.

Activating this mode will launch Syncthing client in the current window.

\\{syncthing-mode-map}"
  :group 'syncthing
  (use-local-map syncthing-mode-map)

  ;; Hook to auto-revert mode for refreshing
  (setq-local revert-buffer-function #'syncthing--update)
  ;; purge resources
  (add-hook 'kill-buffer-hook
            #'syncthing-cleanup
            syncthing-cleanup-priority t)

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
   "sName: \nSyncthing REST API base URL: \nsSynchting REST API token: ")
  (when syncthing-debug
    (with-current-buffer
        (get-buffer-create (format syncthing-trace-format-buffer name))
      (insert (format "%S\n" (syncthing--previous-func
                              "syncthing-with-base")))))
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
;;; syncthing.el ends here
