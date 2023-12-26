;;; syncthing.el --- Client for Syncthing -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Peter Badida

;; Author: Peter Badida <keyweeusr@gmail.com>
;; Keywords: convenience, syncthing, sync, client, view
;; Version: 1.7.0
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

(require 'syncthing-groups)
(require 'syncthing-custom)
(require 'syncthing-faces)
(require 'syncthing-constants)
(require 'syncthing-state)
(require 'syncthing-keyboard)

;; inlines & macros
(defsubst syncthing-trace ()
  "Simple tracing inline func to dump caller and its args into a buffer."
  (when syncthing-debug
    (with-current-buffer
        (get-buffer-create (format syncthing-trace-format-buffer
                                   (syncthing-server-name syncthing-server)))
      (insert (format "%S\n" (syncthing--previous-func))))))

;; private/helper funcs
(defun syncthing--ping (server)
  "Check whether we can use the API at SERVER with TOKEN."
  (syncthing-trace)
  (let ((url-request-method "GET")
        (url-request-extra-headers
         `(("X-Api-Key" . ,(syncthing-server-token server))))
        (url-show-status (null syncthing-no-upstream-noise)))
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
        (url-request-extra-headers `(("X-Api-Key" . ,token)))
        (url-show-status (null syncthing-no-upstream-noise)))
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
  (let* ((data (syncthing-server-data syncthing-server))
         (name (alist-get 'label folder))
         (id (alist-get 'id folder))
         (paused (alist-get 'paused folder))
         (type (alist-get 'type folder))
         (path (alist-get 'path folder))
         (devices (alist-get 'devices folder))
         (completion
          (alist-get
           (intern `,id)
           (alist-get
            (intern `,(alist-get 'myID (alist-get 'system-status data)))
            (syncthing-server-completion syncthing-server))))
         (perc (or (alist-get 'completion completion) 0))
         (order (alist-get 'order folder))
         (stats (alist-get 'stats folder))
         (text ""))
    (when (and (syncthing-buffer-collapse-after-start syncthing-buffer)
               (not (member id (syncthing-buffer-skip-fold-folders
                                syncthing-buffer))))
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
                   (mapcar (lambda (dev)
                             (alist-get
                              (intern `,(alist-get 'deviceID dev))
                              (alist-get 'device-map data)))
                           devices)
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
               (push id (syncthing-buffer-skip-fold-folders syncthing-buffer))
               (save-excursion
                 (widget-delete (syncthing--get-widget (point)))
                 (syncthing--list-37-folder folder)))
           (progn
             ;; TODO: redundant check for push?
             (if (syncthing-buffer-fold-folders syncthing-buffer)
                 (push id (syncthing-buffer-fold-folders syncthing-buffer))
               (setf (syncthing-buffer-fold-folders syncthing-buffer)
                     (list id)))
             (setf (syncthing-buffer-skip-fold-folders syncthing-buffer)
                   (delete id (syncthing-buffer-skip-fold-folders
                               syncthing-buffer)))
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
        (completion
         (alist-get
          'aggregate
          (alist-get (intern `,id)
                     (syncthing-server-completion syncthing-server))))
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
    (when (and (syncthing-buffer-collapse-after-start syncthing-buffer)
               (not (member id (syncthing-buffer-skip-fold-devices
                                syncthing-buffer))))
      (push id (syncthing-buffer-fold-devices syncthing-buffer)))
    (dolist (folder-cfg (alist-get 'folders
                                   (syncthing-server-data syncthing-server)))
      (dolist (dev-cfg (alist-get 'devices folder-cfg))
        (when (string= id (alist-get 'deviceID dev-cfg))
          (push (alist-get 'label folder-cfg) folders))))

    (if connected
        (setq text
              (concat text
                      (format " \tDownload Rate\t\t\t%s\n"
                              (syncthing--bytes-to-rate
                               (or (alist-get 'rate-download device) -1)))
                      (format " \tUpload Rate\t\t\t\t%s\n"
                              (syncthing--bytes-to-rate
                               (or (alist-get 'rate-upload device) -1)))))
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
        (syncthing--color-perc (alist-get 'completion completion))
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
               (push id (syncthing-buffer-skip-fold-devices syncthing-buffer))
               (save-excursion
                 (widget-delete (syncthing--get-widget (point)))
                 (syncthing--list-37-device device)))
           (progn
             ;; TODO: redundant check for push?
             (if (syncthing-buffer-fold-devices syncthing-buffer)
                 (push id (syncthing-buffer-fold-devices syncthing-buffer))
               (setf (syncthing-buffer-fold-devices syncthing-buffer)
                     (list id)))
             (setf (syncthing-buffer-skip-fold-devices syncthing-buffer)
                   (delete id (syncthing-buffer-skip-fold-devices
                               syncthing-buffer)))
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
      (when (string= item syncthing-header-rate-download)
        (push (syncthing--rate-download
               (format syncthing-format-rate-download
                       (syncthing--bytes-to-rate
                        (or (alist-get 'rate-download data) -1))))
              line))
      (when (string= item syncthing-header-rate-upload)
        (push (syncthing--rate-upload
               (format syncthing-format-rate-upload
                       (syncthing--bytes-to-rate
                        (or (alist-get 'rate-upload data) -1))))
              line))
      (when (string= item syncthing-header-count-local-files)
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
      (when (string= item syncthing-header-count-local-folders)
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
      (when (string= item syncthing-header-count-local-bytes)
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
      (when (string= item syncthing-header-count-listeners)
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
      (when (string= item syncthing-header-count-discovery)
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
      (when (string= item syncthing-header-uptime)
        (push (syncthing--uptime
               (format syncthing-format-uptime
                       (syncthing--sec-to-uptime uptime)))
              line))
      (when (string= item syncthing-header-my-id)
        (push (syncthing--my-id
               (format syncthing-format-my-id
                       (substring
                        (alist-get 'myID (alist-get 'system-status data)
                                   "n/a") 0 6)))
              line))
      (when (string= item syncthing-header-version)
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
    ;; header drawing messes up with cursor position, reset to 0,0
    (goto-char (or (syncthing-buffer-point syncthing-buffer) 0))))

(defun syncthing--draw (server)
  "Setup buffer and draw widgets for SERVER."
  (syncthing-trace)
  (when (syncthing-buffer-initialized syncthing-buffer)
    (setf (syncthing-buffer-point syncthing-buffer) (point)))
  (let ((inhibit-read-only t))
    (erase-buffer))
  (syncthing--draw-folders server)
  (syncthing--draw-devices server)
  (when syncthing-display-changes
    (syncthing--draw-changes server))
  (when syncthing-display-logs
    (syncthing--draw-logs server))
  (syncthing--draw-buffer server)
  (setf (syncthing-buffer-initialized syncthing-buffer) t))

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

(defun syncthing--calc-speed (server data)
  "Calculate upload and download rate DATA for SERVER."
  (syncthing-trace)
  (let* ((conns (alist-get 'connections data))
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
          now-total)
    (let (dev dev-id conn)
      (dotimes (dev-idx (length (alist-get 'devices data)))
        (setq dev (nth dev-idx (alist-get 'devices data)))
        (setq dev-id (alist-get 'deviceID dev))
        (dotimes (conn-idx (length (alist-get 'connections conns)))
          (setq conn (nth conn-idx (alist-get 'connections conns)))
          (when (and (string= dev-id (car conn))
                     (alist-get 'connected (cdr conn)))
            (let* ((dev-id (alist-get 'deviceID dev))
                   (dev-now-total (cdr conn))
                   (dev-conns-total
                    (alist-get (intern `,dev-id)
                               (syncthing-server-dev-connections-total server))))
              (setf (alist-get 'rate-download dev)
                    (max
                     0 (/ (- (alist-get 'inBytesTotal dev-now-total)
                             (or (alist-get 'inBytesTotal dev-conns-total) 0))
                          td))
                    (alist-get 'rate-upload dev)
                    (max
                     0 (/ (- (alist-get 'outBytesTotal dev-now-total)
                             (or (alist-get 'outBytesTotal dev-conns-total) 0))
                          td))
                    (alist-get
                     (intern `,dev-id)
                     (syncthing-server-dev-connections-total server))
                    dev-now-total))))
        ;; replace with modified item
        (setf (nth dev-idx (alist-get 'devices data)) dev))))
  data)

(defun syncthing--server-update-completion (server data)
  "Update separate and aggregate completions for all folders and devices."
  (syncthing-trace)
  (let* ((devices (alist-get 'devices data))
         (folders (alist-get 'folders data))
         completion)
    (let (device-id tmp folder-id)
      (dolist (device devices) ; remote + local
        (setq device-id (alist-get 'deviceID device))
        (setq tmp nil)

        (dolist (folder folders)
          (unless (alist-get 'paused folder)
            (setq folder-id (alist-get 'id folder))
            (setf (alist-get (intern `,folder-id) tmp)
                  (syncthing-request
                   server "GET"
                   (format "rest/db/completion?device=%s&folder=%s"
                           device-id folder-id)))))
        (unless (alist-get 'paused device)
          (setf (alist-get 'aggregate tmp)
                (syncthing-request
                 server "GET"
                 (format "rest/db/completion?device=%s" device-id)))
          (setf (alist-get (intern `,device-id) completion) tmp))))
    completion))

(defun syncthing--server-update-folder-status (server data)
  "Update folder completion DATA for SERVER."
  (syncthing-trace)
  (let ((folders (alist-get 'folders data)))
    (dotimes (idx (length folders))
      (let ((folder (nth idx folders)))
        (setf (alist-get 'status folder)
              (syncthing-request
               server "GET" (format "rest/db/status?folder=%s"
                                    (alist-get 'id folder)))
              (nth idx folders)
              folder)))
    (setf (alist-get 'folders data) folders))
  data)

(defun syncthing--server-update-folder-stats (server data)
  "Update folder stats DATA for SERVER."
  (syncthing-trace)
  (let* ((folders (alist-get 'folders data))
         (stats (syncthing-request server "GET" "rest/stats/folder")))
    (dotimes (idx (length folders))
      (dolist (stat stats)
        (when (string= (car stat) (alist-get 'id (nth idx folders)))
          (setf (alist-get 'stats (nth idx folders))
                (cdr stat))))))
  data)

(defun syncthing--server-update-device-stats (server data)
  "Update device stats DATA for SERVER."
  (syncthing-trace)
  (let* ((devices (alist-get 'devices data))
         (stats (syncthing-request server "GET" "rest/stats/device")))
    (dotimes (idx (length devices))
      (let ((device (nth idx devices)))
        (dolist (stat stats)
          (when (string= (car stat) (alist-get 'deviceID device))
            (setf (alist-get 'stats device) (cdr stat))))
        (setf (nth idx devices) device)))
    (setf (alist-get 'devices data) devices))
  data)

(defun syncthing--server-update-device-map (data)
  "Create deviceID<->name map in DATA."
  (syncthing-trace)
  (let (device-map)
    (dolist (folder (alist-get 'folders data))
      (dolist (fol-dev (alist-get 'devices folder))
        (dolist (device (alist-get 'devices data))
          (let ((fol-dev-id (alist-get 'deviceID fol-dev)))
            (when (string= fol-dev-id
                           (alist-get 'deviceID device))
              (setf (alist-get (intern `,fol-dev-id) device-map)
                    (alist-get 'name device)))))))
    (setf (alist-get 'device-map data) device-map))
  data)

(defun syncthing--server-update (server)
  "Update SERVER data."
  (syncthing-trace)
  ;; TODO: handle version change: >= current + branches for each <
  ;;       via rest/config's '{"version": 37}' key
  (let ((data (syncthing-request server "GET" "rest/config")))
    (setf (alist-get 'system-version data)
          (alist-get 'version
                     (syncthing-request server "GET" "rest/system/version"))
          (alist-get 'system-status data)
          (syncthing-request server "GET" "rest/system/status")
          (alist-get 'connections data)
          (syncthing-request server "GET" "rest/system/connections"))

    (when (and syncthing-watch-events (not syncthing-watcher))
      (syncthing--watcher-start server))

    (when syncthing-display-logs
      (setf (alist-get 'logs data)
            (syncthing-request server "GET" "rest/system/log")))

    (when syncthing-display-changes
      (setf (alist-get 'changes data)
            (syncthing-request
             server "GET"
             (format "rest/events/disk?limit=%s&timeout=%s"
                     syncthing-limit-changes
                     syncthing-timeout-events))))

    (setq data (syncthing--server-update-folder-status server data))
    (setq data (syncthing--server-update-folder-stats server data))
    (setq data (syncthing--server-update-device-stats server data))
    (setq data (syncthing--server-update-device-map data))
    (setf (syncthing-server-completion server)
          (syncthing--server-update-completion server data))

    (setq data (syncthing--calc-speed server data))
    (setf (syncthing-server-data server) data)
    (when syncthing-watch-events
      (syncthing--watcher-poll server syncthing-watcher))))

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

(defun syncthing--watcher-start (server)
  "Start `syncthing-watcher' and continue polling for events.
Argument SERVER `syncthing-server' instance."
  (setq-local syncthing-watcher (syncthing--watcher))
  (put 'syncthing-watcher 'permanent-local t)
  (syncthing--watcher-poll server syncthing-watcher t))

(defun syncthing--watcher-poll (server watcher &optional init)
  "Poll Syncthing for incoming events.
Argument SERVER `syncthing-server' instance.
Argument WATCHER `syncthing-watcher' instance.
Optional argument INIT Are we in the initialization stage?"
  (let* ((endpoint (if init
                       (format "rest/events?timeout=%s&limit=1"
                               syncthing-timeout-events)
                     (format "rest/events?timeout=%s&since=%s"
                             syncthing-timeout-events
                             (syncthing-watcher-last-id watcher))))
         (events (syncthing-request server "GET" endpoint))
         last-id type)
    (dolist (event events)
      (setq last-id (alist-get 'id event))
      (setq type (alist-get 'type event))
      (cond ((string= type syncthing-event-state-changed)
             (syncthing--watcher-state-changed event))
            ((string= type syncthing-event-config-saved)
             (syncthing--watcher-config-saved event))
            ((string= type syncthing-event-device-connected)
             (syncthing--watcher-device-connected event))
            ((string= type syncthing-event-device-disconnected)
             (syncthing--watcher-device-disconnected event))
            ((string= type syncthing-event-device-discovered)
             (syncthing--watcher-device-discovered event))
            ((string= type syncthing-event-device-rejected)
             (syncthing--watcher-device-rejected event))
            ((string= type syncthing-event-pending-devices-changed)
             (syncthing--watcher-pending-devices-changed event))
            ((string= type syncthing-event-device-paused)
             (syncthing--watcher-device-paused event))
            ((string= type syncthing-event-device-resumed)
             (syncthing--watcher-device-resumed event))
            ((string= type syncthing-event-cluster-config-received)
             (syncthing--watcher-cluster-config-received event))
            ((string= type syncthing-event-download-progress)
             (syncthing--watcher-download-progress event))
            ((string= type syncthing-event-failure)
             (syncthing--watcher-failure event))
            ((string= type syncthing-event-folder-completion)
             (syncthing--watcher-folder-completion event))
            ((string= type syncthing-event-folder-rejected)
             (syncthing--watcher-folder-rejected event))
            ((string= type syncthing-event-pending-folders-changed)
             (syncthing--watcher-pending-folders-changed event))
            ((string= type syncthing-event-folder-summary)
             (syncthing--watcher-folder-summary event))
            ((string= type syncthing-event-item-finished)
             (syncthing--watcher-item-finished event))
            ((string= type syncthing-event-item-started)
             (syncthing--watcher-item-started event))
            ((string= type syncthing-event-listen-addresses-changed)
             (syncthing--watcher-listen-addresses-changed event))
            ((string= type syncthing-event-local-change-detected)
             (syncthing--watcher-local-change-detected event))
            ((string= type syncthing-event-local-index-updated)
             (syncthing--watcher-local-index-updated event))
            ((string= type syncthing-event-login-attempt)
             (syncthing--watcher-login-attempt event))
            ((string= type syncthing-event-remote-change-detected)
             (syncthing--watcher-remote-change-detected event))
            ((string= type syncthing-event-remote-download-progress)
             (syncthing--watcher-remote-download-progress event))
            ((string= type syncthing-event-remote-index-updated)
             (syncthing--watcher-remote-index-updated event))
            ((string= type syncthing-event-starting)
             (syncthing--watcher-starting event))
            ((string= type syncthing-event-startup-completed)
             (syncthing--watcher-startup-completed event))
            ((string= type syncthing-event-state-changed)
             (syncthing--watcher-state-changed event))
            ((string= type syncthing-event-folder-errors)
             (syncthing--watcher-folder-errors event))
            ((string= type syncthing-event-folder-watch-state-changed)
             (syncthing--watcher-folder-watch-state-changed event))
            ((string= type syncthing-event-folder-scan-progress)
             (syncthing--watcher-folder-scan-progress event))
            ((string= type syncthing-event-folder-paused)
             (syncthing--watcher-folder-paused event))
            ((string= type syncthing-event-folder-resumed)
             (syncthing--watcher-folder-resumed event))))
    (when last-id
      ;; set only if there was some event present
      (setf (syncthing-watcher-last-id watcher) last-id))))

(defun syncthing--watcher-config-saved (event)
  "TODO docstring, handle EVENT."
  (ignore event)
  (when syncthing-info
    (message "%s: Event: syncthing--watcher-config-saved"
             syncthing-prefix)))

(defun syncthing--watcher-device-connected (event)
  "TODO docstring, handle EVENT."
  (ignore event)
  (when syncthing-info
    (message "%s: Event: syncthing--watcher-device-connected"
             syncthing-prefix)))

(defun syncthing--watcher-device-disconnected (event)
  "TODO docstring, handle EVENT."
  (ignore event)
  (when syncthing-info
    (message "%s: Event: syncthing--watcher-device-disconnected"
             syncthing-prefix)))

(defun syncthing--watcher-device-discovered (event)
  "TODO docstring, handle EVENT."
  (ignore event)
  (when syncthing-info
    (message "%s: Event: syncthing--watcher-device-discovered"
             syncthing-prefix)))

(defun syncthing--watcher-device-rejected (event)
  "TODO docstring, handle EVENT."
  (ignore event)
  (when syncthing-info
    (message "%s: Event: syncthing--watcher-device-rejected"
             syncthing-prefix)))

(defun syncthing--watcher-pending-devices-changed (event)
  "TODO docstring, handle EVENT."
  (ignore event)
  (when syncthing-info
    (message "%s: Event: syncthing--watcher-pending-devices-changed"
             syncthing-prefix)))

(defun syncthing--watcher-device-paused (event)
  "TODO docstring, handle EVENT."
  (ignore event)
  (when syncthing-info
    (message "%s: Event: syncthing--watcher-device-paused"
             syncthing-prefix)))

(defun syncthing--watcher-device-resumed (event)
  "TODO docstring, handle EVENT."
  (ignore event)
  (when syncthing-info
    (message "%s: Event: syncthing--watcher-device-resumed"
             syncthing-prefix)))

(defun syncthing--watcher-cluster-config-received (event)
  "TODO docstring, handle EVENT."
  (ignore event)
  (when syncthing-info
    (message "%s: Event: syncthing--watcher-cluster-config-received"
             syncthing-prefix)))

(defun syncthing--watcher-download-progress (event)
  "TODO docstring, handle EVENT."
  (ignore event)
  (when syncthing-info
    (message "%s: Event: syncthing--watcher-download-progress"
             syncthing-prefix)))

(defun syncthing--watcher-failure (event)
  "TODO docstring, handle EVENT."
  (ignore event)
  (when syncthing-info
    (message "%s: Event: syncthing--watcher-failure"
             syncthing-prefix)))

(defun syncthing--watcher-folder-completion (event)
  "TODO docstring for EVENT."
  (let* ((server-data (syncthing-server-data syncthing-server))
         (folders (alist-get 'folders server-data))
         (data (alist-get 'data event)))
    (dolist (folder folders)
      (when (string= (alist-get 'folder data) (alist-get 'id folder))
        (setf (alist-get 'completion folder) data))))
  (when syncthing-info
    (message "%s: Event: syncthing--watcher-folder-completion"
             syncthing-prefix)))

(defun syncthing--watcher-folder-rejected (event)
  "TODO docstring, handle EVENT."
  (ignore event)
  (when syncthing-info
    (message "%s: Event: syncthing--watcher-folder-rejected"
             syncthing-prefix)))

(defun syncthing--watcher-pending-folders-changed (event)
  "TODO docstring, handle EVENT."
  (ignore event)
  (when syncthing-info
    (message "%s: Event: syncthing--watcher-pending-folders-changed"
             syncthing-prefix)))

(defun syncthing--watcher-folder-summary (event)
  "TODO docstring, handle EVENT."
  (ignore event)
  (when syncthing-info
    (message "%s: Event: syncthing--watcher-folder-summary"
             syncthing-prefix)))

(defun syncthing--watcher-item-finished (event)
  "TODO docstring, handle EVENT."
  (ignore event)
  (when syncthing-info
    (message "%s: Event: syncthing--watcher-item-finished"
             syncthing-prefix)))

(defun syncthing--watcher-item-started (event)
  "TODO docstring, handle EVENT."
  (ignore event)
  (when syncthing-info
    (message "%s: Event: syncthing--watcher-item-started"
             syncthing-prefix)))

(defun syncthing--watcher-listen-addresses-changed (event)
  "TODO docstring, handle EVENT."
  (ignore event)
  (when syncthing-info
    (message "%s: Event: syncthing--watcher-listen-addresses-changed"
             syncthing-prefix)))

(defun syncthing--watcher-local-change-detected (event)
  "TODO docstring, handle EVENT."
  (ignore event)
  (when syncthing-info
    (message "%s: Event: syncthing--watcher-local-change-detected"
             syncthing-prefix)))

(defun syncthing--watcher-local-index-updated (event)
  "TODO docstring, handle EVENT."
  (ignore event)
  (when syncthing-info
    (message "%s: Event: syncthing--watcher-local-index-updated"
             syncthing-prefix)))

(defun syncthing--watcher-login-attempt (event)
  "TODO docstring, handle EVENT."
  (ignore event)
  (when syncthing-info
    (message "%s: Event: syncthing--watcher-login-attempt"
             syncthing-prefix)))

(defun syncthing--watcher-remote-change-detected (event)
  "TODO docstring, handle EVENT."
  (ignore event)
  (when syncthing-info
    (message "%s: Event: syncthing--watcher-remote-change-detected"
             syncthing-prefix)))

(defun syncthing--watcher-remote-download-progress (event)
  "TODO docstring, handle EVENT."
  (ignore event)
  (when syncthing-info
    (message "%s: Event: syncthing--watcher-remote-download-progress"
             syncthing-prefix)))

(defun syncthing--watcher-remote-index-updated (event)
  "TODO docstring, handle EVENT."
  (ignore event)
  (when syncthing-info
    (message "%s: Event: syncthing--watcher-remote-index-updated"
             syncthing-prefix)))

(defun syncthing--watcher-starting (event)
  "TODO docstring, handle EVENT."
  (ignore event)
  (when syncthing-info
    (message "%s: Event: syncthing--watcher-starting"
             syncthing-prefix)))

(defun syncthing--watcher-startup-completed (event)
  "TODO docstring, handle EVENT."
  (ignore event)
  (when syncthing-info
    (message "%s: Event: syncthing--watcher-startup-completed"
             syncthing-prefix)))

(defun syncthing--watcher-state-changed (event)
  "TODO docstring, handle EVENT."
  (ignore event)
  (when syncthing-info
    (message "%s: Event: syncthing--watcher-state-changed"
             syncthing-prefix)))

(defun syncthing--watcher-folder-errors (event)
  "TODO docstring, handle EVENT."
  (ignore event)
  (when syncthing-info
    (message "%s: Event: syncthing--watcher-folder-errors"
             syncthing-prefix)))

(defun syncthing--watcher-folder-watch-state-changed (event)
  "TODO docstring, handle EVENT."
  (ignore event)
  (when syncthing-info
    (message "%s: Event: syncthing--watcher-folder-watch-state-changed"
             syncthing-prefix)))

(defun syncthing--watcher-folder-scan-progress (event)
  "TODO docstring, handle EVENT."
  (ignore event)
  (when syncthing-info
    (message "%s: Event: syncthing--watcher-folder-scan-progress"
             syncthing-prefix)))

(defun syncthing--watcher-folder-paused (event)
  "TODO docstring, handle EVENT."
  (ignore event)
  (when syncthing-info
    (message "%s: Event: syncthing--watcher-folder-paused"
             syncthing-prefix)))

(defun syncthing--watcher-folder-resumed (event)
  "TODO docstring, handle EVENT."
  (ignore event)
  (when syncthing-info
    (message "%s: Event: syncthing--watcher-folder-resumed"
             syncthing-prefix)))

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
  (when syncthing-info
    (message "%s: Cleaning up client %s"
             syncthing-prefix
             (syncthing-server-name syncthing-server)))
  (setq syncthing--servers
        (delete syncthing-server syncthing--servers))
  (when syncthing-info
    (message "%s: Remaining open clients: %s"
             syncthing-prefix
             (length syncthing--servers))))

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
  (when syncthing-no-upstream-noise
    (setq-local auto-revert-verbose nil))
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
