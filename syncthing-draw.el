;;; syncthing-draw.el --- Client for Syncthing -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'iso8601)
(require 'org-table)
(require 'subr-x)
(require 'widget)
(require 'wid-edit)

(require 'syncthing-common)
(require 'syncthing-custom)
(require 'syncthing-faces)
(require 'syncthing-state)


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
  (let ((data (syncthing-server-data syncthing-server)))
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
               (push (format
                      "|%s|%s|%s|%s|%s|%s|"
                      (alist-get (intern `,.modifiedBy)
                                 (alist-get 'device-map data))
                      .action .type .label .path
                      (format-time-string
                       "%F %T"
                       (encode-time
                        (iso8601-parse (alist-get 'time item)))))
                     text)))
           (insert (string-join (reverse text) "\n")))
         (org-mode)
         (org-table-align)
         (substring-no-properties (buffer-string)))))))

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
    (setq
     text
     (format
      "%s \tRescans\t\t\t\t%s\n"
      text
      (format " %s  %s"
              (format (syncthing--sec-to-uptime
                       (alist-get 'rescanIntervalS folder)
                       :full (or (string= syncthing-header-uptime-type
                                          syncthing-header-uptime-full)
                                 (string= syncthing-header-uptime-type
                                          syncthing-header-uptime-padded-full))
                       :pad (or (string= syncthing-header-uptime-type
                                         syncthing-header-uptime-padded-short)
                                (string=
                                 syncthing-header-uptime-type
                                 syncthing-header-uptime-padded-full))))
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
                  (format-time-string
                            "%F %T"
                            (encode-time
                             (iso8601-parse
                              (alist-get 'stateChanged
                                         (alist-get 'status folder)))))))
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
       `(lambda (&rest _event)
         (if (member ,id (syncthing-buffer-fold-folders syncthing-buffer))
             (progn
               (setf (syncthing-buffer-fold-folders syncthing-buffer)
                     (delete ,id
                             (syncthing-buffer-fold-folders syncthing-buffer)))
               (push ,id (syncthing-buffer-skip-fold-folders syncthing-buffer))
               (save-excursion
                 (widget-delete (syncthing--get-widget (point)))
                 (syncthing--list-37-folder ',folder)))
           (progn
             (push ,id (syncthing-buffer-fold-folders syncthing-buffer))
             (setf (syncthing-buffer-skip-fold-folders syncthing-buffer)
                   (delete ,id (syncthing-buffer-skip-fold-folders
                               syncthing-buffer)))
             (save-excursion
               (widget-delete (syncthing--get-widget (point)))
               (syncthing--list-37-folder ',folder)))))

       (if (member id (syncthing-buffer-fold-folders syncthing-buffer))
           (syncthing--bold ">")
         (syncthing--bold "v"))))))

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
                    (format-time-string
                     "%F %T"
                     (encode-time (iso8601-parse (alist-get 'lastSeen stats))))
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
       `(lambda (&rest _event)
         (if (member ,id (syncthing-buffer-fold-devices syncthing-buffer))
             (progn
               (setf (syncthing-buffer-fold-devices syncthing-buffer)
                     (delete ,id
                             (syncthing-buffer-fold-devices syncthing-buffer)))
               (push ,id (syncthing-buffer-skip-fold-devices syncthing-buffer))
               (save-excursion
                 (widget-delete (syncthing--get-widget (point)))
                 (syncthing--list-37-device ',device)))
           (progn
             (push ,id (syncthing-buffer-fold-devices syncthing-buffer))
             (setf (syncthing-buffer-skip-fold-devices syncthing-buffer)
                   (delete ,id (syncthing-buffer-skip-fold-devices
                               syncthing-buffer)))
             (save-excursion
               (widget-delete (syncthing--get-widget (point)))
               (syncthing--list-37-device ',device)))))

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
                       (syncthing--sec-to-uptime
                        uptime
                        :full (or (string= syncthing-header-uptime-type
                                           syncthing-header-uptime-full)
                                  (string=
                                   syncthing-header-uptime-type
                                   syncthing-header-uptime-padded-full))
                        :pad (or (string= syncthing-header-uptime-type
                                          syncthing-header-uptime-padded-short)
                                 (string=
                                  syncthing-header-uptime-type
                                  syncthing-header-uptime-padded-full)))))
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

(provide 'syncthing-draw)
;;; syncthing-draw.el ends here
