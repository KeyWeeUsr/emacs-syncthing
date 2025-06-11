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
(require 'syncthing-constants)
(require 'syncthing-custom)
(require 'syncthing-faces)
(require 'syncthing-state)
(require 'syncthing-widgets)


(defun syncthing--draw-folders-header (&optional &key before after)
  "Draw folder header with optional BEFORE and AFTER separator."
  (syncthing-trace)
  (save-window-excursion
    (switch-to-buffer
     (get-buffer-create (syncthing-buffer-name syncthing-buffer)))
    (when before
      (widget-insert (syncthing--title "\n")))
    (widget-insert
     (syncthing--title (format "%s Folders\n"
                               (syncthing--fallback-ascii "folder"))))
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
    (widget-insert
     (syncthing--title (format "%s Devices\n"
                               (syncthing--fallback-ascii "laptop"))))
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
    (widget-insert
     (syncthing--title (format "%s Logs\n"
                               (syncthing--fallback-ascii "envelope"))))
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
    (widget-insert
     (syncthing--title (format "%s Recent Changes\n"
                               (syncthing--fallback-ascii "hourglass"))))
    (when after
      (widget-insert (syncthing--title "\n")))))

(defun syncthing--draw-folders (server)
  "Draw folder widget in buffer from SERVER."
  (syncthing-trace)
  (let-alist (syncthing-server-data server)
    (syncthing--draw-folders-header)
    (cond ((>= .version 35)
           (mapc (lambda (folder)
                   (syncthing--list-37-folder folder)
                   (widget-insert "\n"))
                 (sort (copy-alist .folders) #'syncthing--sort-folders)))
          (t (syncthing-trace-log :format "problematic version %s"
                                  :args (list .version))))))

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
      (cond ((>= .version 35)
             (mapc (lambda (device)
                     (syncthing--list-37-device device)
                     (widget-insert "\n"))
                   (sort (copy-alist filtered)
                         #'syncthing--sort-devices)))
            (t (syncthing-trace-log :format "problematic version %s"
                                    :args (list .version)))))))

(defun syncthing--draw-logs (server)
  "Draw logs widget in buffer from SERVER."
  (syncthing-trace)
  (let-alist (syncthing-server-data server)
    (syncthing--draw-logs-header :before t)
    (cond ((>= .version 35)
           (syncthing--list-logs .logs))
          (t (syncthing-trace-log :format "problematic version %s"
                                  :args (list .version))))))

(defun syncthing--draw-changes (server)
  "Draw recent-changes widget in buffer from SERVER."
  (syncthing-trace)
  (let-alist (syncthing-server-data server)
    (syncthing--draw-changes-header :before t)
    (cond ((>= .version 35)
           (syncthing--list-changes .changes))
          (t (syncthing-trace-log :format "problematic version %s"
                                  :args (list .version))))))

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
         (orgtbl-mode)
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
         items)
    (when (and (syncthing-buffer-collapse-after-start syncthing-buffer)
               (not (member id (syncthing-buffer-skip-fold-folders
                                syncthing-buffer))))
      (push id (syncthing-buffer-fold-folders syncthing-buffer)))

    (push (widget-convert
           'item
           :format "%t %v\n"
           :tag (format "%s%sFolder ID%s"
                        (syncthing--fallback-ascii "info")
                        (syncthing--space syncthing-align-folder-headers)
                        (syncthing--space syncthing-align-folder-values))
           :value id)
          items)
    (push (widget-convert
           'item
           :format "%t %v\n"
           :tag (format "%s%sFolder Path%s"
                        (syncthing--fallback-ascii "folder-open")
                        (syncthing--space syncthing-align-folder-headers)
                        (syncthing--space syncthing-align-folder-values))
           :value path)
          items)
    (unless paused
      (push (widget-convert
             'item
             :format "%t %v\n"
             :tag (format "%s%sGlobal State%s"
                          (syncthing--fallback-ascii "world")
                          (syncthing--space syncthing-align-folder-headers)
                          (syncthing--space syncthing-align-folder-values))
             :value
             (format "%s %s %s"
                     (format
                      (replace-regexp-in-string
                       "<icon>" (syncthing--fallback-ascii "files")
                       syncthing-format-count-local-files)
                      (syncthing--num-group
                       (alist-get 'globalFiles (alist-get 'status folder))
                       :dec-sep syncthing-decimal-separator
                       :ths-sep syncthing-thousands-separator))
                     (format
                      (replace-regexp-in-string
                       "<icon>" (syncthing--fallback-ascii "folder")
                       syncthing-format-count-local-folders)
                      (syncthing--num-group
                       (alist-get 'globalDirectories
                                  (alist-get 'status folder))
                       :dec-sep syncthing-decimal-separator
                       :ths-sep syncthing-thousands-separator))
                     (format
                      (replace-regexp-in-string
                       "<icon>" (syncthing--fallback-ascii "drive")
                       syncthing-format-count-local-bytes)
                      (syncthing--scale-bytes
                       (alist-get 'globalBytes
                                  (alist-get 'status folder)) 2))))
            items)
      (push (widget-convert
             'item
             :format "%t %v\n"
             :tag (format "%s%sLocal State%s"
                          (syncthing--fallback-ascii "house")
                          (syncthing--space syncthing-align-folder-headers)
                          (syncthing--space syncthing-align-folder-values))
             :value
             (format "%s %s %s"
                     (format
                      (replace-regexp-in-string
                       "<icon>" (syncthing--fallback-ascii "files")
                       syncthing-format-count-local-files)
                      (syncthing--num-group
                       (alist-get 'localFiles (alist-get 'status folder))
                       :dec-sep syncthing-decimal-separator
                       :ths-sep syncthing-thousands-separator))
                     (format
                      (replace-regexp-in-string
                       "<icon>" (syncthing--fallback-ascii "folder")
                       syncthing-format-count-local-folders)
                      (syncthing--num-group
                       (alist-get 'localDirectories
                                  (alist-get 'status folder))
                       :dec-sep syncthing-decimal-separator
                       :ths-sep syncthing-thousands-separator))
                     (format
                      (replace-regexp-in-string
                       "<icon>" (syncthing--fallback-ascii "drive")
                       syncthing-format-count-local-bytes)
                      (syncthing--scale-bytes
                       (alist-get 'localBytes
                                  (alist-get 'status folder)) 2))))
            items))
    (push (widget-convert
           'item
           :format "%t %v\n"
           :tag (format "%s%sFolder Type%s"
                        (syncthing--fallback-ascii "folder")
                        (syncthing--space syncthing-align-folder-headers)
                        (syncthing--space syncthing-align-folder-values))
           :value
           (cond ((string= type "sendreceive") "Send & Receive")
                 ((string= type "sendonly") "Send Only")
                 ((string= type "receiveonly") "Receive Only")
                 ((string= type "receiveencrypted") "Receive Encrypted")))
          items)
    (push (widget-convert
           'item
           :format "%t %v\n"
           :tag (format "%s%sRescans%s"
                        (syncthing--fallback-ascii "sync")
                        (syncthing--space syncthing-align-folder-headers)
                        (syncthing--space syncthing-align-folder-values))
           :value
           (format "%s %s %s %s"
                   (syncthing--fallback-ascii "watch")
                   (format (syncthing--sec-to-uptime
                            (alist-get 'rescanIntervalS folder)
                            :full
                            (or (string= syncthing-header-uptime-type
                                         syncthing-header-uptime-full)
                                (string= syncthing-header-uptime-type
                                         syncthing-header-uptime-padded-full))
                            :pad
                            (or (string= syncthing-header-uptime-type
                                         syncthing-header-uptime-padded-short)
                                (string=
                                 syncthing-header-uptime-type
                                 syncthing-header-uptime-padded-full))))
                   (syncthing--fallback-ascii "eye")
                   (if (alist-get 'fsWatcherEnabled folder)
                       "Enabled" "Disabled")))
          items)
    (push (widget-convert
           'item
           :format "%t %v\n"
           :tag (format "%s%sFile Pull Order%s"
                        (syncthing--fallback-ascii "lift")
                        (syncthing--space syncthing-align-folder-headers)
                        (syncthing--space syncthing-align-folder-values))
           :value (cond ((string= order "random") "Random")
                        ((string= order "alphabetic") "Alphabetic")
                        ((string= order "smallestFirst") "Smallest First")
                        ((string= order "largestFirst") "Largest First")
                        ((string= order "oldestFirst") "Oldest First")
                        ((string= order "newestFirst") "Newest First")))
          items)
    (push (widget-convert
           'item
           :format "%t %v\n"
           :tag (format "%s%sShared With%s"
                        (syncthing--fallback-ascii "share")
                        (syncthing--space syncthing-align-folder-headers)
                        (syncthing--space syncthing-align-folder-values))
           :value (string-join
                   (mapcar (lambda (dev)
                             (alist-get
                              (intern `,(alist-get 'deviceID dev))
                              (alist-get 'device-map data)))
                           devices)
                   ", "))
          items)
    (push (widget-convert
           'item
           :format "%t %v\n"
           :tag (format "%s%sLast Scan%s"
                        (syncthing--fallback-ascii "watch")
                        (syncthing--space syncthing-align-folder-headers)
                        (syncthing--space syncthing-align-folder-values))
           :value (format-time-string
                   "%F %T"
                   (encode-time
                    (iso8601-parse
                     (alist-get 'stateChanged
                                (alist-get 'status folder))))))
          items)
    (push (widget-convert
           'item
           :format "%t %v\n"
           :tag (format "%s%sLatest Change%s"
                        (syncthing--fallback-ascii "swap")
                        (syncthing--space syncthing-align-folder-headers)
                        (syncthing--space syncthing-align-folder-values))
           :value
           (when stats
             (concat
              (if (alist-get 'deleted (alist-get 'lastFile stats))
                  "Deleted " "Updated ")
              (if (file-name-extension
                   (alist-get 'filename (alist-get 'lastFile stats)))

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
                 (alist-get 'filename (alist-get 'lastFile stats)))))))
          items)
    (save-window-excursion
      (switch-to-buffer
       (get-buffer-create (syncthing-buffer-name syncthing-buffer)))
      (widget-create
       'syncthing-collapsible
       :value
       (not (member id (syncthing-buffer-fold-folders syncthing-buffer)))
       :tag
       (concat
        " "
        (syncthing--color-perc perc)
        (format "%s%s"
                (syncthing--bold
                 (format " %s" name))
                (if paused (syncthing--italic " (Paused)") "")))
       :items (nreverse items)
       :spacing syncthing-align-folder-values
       :action
       `(lambda (widget &optional event)
         ;; "super"
         (apply 'widget-toggle-action `(,widget ,event))

         (if (member ,id (syncthing-buffer-fold-folders syncthing-buffer))
             (progn
               (setf (syncthing-buffer-fold-folders syncthing-buffer)
                     (delete ,id (syncthing-buffer-fold-folders
                                  syncthing-buffer)))
               (push ,id (syncthing-buffer-skip-fold-folders
                          syncthing-buffer)))
           (progn
             (push ,id (syncthing-buffer-fold-folders syncthing-buffer))
             (setf (syncthing-buffer-skip-fold-folders syncthing-buffer)
                   (delete ,id (syncthing-buffer-skip-fold-folders
                                syncthing-buffer))))))))))

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
        (conn-type "Disconnected")
        (sync-state 0)
        dev-conn
        connected
        folders
        items)
    (dolist (item conns)
      (when (string= id (car item))
        (setq dev-conn (cdr item))))
    (when dev-conn
      (setq connected (alist-get 'connected dev-conn))
      (when connected
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
        (progn
          (push (widget-convert
                 'item
                 :format "%t %v\n"
                 :tag (format "%s%sDownload Rate%s"
                              (syncthing--fallback-ascii "download")
                              (syncthing--space syncthing-align-device-headers)
                              (syncthing--space syncthing-align-device-values))
                 :value (syncthing--bytes-to-rate
                         (or (alist-get 'rate-download device) -1)))
                items)
          (push (widget-convert
                 'item
                 :format "%t %v\n"
                 :tag (format "%s%sUpload Rate%s"
                              (syncthing--fallback-ascii "upload")
                              (syncthing--space syncthing-align-device-headers)
                              (syncthing--space syncthing-align-device-values))
                 :value (syncthing--bytes-to-rate
                         (or (alist-get 'rate-upload device) -1)))
                items))
      (push (widget-convert
             'item
             :format "%t %v\n"
             :tag (format "%s%sLast seen%s"
                          (syncthing--fallback-ascii "eye")
                          (syncthing--space syncthing-align-device-headers)
                          (syncthing--space syncthing-align-device-values))
             :value (format-time-string
                     "%F %T"
                     (encode-time
                      (iso8601-parse (alist-get 'lastSeen stats)))))
            items)
      (push (widget-convert
             'item
             :format "%t %v\n"
             :tag (format "%s%sSync Status%s"
                          (syncthing--fallback-ascii "cloud")
                          (syncthing--space syncthing-align-device-headers)
                          (syncthing--space syncthing-align-device-values))
             :value (if (< (floor sync-state) 100)
                        (format "Out of Sync (%.2f%%)" sync-state)
                      "Up to Date"))
            items)
      (when (< (floor sync-state) 100)
        (push (widget-convert
               'item
               :format "%t %v\n"
               :tag (format "%s%sOut of Sync Items%s"
                            (syncthing--fallback-ascii "swap")
                            (syncthing--space syncthing-align-device-headers)
                            (syncthing--space syncthing-align-device-values))
               :value (format "%s items, ~%s"
                              (syncthing--num-group
                               (+ (alist-get 'needItems completion)
                                  (alist-get 'needDeletes completion))
                               :dec-sep syncthing-decimal-separator
                               :ths-sep syncthing-thousands-separator)
                              (syncthing--scale-bytes
                               (alist-get 'needBytes completion) 2)))
              items)))
    (push (widget-convert
           'item
           :format "%t %v\n"
           :tag (format "%s%sAddress%s"
                        (syncthing--fallback-ascii "link")
                        (syncthing--space syncthing-align-device-headers)
                        (syncthing--space syncthing-align-device-values))
           :value (or (unless (string= "" (alist-get 'address dev-conn))
                        (alist-get 'address dev-conn))
                      (string-join
                       addresses
                       (format "\n%s "
                               (syncthing--space
                                syncthing-align-device-values)))))
          items)
    (when connected
      (push (widget-convert
             'item
             :format "%t %v\n"
             :tag (format "%s%sConnection Type%s"
                          (syncthing--fallback-ascii "signal")
                          (syncthing--space syncthing-align-device-headers)
                          (syncthing--space syncthing-align-device-values))
             :value conn-type)
            items)
      (push (widget-convert
             'item
             :format "%t %v\n"
             :tag (format "%s%sNumber of Connections%s"
                          (syncthing--fallback-ascii "switch")
                          (syncthing--space syncthing-align-device-headers)
                          (syncthing--space syncthing-align-device-values))
             :value (number-to-string
                     (+ 1 (length (alist-get 'secondary dev-conn)))))
            items))
    (push (widget-convert
           'item
           :format "%t %v\n"
           :tag (format "%s%sCompression%s"
                        (syncthing--fallback-ascii "press")
                        (syncthing--space syncthing-align-device-headers)
                        (syncthing--space syncthing-align-device-values))
           :value (cond ((string= compression "always") "All Data")
                        ((string= compression "metadata") "Metadata Only")
                        ((string= compression "never") "Off")))
          items)
    (push (widget-convert
           'item
           :format "%t %v\n"
           :tag (format "%s%sIdentification%s"
                        (syncthing--fallback-ascii "qr")
                        (syncthing--space syncthing-align-device-headers)
                        (syncthing--space syncthing-align-device-values))
           :value (substring id 0 syncthing-device-short-length))
          items)
    (when (alist-get 'connected dev-conn)
      (push (widget-convert
             'item
             :format "%t %v\n"
             :tag (format "%s%sVersion%s"
                          (syncthing--fallback-ascii "tag")
                          (syncthing--space syncthing-align-device-headers)
                          (syncthing--space syncthing-align-device-values))
             :value (alist-get 'clientVersion dev-conn))
            items))
    (push (widget-convert
           'item
           :format "%t %v\n"
           :tag (format "%s%sFolders%s"
                        (syncthing--fallback-ascii "folder")
                        (syncthing--space syncthing-align-device-headers)
                        (syncthing--space syncthing-align-device-values))
           :value (string-join
                   folders
                   (format "\n%s "
                           (syncthing--space syncthing-align-device-values))))
          items)
    (save-window-excursion
      (switch-to-buffer
       (get-buffer-create (syncthing-buffer-name syncthing-buffer)))
      (widget-create
       'syncthing-collapsible
       :value (not (member id (syncthing-buffer-fold-devices
                               syncthing-buffer)))
       :tag
       (concat
        " "
        (syncthing--color-perc (alist-get 'completion completion))
        (format "%s%s" (syncthing--bold (format " %s" name))
                (if paused (syncthing--italic " (Paused)") "")))
       :items (nreverse items)
       :spacing syncthing-align-device-values
       :action
       `(lambda (widget &optional event)
          ;; "super"
          (apply 'widget-toggle-action `(,widget ,event))

          (if (member ,id (syncthing-buffer-fold-devices syncthing-buffer))
              (progn
                (setf (syncthing-buffer-fold-devices syncthing-buffer)
                      (delete ,id (syncthing-buffer-fold-devices
                                   syncthing-buffer)))
                (push ,id (syncthing-buffer-skip-fold-devices
                           syncthing-buffer)))
            (progn
              (push ,id (syncthing-buffer-fold-devices syncthing-buffer))
              (setf (syncthing-buffer-skip-fold-devices syncthing-buffer)
                    (delete ,id (syncthing-buffer-skip-fold-devices
                                 syncthing-buffer))))))))))

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
               (format (replace-regexp-in-string
                        "<icon>" (syncthing--fallback-ascii "download")
                        syncthing-format-rate-download)
                       (syncthing--bytes-to-rate
                        (or (alist-get 'rate-download data) -1))))
              line))
      (when (string= item syncthing-header-rate-upload)
        (push (syncthing--rate-upload
               (format (replace-regexp-in-string
                        "<icon>" (syncthing--fallback-ascii "upload")
                        syncthing-format-rate-upload)
                       (syncthing--bytes-to-rate
                        (or (alist-get 'rate-upload data) -1))))
              line))
      (when (string= item syncthing-header-count-local-files)
        (push (syncthing--count-local-files
               (format
                (replace-regexp-in-string
                 "<icon>" (syncthing--fallback-ascii "files")
                 syncthing-format-count-local-files)
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
                (replace-regexp-in-string
                 "<icon>" (syncthing--fallback-ascii "folder")
                 syncthing-format-count-local-folders)
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
                (replace-regexp-in-string
                 "<icon>" (syncthing--fallback-ascii "drive")
                 syncthing-format-count-local-bytes)
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
                (replace-regexp-in-string
                 "<icon>" (syncthing--fallback-ascii "network")
                 syncthing-format-count-listeners)
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
                (replace-regexp-in-string
                 "<icon>" (syncthing--fallback-ascii "sign")
                 syncthing-format-count-discovery)
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
               (format (replace-regexp-in-string
                        "<icon>" (syncthing--fallback-ascii "watch")
                        syncthing-format-uptime)
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
               (format (replace-regexp-in-string
                        "<icon>" (syncthing--fallback-ascii "qr")
                        syncthing-format-my-id)
                       (substring
                        (alist-get 'myID (alist-get 'system-status data)
                                   "n/a") 0 syncthing-device-short-length)))
              line))
      (when (string= item syncthing-header-version)
        (push (format (replace-regexp-in-string
                       "<icon>" (syncthing--fallback-ascii "tag")
                       syncthing-format-version)
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
