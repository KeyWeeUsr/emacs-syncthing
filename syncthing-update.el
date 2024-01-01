;;; syncthing-update.el --- Client for Syncthing -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Code:

(require 'syncthing-common)
(require 'syncthing-custom)
(require 'syncthing-draw)
(require 'syncthing-network)
(require 'syncthing-state)
(require 'syncthing-watcher)


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
  "Update separate and aggregate completions for all folders and devices.
Argument SERVER `syncthing-server' instance.
Argument DATA server data."
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

(provide 'syncthing-update)
;;; syncthing-update.el ends here
