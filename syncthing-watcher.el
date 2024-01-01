;;; syncthing-watcher.el --- Client for Syncthing -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Code:

(require 'syncthing-network)
(require 'syncthing-state)


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

(provide 'syncthing-watcher)
;;; syncthing-watcher.el ends here
