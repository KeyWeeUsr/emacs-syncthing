;;; syncthing-constants.el --- Client for Syncthing -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Code:

(defconst syncthing-gibibyte (expt 1024 3))
(defconst syncthing-mibibyte (expt 1024 2))
(defconst syncthing-kibibyte (expt 1024 1))
(defconst syncthing-day-seconds (* 1 60 60 24))
(defconst syncthing-hour-seconds (* 1 60 60))
(defconst syncthing-min-seconds (* 1 60))

;; Docstrings from Syncthing (MPL-2.0)
;; https://www.fsf.org/blogs/licensing/mpl-2.0-release
(defconst syncthing-event-config-saved "ConfigSaved"
  "683b481/gui/default/syncthing/core/eventService.js#L65.

Emitted after the config has been saved by the user or by Syncthing itself.")

(defconst syncthing-event-device-connected "DeviceConnected"
  "683b481/gui/default/syncthing/core/eventService.js#L66.

Generated each time a connection to a device has been established.")

(defconst syncthing-event-device-disconnected "DeviceDisconnected"
  "683b481/gui/default/syncthing/core/eventService.js#L67.

Generated each time a connection to a device has been terminated.")

(defconst syncthing-event-device-discovered "DeviceDiscovered"
  "683b481/gui/default/syncthing/core/eventService.js#L68.

Emitted when a new device is discovered using local discovery.")

(defconst syncthing-event-device-rejected "DeviceRejected"
  "683b481/gui/default/syncthing/core/eventService.js#L69.

DEPRECATED: Emitted when there is a connection from a device we are not
configured to talk to.")

(defconst syncthing-event-pending-devices-changed "PendingDevicesChanged"
  "683b481/gui/default/syncthing/core/eventService.js#L70.

Emitted when pending devices were added / updated (connection from unknown ID)
or removed (device is ignored or added).")

(defconst syncthing-event-device-paused "DevicePaused"
  "683b481/gui/default/syncthing/core/eventService.js#L71.

Emitted when a device has been paused.")

(defconst syncthing-event-device-resumed "DeviceResumed"
  "683b481/gui/default/syncthing/core/eventService.js#L72.

Emitted when a device has been resumed.")

(defconst syncthing-event-cluster-config-received "ClusterConfigReceived"
  "683b481/gui/default/syncthing/core/eventService.js#L73.

Emitted when receiving a remote device's cluster config.")

(defconst syncthing-event-download-progress "DownloadProgress"
  "683b481/gui/default/syncthing/core/eventService.js#L74.

Emitted during file downloads for each folder for each file.")

(defconst syncthing-event-failure "Failure"
  "683b481/gui/default/syncthing/core/eventService.js#L75.

Specific errors sent to the usage reporting server for diagnosis.")

(defconst syncthing-event-folder-completion "FolderCompletion"
  "683b481/gui/default/syncthing/core/eventService.js#L76.

Emitted when the local or remote contents for a folder changes.")

(defconst syncthing-event-folder-rejected "FolderRejected"
  "683b481/gui/default/syncthing/core/eventService.js#L77.

DEPRECATED: Emitted when a device sends index information for a folder we do
not have, or have but do not share with the device in question.")

(defconst syncthing-event-pending-folders-changed "PendingFoldersChanged"
  "683b481/gui/default/syncthing/core/eventService.js#L78.

Emitted when pending folders were added / updated (offered by some device, but
not shared to them) or removed (folder ignored or added or no longer offered
from the remote device).")

(defconst syncthing-event-folder-summary "FolderSummary"
  "683b481/gui/default/syncthing/core/eventService.js#L79.

Emitted when folder contents have changed locally.")

(defconst syncthing-event-item-finished "ItemFinished"
  "683b481/gui/default/syncthing/core/eventService.js#L80.

Generated when Syncthing ends synchronizing a file to a newer version.")

(defconst syncthing-event-item-started "ItemStarted"
  "683b481/gui/default/syncthing/core/eventService.js#L81.

Generated when Syncthing begins synchronizing a file to a newer version.")

(defconst syncthing-event-listen-addresses-changed "ListenAddressesChanged"
  "683b481/gui/default/syncthing/core/eventService.js#L82.

Listen address resolution has changed.")

(defconst syncthing-event-local-change-detected "LocalChangeDetected"
  "683b481/gui/default/syncthing/core/eventService.js#L83.

Generated upon scan whenever the local disk has discovered an updated file from
the previous scan.")

(defconst syncthing-event-local-index-updated "LocalIndexUpdated"
  "683b481/gui/default/syncthing/core/eventService.js#L84.

Generated when the local index information has changed, due to synchronizing
one or more items from the cluster or discovering local changes during a scan.")

(defconst syncthing-event-login-attempt "LoginAttempt"
  "683b481/gui/default/syncthing/core/eventService.js#L85.

Emitted on every login attempt when authentication is enabled for the GUI.")

(defconst syncthing-event-remote-change-detected "RemoteChangeDetected"
  "683b481/gui/default/syncthing/core/eventService.js#L86.

Generated upon scan whenever a file is locally updated due to a remote
change.")

(defconst syncthing-event-remote-download-progress "RemoteDownloadProgress"
  "683b481/gui/default/syncthing/core/eventService.js#L87.

DownloadProgress message received from a connected remote device.")

(defconst syncthing-event-remote-index-updated "RemoteIndexUpdated"
  "683b481/gui/default/syncthing/core/eventService.js#L88.

Generated each time new index information is received from a device.")

(defconst syncthing-event-starting "Starting"
  "683b481/gui/default/syncthing/core/eventService.js#L89.

Emitted exactly once, when Syncthing starts, before parsing configuration
etc.")

(defconst syncthing-event-startup-completed "StartupCompleted"
  "683b481/gui/default/syncthing/core/eventService.js#L90.

Emitted exactly once, when initialization is complete and Syncthing is ready to
start exchanging data with other devices.")

(defconst syncthing-event-state-changed "StateChanged"
  "683b481/gui/default/syncthing/core/eventService.js#L91.

Emitted when a folder changes state.")

(defconst syncthing-event-folder-errors "FolderErrors"
  "683b481/gui/default/syncthing/core/eventService.js#L92.

Emitted when a folder has errors preventing a full sync.")

(defconst syncthing-event-folder-watch-state-changed "FolderWatchStateChanged"
  "683b481/gui/default/syncthing/core/eventService.js#L93.

Watcher routine encountered a new error, or a previous error disappeared after
retrying.")

(defconst syncthing-event-folder-scan-progress "FolderScanProgress"
  "683b481/gui/default/syncthing/core/eventService.js#L94.

Emitted every ScanProgressIntervalS seconds, indicating how far into the scan
it is at.")

(defconst syncthing-event-folder-paused "FolderPaused"
  "683b481/gui/default/syncthing/core/eventService.js#L95.

Emitted when a folder is paused.")

(defconst syncthing-event-folder-resumed "FolderResumed"
  "683b481/gui/default/syncthing/core/eventService.js#L96.

Emitted when a folder is resumed.")

(provide 'syncthing-constants)
;;; syncthing-constants.el ends here
