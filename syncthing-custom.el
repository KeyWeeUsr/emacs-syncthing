;;; syncthing-custom.el --- Client for Syncthing -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Code:

(require 'syncthing-groups)


(defcustom syncthing-format-buffer
  "*syncthing(%s)*"
  "Client's buffer name with a `%s' placeholder for address."
  :group 'syncthing-startup
  :group 'syncthing-format
  :type 'string)

(defcustom syncthing-trace-format-buffer
  "*syncthing trace(%s)*"
  "Tracing buffer name with a `%s' placeholder for address."
  :group 'syncthing-debug
  :group 'syncthing-format
  :type 'string)

(defcustom syncthing-default-name
  "Default Localhost"
  "Default client name for `syncthing-format-buffer'."
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
  :group 'syncthing-format
  :type 'string)

(defcustom syncthing-cleanup-priority
  0
  "`add-hook' priority."
  :group 'syncthing-cleanup
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
  :group 'syncthing-display
  :type 'boolean)

(defcustom syncthing-prefer-unicode
  t
  "Prefer unicode characters when rendering."
  :group 'syncthing-display
  :type 'boolean)

(defcustom syncthing-start-with-auto-refresh
  t
  "Start with auto-refresh enabled."
  :group 'syncthing-startup
  :type 'boolean)

(defcustom syncthing-auto-refresh-interval
  10
  "Number of seconds to wait before refreshing client buffer."
  :group 'syncthing-time
  :type 'number)

(defconst syncthing-header-rate-download "rate-download"
  "Header value for `syncthing-header-items'.")
(defconst syncthing-header-rate-upload "rate-upload"
  "Header value for `syncthing-header-items'.")
(defconst syncthing-header-count-local-files "count-local-files"
  "Header value for `syncthing-header-items'.")
(defconst syncthing-header-count-local-folders "count-local-folders"
  "Header value for `syncthing-header-items'.")
(defconst syncthing-header-count-local-bytes "count-local-bytes"
  "Header value for `syncthing-header-items'.")
(defconst syncthing-header-count-listeners "count-listeners"
  "Header value for `syncthing-header-items'.")
(defconst syncthing-header-count-discovery "count-discovery"
  "Header value for `syncthing-header-items'.")
(defconst syncthing-header-uptime "uptime"
  "Header value for `syncthing-header-items'.")
(defconst syncthing-header-my-id "my-id"
  "Header value for `syncthing-header-items'.")
(defconst syncthing-header-version "version"
  "Header value for `syncthing-header-items'.")
(defcustom syncthing-header-items
  '("rate-download" "rate-upload" "count-local-files" "count-local-folders"
    "count-local-bytes" "count-listeners" "count-discovery" "uptime" "my-id"
    "version")
  "Items to render with `header-line-format'.

Special meaning for empty list / nil to skip rendering the header line."
  :group 'syncthing-display
  :type `(repeat
          (choice :tag "Item"
                  (const :tag "Download rate" ,syncthing-header-rate-download)
                  (const :tag "Upload rate" ,syncthing-header-rate-upload)
                  (const :tag "Files" ,syncthing-header-count-local-files)
                  (const :tag "Folders" ,syncthing-header-count-local-folders)
                  (const :tag "Size" ,syncthing-header-count-local-bytes)
                  (const :tag "Listeners" ,syncthing-header-count-listeners)
                  (const :tag "Discovery" ,syncthing-header-count-discovery)
                  (const :tag "Uptime" ,syncthing-header-uptime)
                  (const :tag "ID" ,syncthing-header-my-id)
                  (const :tag "Version" ,syncthing-header-version))))

(defcustom syncthing-display-logs
  nil
  "Display logs in `syncthing-buffer'."
  :group 'syncthing-display
  :type 'boolean)

(defcustom syncthing-timeout-events
  0
  "Amount of time to wait for server to generate events."
  :group 'syncthing-times
  :type 'number)

(defcustom syncthing-display-changes
  t
  "Display recent-changes in `syncthing-buffer'.

Note:
    Syncthing timeouts after 60s with [] when there are no events and the
    listener waits for some to be emitted which causes Emacs to hang while
    waiting for the response but can be stopped with \\[keyboard-quit].
    This is customized with `syncthing-timeout-events'.
    https://docs.syncthing.net/rest/events-get.html"
  :group 'syncthing-display
  :type 'boolean)

(defcustom syncthing-limit-changes
  25
  "Limit of items for recent changes."
  :group 'syncthing-limits
  :type 'number)

(defcustom syncthing-debug
  nil
  "Enable debugging logs in special buffer."
  :group 'syncthing-debug
  :type 'boolean)

(defcustom syncthing-prefix
  "syncthing"
  "Prefix for `message' logs."
  :group 'syncthing-debug
  :type 'string)

(defcustom syncthing-decimal-separator
  "."
  "Stylize number with custom decimal separator."
  :group 'syncthing-format
  :type 'string)

(defcustom syncthing-thousands-separator
  " "
  "Stylize number with custom thousands separator."
  :group 'syncthing-format
  :type 'string)

(defcustom syncthing-watch-events
  t
  "Poll Syncthing server for events such as status, files or errors.

Note:
    Syncthing timeouts after 60s with [] when there are no events and the
    listener waits for some to be emitted which causes Emacs to hang while
    waiting for the response but can be stopped with \\[keyboard-quit].
    This is customized with `syncthing-timeout-events'.
    https://docs.syncthing.net/rest/events-get.html"
  :group 'syncthing-events
  :type 'boolean)

(defcustom syncthing-watch-events-interval
  1
  "Number of seconds to wait before polling for next event batch."
  :group 'syncthing-events
  :group 'syncthing-time
  :type 'number)

(defcustom syncthing-no-upstream-noise
  t
  "Prevent any upstream library to pollute minibuffer with `message'.

Many libs, modes and other facilities have their info messages, but can be
annoying or hide more urgent messages when used at large scale or in short
intervals. Setting this to non-nil allows `syncthing' to purge all of them."
  :group 'syncthing-debug
  :type 'boolean)

(defcustom syncthing-info
  t
  "Show info messages in minibuffer."
  :group 'syncthing-debug
  :type 'boolean)

(defconst syncthing-header-uptime-short "uptime-short"
  "Header value for `syncthing-header-uptime-type'.")
(defconst syncthing-header-uptime-full "uptime-full"
  "Header value for `syncthing-header-uptime-type'.")
(defconst syncthing-header-uptime-padded-short "uptime-padded-short"
  "Header value for `syncthing-header-uptime-type'.")
(defconst syncthing-header-uptime-padded-full "uptime-padded-full"
  "Header value for `syncthing-header-uptime-type'.")
(defcustom syncthing-header-uptime-type
  syncthing-header-uptime-padded-short
  "Items to render with `header-line-format'.

Special meaning for empty list / nil to skip rendering the header line."
  :group 'syncthing-display
  :type `(choice :tag "Type"
                 (const :tag "[0d] [0h] [0m] [0s]"
                        ,syncthing-header-uptime-short)
                 (const :tag "0d 0h 0m 0s"
                        ,syncthing-header-uptime-full)
                 (const :tag "[000d] [00h] [00m] [00s]"
                        ,syncthing-header-uptime-padded-short)
                 (const :tag "000d 00h 00m 00s"
                        ,syncthing-header-uptime-padded-full)))

(defcustom syncthing-align-folder-headers
  3
  "`:align-to' value for aligning text block for values in folder widget."
  :group 'syncthing-display
  :type 'number)

(defcustom syncthing-align-folder-values
  20
  "`:align-to' value for aligning text block for values in folder widget."
  :group 'syncthing-display
  :type 'number)

(defcustom syncthing-align-device-headers
  3
  "`:align-to' value for aligning text block for values in device widget."
  :group 'syncthing-display
  :type 'number)

(defcustom syncthing-align-device-values
  20
  "`:align-to' value for aligning text block for values in device widget."
  :group 'syncthing-display
  :type 'number)

(defcustom syncthing-format-rate-download
  "<icon> %s"
  "Format for displaying download rate in header line."
  :group 'syncthing-format
  :type 'string)

(defcustom syncthing-format-rate-upload
  "<icon> %s"
  "Format for displaying upload rate in header line."
  :group 'syncthing-format
  :type 'string)

(defcustom syncthing-format-count-local-files
  "<icon> %s"
  "Format for displaying local files count in header line."
  :group 'syncthing-format
  :type 'string)

(defcustom syncthing-format-count-local-folders
  "<icon> %s"
  "Format for displaying local folders count in header line."
  :group 'syncthing-format
  :type 'string)

(defcustom syncthing-format-count-local-bytes
  "<icon> ~%s"
  "Format for displaying local size in header line."
  :group 'syncthing-format
  :type 'string)

(defcustom syncthing-format-count-listeners
  "<icon> %s"
  "Format for displaying listeners count in header line."
  :group 'syncthing-format
  :type 'string)

(defcustom syncthing-format-count-discovery
  "<icon> %s"
  "Format for displaying discovery count in header line."
  :group 'syncthing-format
  :type 'string)

(defcustom syncthing-format-uptime
  "<icon> %s"
  "Format for displaying Syncthing server uptime in header line."
  :group 'syncthing-format
  :type 'string)

(defcustom syncthing-format-my-id
  "<icon> %s"
  "Format for displaying current device's ID in header line."
  :group 'syncthing-format
  :type 'string)

(defcustom syncthing-format-version
  "<icon> %s"
  "Format for displaying version in header line."
  :group 'syncthing-format
  :type 'string)


(provide 'syncthing-custom)
;;; syncthing-custom.el ends here
