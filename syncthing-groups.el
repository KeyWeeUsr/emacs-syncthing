;;; syncthing-groups.el --- Client for Syncthing -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Code:

(defgroup syncthing nil
  "Basic configuration."
  :group 'external
  :group 'communication)

(defgroup syncthing-startup nil
  "Start-up stage."
  :group 'syncthing)

(defgroup syncthing-debug nil
  "Debugging and verbosity configuration."
  :group 'syncthing)

(defgroup syncthing-faces nil
  "Faces used in client buffers."
  :group 'syncthing)

(defgroup syncthing-format nil
  "String formatting and templating."
  :group 'syncthing)

(defgroup syncthing-times nil
  "Times and intervals."
  :group 'syncthing)

(defgroup syncthing-limits nil
  "Limits and paginations."
  :group 'syncthing)

(defgroup syncthing-display nil
  "What to display in client buffers."
  :group 'syncthing)

(defgroup syncthing-events nil
  "Configuration for `syncthing-watcher'."
  :group 'syncthing)

(defgroup syncthing-cleanup nil
  "Cleanup configuration."
  :group 'syncthing)

(defgroup syncthing-icons nil
  "Constants for non-ASCII icons."
  :group 'syncthing)

(defgroup syncthing-ascii nil
  "Constants for ASCII icons."
  :group 'syncthing)

(provide 'syncthing-groups)
;;; syncthing-groups.el ends here
