;;; syncthing-state.el --- Client for Syncthing -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Code:

(require 'cl-lib)

(require 'syncthing-custom)


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
  name url token data
  connections-total dev-connections-total last-speed-date completion)

(defvar-local syncthing-buffer nil
  "Buffer-local instance for all drawables and other buffer states.")

(cl-defstruct (syncthing-buffer
               (:copier nil) (:named nil) (:constructor syncthing--buffer))
  "Local state holder for Syncthing buffer drawables and state."
  name collapse-after-start initialized
  fold-folders skip-fold-folders
  fold-devices skip-fold-devices
  point)

(defvar-local syncthing-watcher nil
  "Buffer-local instance for event poller.")

(cl-defstruct (syncthing-watcher
               (:copier nil) (:named nil) (:constructor syncthing--watcher))
  "Poller and state holder for Syncthing server events."
  (last-id 1))

(provide 'syncthing-state)
;;; syncthing-state.el ends here
