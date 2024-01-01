;;; syncthing.el --- Client for Syncthing -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Peter Badida

;; Author: Peter Badida <keyweeusr@gmail.com>
;; Keywords: convenience, syncthing, sync, client, view
;; Version: 2.1.0
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

(require 'autorevert)

(require 'syncthing-groups)
(require 'syncthing-custom)
(require 'syncthing-faces)
(require 'syncthing-constants)
(require 'syncthing-state)
(require 'syncthing-keyboard)
(require 'syncthing-common)
(require 'syncthing-network)
(require 'syncthing-update)
(require 'syncthing-watcher)
(require 'syncthing-draw)


;; private/helper funcs
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

;; public funcs
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
  (use-local-map syncthing-keyboard-map)

  ;; Hook to auto-revert mode for refreshing
  (setq-local revert-buffer-function #'syncthing--update)
  ;; purge resources
  (add-hook 'kill-buffer-hook
            #'syncthing-cleanup
            syncthing-cleanup-priority t)

  (when syncthing-start-with-auto-refresh
    (syncthing-auto-refresh-mode 1)))

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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
