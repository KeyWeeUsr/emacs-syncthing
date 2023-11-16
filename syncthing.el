;;; syncthing.el --- Emacs client for Syncthing -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Peter Badida

;; Author: Peter Badida <keyweeusr@gmail.com>
;; Keywords: convenience, syncthing, sync, client, view
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))
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

;;; Code:

(require 'widget)
(require 'wid-edit)

(defgroup syncthing
  nil
  "Customization group for `syncthing'."
  :group 'external
  :group 'communication)

(defgroup syncthing-startup
  nil
  "Customization sub-group for `syncthing' start-up stage."
  :group 'syncthing)

(defgroup syncthing-faces
  nil
  "Customization group for `syncthing' faces."
  :group 'syncthing)

;; constants
(defconst syncthing-buffer
  "*syncthing-ID*"
  "Syncthing output destination.")

;; customization values
(defcustom syncthing-base-url
  "https://127.0.0.1:8384"
  "Base URL for Syncthing REST API endpoint."
  :group 'syncthing
  :type '(string))

(defcustom syncthing-format-perc
  "%6.2f%%"
  "Format for displaying process percentage."
  :group 'syncthing
  :type '(string))

(defcustom syncthing-cleanup-priority
  0
  "`add-hook' priority."
  :group 'syncthing
  :type '(number))

(defcustom syncthing-token
  nil
  "Syncthing REST API token."
  :group 'syncthing
  :type '(string))

(defcustom syncthing-start-collapsed
  t
  "Start all items collapsed."
  :group 'syncthing-startup
  :type '(boolean))

(defcustom syncthing-start-with-auto-refresh
  t
  "Start with auto-refresh enabled."
  :group 'syncthing-startup
  :type '(boolean))

(defcustom syncthing-auto-refresh-interval-sec
  10
  "Refresh interval in seconds."
  :group 'syncthing
  :type '(number))

;; customization faces/colors/fonts
(defface syncthing-title
  '((((class color) (background dark))
     (:inherit 'info-title-1))
    (((class color) (background light))
     (:inherit 'info-title-1))
    (t :inherit 'info-title-1))
  "Face for section titles."
  :group 'syncthing-faces)

(defface syncthing-prop
  '((((class color) (background dark))
     (:foreground "white" :height 0.75))
    (((class color) (background light))
     (:foreground "black" :height 0.75))
    (t (:height 0.75)))
  "Face for item properties."
  :group 'syncthing-faces)

(defface syncthing-bold
  '((((class color) (background dark))
     (:foreground "white" :bold t))
    (((class color) (background light))
     (:foreground "black" :bold t))
    (t (:bold t)))
  "Face for bold."
  :group 'syncthing-faces)

(defface syncthing-italic
  '((((class color) (background dark))
     (:foreground "white" :italic t))
    (((class color) (background light))
     (:foreground "black" :italic t))
    (t (:italic t)))
  "Face for italic."
  :group 'syncthing-faces)

(defface syncthing-green
  '((((class color) (background dark))
     (:foreground "green"))
    (((class color) (background light))
     (:foreground "green"))
    (t (:foreground "green")))
  "Face for 100% progress."
  :group 'syncthing-faces)

(defface syncthing-light-green
  '((((class color) (background dark))
     (:foreground "lightgreen"))
    (((class color) (background light))
     (:foreground "lightgreen"))
    (t (:foreground "lightgreen")))
  "Face for 75%-100% progress."
  :group 'syncthing-faces)

(defface syncthing-yellow
  '((((class color) (background dark))
     (:foreground "yellow"))
    (((class color) (background light))
     (:foreground "yellow"))
    (t (:foreground "yellow")))
  "Face for 50%-75% progress."
  :group 'syncthing-faces)

(defface syncthing-orange
  '((((class color) (background dark))
     (:foreground "orange"))
    (((class color) (background light))
     (:foreground "orange"))
    (t (:foreground "orange")))
  "Face for 25%-50% progress."
  :group 'syncthing-faces)

(defface syncthing-red
  '((((class color) (background dark))
     (:foreground "red"))
    (((class color) (background light))
     (:foreground "red"))
    (t (:foreground "red")))
  "Face for 0%-25% progress."
  :group 'syncthing-faces)

(defface syncthing-deep-sky-blue
  '((((class color) (background dark))
     (:foreground "deep sky blue"))
    (((class color) (background light))
     (:foreground "deep sky blue"))
    (t (:foreground "deep sky blue")))
  "Face for current download rate."
  :group 'syncthing-faces)

(defface syncthing-white
  '((((class color) (background dark))
     (:foreground "white"))
    (((class color) (background light))
     (:foreground "white"))
    (t (:foreground "white")))
  "Face for local files counter."
  :group 'syncthing-faces)

(defface syncthing-light-sea-green
  '((((class color) (background dark))
     (:foreground "light sea green"))
    (((class color) (background light))
     (:foreground "light sea green"))
    (t (:foreground "light sea green")))
  "Face for local bytes counter."
  :group 'syncthing-faces)

(defface syncthing-steel-blue
  '((((class color) (background dark))
     (:foreground "steel blue"))
    (((class color) (background light))
     (:foreground "steel blue"))
    (t (:foreground "steel blue")))
  "Face for discovery counter."
  :group 'syncthing-faces)

(defface syncthing-orchid
  '((((class color) (background dark))
     (:foreground "orchid"))
    (((class color) (background light))
     (:foreground "orchid"))
    (t (:foreground "orchid")))
  "Face for uptime counter."
  :group 'syncthing-faces)

(defface syncthing-id-blue
  '((((class color) (background dark))
     (:foreground "#3498db"))
    (((class color) (background light))
     (:foreground "#3498db"))
    (t (:foreground "#3498db")))
  "Face for this device's ID."
  :group 'syncthing-faces)

;; local/state variables
(defvar-local syncthing--session-buffer
  ""
  "Tmp to hold session's buffer instance/object.")

(defvar-local syncthing--fold-folders
  nil
  "Tmp list to hold IDs of folds.")

(defvar-local syncthing--fold-devices
  nil
  "Tmp list to hold IDs of folds.")

(defvar-local syncthing--collapse-after-start
  nil
  "Tmp to hold collapse toggle.")

(defvar-local syncthing--count-local-files
  0
  "Tmp to hold local state.")

(defvar-local syncthing--count-local-folders
  0
  "Tmp to hold local state.")

(defvar-local syncthing--count-local-bytes
  0
  "Tmp to hold local state.")

(defvar-local syncthing--version
  ""
  "Tmp to hold local state.")

(defvar-local syncthing--my-id
  ""
  "Tmp to hold local state.")

(defvar-local syncthing--uptime
  0
  "Tmp to hold local state.")

(defvar-local syncthing--auto-refresh
  nil
  "Tmp to hold local state.")

(defvar-local syncthing--auto-refresh-timer
  nil
  "Tmp to hold local state.")

;; keyboard
(defvar-local syncthing-mode-map
  (let ((map (make-keymap)))
    ;; (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'syncthing--newline)
    (define-key map (kbd "?") #'describe-bindings)
    map))

;; private/helper funcs
(defun syncthing--request (method url &rest data)
  "Send authenticated HTTP request to Syncthing REST API.
Argument METHOD HTTP method/verb.
Argument URL API to call.
Optional argument DATA Data to send."
  (unless syncthing-token
    (setq syncthing-token (read-string "Synchting REST API token: ")))

  (let ((url-request-method method)
        (url-request-data data)
        (url-request-extra-headers
         `(("X-Api-Key" . ,syncthing-token))))
    (ignore url-request-method method
            url-request-data data
            url-request-extra-headers)
    (with-temp-buffer
      (url-insert-file-contents url)
      (json-parse-buffer :object-type 'alist))))

(defun syncthing--get-widget (pos)
  "Try to find an Emacs Widget at POS."
  (let ((button (get-char-property pos 'button)))
    (or button
        (setq button (get-char-property (line-beginning-position) 'button)))
    button))

(defun syncthing--newline (pos &optional event)
  "RET/Enter/newline-keypress handler.
Argument POS Incoming EVENT position."
  (interactive "@d")
  (let ((button (syncthing--get-widget pos)))
    (if button
	    (widget-apply-action button event)
      (error "You can't edit this part of the Syncthing buffer"))))

(defun syncthing--url (path)
  "Assemble full API url from PATH."
  (format "%s/%s" syncthing-base-url path))

(defun syncthing--title (text)
  "Format TEXT as title."
  (propertize text 'face 'syncthing-title))

(defun syncthing--prop (text)
  "Format TEXT as property."
  (propertize text 'face 'syncthing-prop))

(defun syncthing--bold (text)
  "Format TEXT as bold."
  (propertize text 'face 'syncthing-bold))

(defun syncthing--italic (text)
  "Format TEXT as italic."
  (propertize text 'face 'syncthing-italic))

(defun syncthing--green (text)
  "Format TEXT as =green=."
  (propertize text 'face 'syncthing-green))

(defun syncthing--light-green (text)
  "Format TEXT as =lightgreen=."
  (propertize text 'face 'syncthing-light-green))

(defun syncthing--yellow (text)
  "Format TEXT as =yellow=."
  (propertize text 'face 'syncthing-yellow))

(defun syncthing--orange (text)
  "Format TEXT as =orange=."
  (propertize text 'face 'syncthing-orange))

(defun syncthing--red (text)
  "Format TEXT as =red=."
  (propertize text 'face 'syncthing-red))

(defun syncthing--deep-sky-blue (text)
  "Format TEXT as =deep sky blue=."
  (propertize text 'face 'syncthing-deep-sky-blue))

(defun syncthing--white (text)
  "Format TEXT as =white=."
  (propertize text 'face 'syncthing-white))

(defun syncthing--light-sea-green (text)
  "Format TEXT as =light sea green=."
  (propertize text 'face 'syncthing-light-sea-green))

(defun syncthing--steel-blue (text)
  "Format TEXT as =steel blue=."
  (propertize text 'face 'syncthing-steel-blue))

(defun syncthing--orchid (text)
  "Format TEXT as =orchid=."
  (propertize text 'face 'syncthing-orchid))

(defun syncthing--id-blue (text)
  "Format TEXT as Syncthing ID blue (=#3498db=)."
  (propertize text 'face 'syncthing-id-blue))

(defun syncthing--list ()
  "List all resources."
  (let-alist (syncthing--request
              "GET" (syncthing--url "rest/config"))
    (cond ((eq .version 37)
           (save-window-excursion
             (switch-to-buffer (get-buffer-create syncthing--session-buffer))
             (widget-insert (syncthing--title " Folders\n")))
           (mapc
            #'syncthing--list-37-folder
            (sort .folders
                  (lambda (left right)
                    (let ((lname "")
                          (rname ""))
                      (dolist (litem left)
                        (when (string-equal "label" (car litem))
                          (setq lname (cdr litem))))
                      (dolist (ritem right)
                        (when (string-equal "label" (car ritem))
                          (setq rname (cdr ritem))))
                      (string< lname rname)))))
           (save-window-excursion
             (switch-to-buffer (get-buffer-create syncthing--session-buffer))
             (widget-insert (syncthing--title "\n"))
             (widget-insert (syncthing--title " Devices\n")))
           (mapc
            #'syncthing--list-37-device
            (sort .devices
                  (lambda (left right)
                    (let ((lname "")
                          (rname ""))
                      (dolist (litem left)
                        (when (string-equal "name" (car litem))
                          (setq lname (cdr litem))))
                      (dolist (ritem right)
                        (when (string-equal "name" (car ritem))
                          (setq rname (cdr ritem))))
                      (string< lname rname)))))))))

(defun syncthing--progress (device-id folder-id)
  "Get progress for DEVICE-ID and FOLDER-ID."
  (let-alist (syncthing--request
              "GET" (syncthing--url
                     (format "rest/db/completion?device=%s&folder=%s"
                             device-id folder-id)))
    .completion))

(defun syncthing--list-37-folder (folder)
  "Render single FOLDER item in a widget."
  (let ((name "")
        (id "")
        (type "")
        (path "")
        (devices nil)
        (perc 0))
    (dolist (item folder)
      (cond ((string-equal "label" (car item))
             (setq name (cdr item)))
            ((string-equal "id" (car item))
             (setq id (cdr item))
             (when syncthing--collapse-after-start
               (push id syncthing--fold-folders)))
            ((string-equal "type" (car item))
             (setq type (cdr item)))
            ((string-equal "path" (car item))
             (setq path (cdr item)))
            ((string-equal "devices" (car item))
             (setq devices (cdr item)))))
    (let-alist (syncthing--request
                "GET" (syncthing--url
                       (format "rest/db/status?folder=%s" id)))
      (setq syncthing--count-local-files
            (+ syncthing--count-local-files .localFiles))
      (setq syncthing--count-local-bytes
            (+ syncthing--count-local-bytes .localBytes))
      (setq syncthing--count-local-folders
            (+ syncthing--count-local-folders .localDirectories)))
    (dolist (item (syncthing--request
                   "GET" (syncthing--url
                          (format "rest/db/completion?folder=%s" id))))
      (cond ((string-equal "completion" (car item))
             (setq perc (cdr item)))))
    (save-window-excursion
      (switch-to-buffer (get-buffer-create syncthing--session-buffer))
      (widget-create
       'push-button
       :button-face "menu"
       :button-suffix
       (concat
        " "
        (syncthing--color-perc perc)
        (syncthing--bold (format " %s\n" name))
        (unless (member id syncthing--fold-folders)
          (syncthing--prop (format "\t%s\n\t%s\n\t%s\n\t%s\n"
                                   id type path devices))))
       :action
       (lambda (&rest _event)
         (if (member id syncthing--fold-folders)
             (progn
               (setq syncthing--fold-folders
                     (delete id syncthing--fold-folders))
               (save-excursion
                 (widget-delete (syncthing--get-widget (point)))
                 (syncthing--list-37-folder folder)))
           (progn
             (if syncthing--fold-folders
                 (push id syncthing--fold-folders)
               (setq syncthing--fold-folders (list id)))
             (save-excursion
               (widget-delete (syncthing--get-widget (point)))
               (syncthing--list-37-folder folder)))))
       (if (member id syncthing--fold-folders)
           (syncthing--bold ">")
         (syncthing--bold "v"))))))

(defun syncthing--color-perc (perc)
  "Colorize PERC float."
  (cond ((< perc 25)
         (syncthing--red (format syncthing-format-perc perc)))
        ((and (>= perc 25) (< perc 50))
         (syncthing--orange (format syncthing-format-perc perc)))
        ((and (>= perc 50) (< perc 75))
         (syncthing--yellow (format syncthing-format-perc perc)))
        ((and (>= perc 75) (< perc 100))
         (syncthing--light-green (format syncthing-format-perc perc)))
        ((>= perc 100)
         (syncthing--green (format syncthing-format-perc perc)))))

(defun syncthing--list-37-device (device)
  "Render single DEVICE item in a widget."
  (let ((name "")
        (id "")
        (paused t)
        (addresses '())
        (perc 0))
    (dolist (item device)
      (cond ((string-equal "name" (car item))
             (setq name (format "%s" (cdr item))))
            ((string-equal "deviceID" (car item))
             (setq id (format "%s" (cdr item)))
             (when syncthing--collapse-after-start
               (push id syncthing--fold-devices)))
            ((string-equal "paused" (car item))
             (setq paused (if (eq (cdr item) :false) "active" "paused")))
            ((string-equal "addresses" (car item))
             (setq addresses (format "%s" (cdr item))))))
    (dolist (item (syncthing--request
                   "GET" (syncthing--url
                          (format "rest/db/completion?device=%s" id))))
      (cond ((string-equal "completion" (car item))
             (setq perc (cdr item)))))
    (save-window-excursion
      (switch-to-buffer (get-buffer-create syncthing--session-buffer))
      (widget-create
       'push-button
       :button-face "menu"
       :button-suffix
       (concat
        " "
        (syncthing--color-perc perc)
        (syncthing--bold (format " %s\n" name))
        (unless (member id syncthing--fold-devices)
          (syncthing--prop (format "\t%s\n\t%s\n\t%s\n"
                                   id paused addresses))))
       :action
       (lambda (&rest _event)
         (if (member id syncthing--fold-devices)
             (progn
               (setq syncthing--fold-devices
                     (delete id syncthing--fold-devices))
               (save-excursion
                 (widget-delete (syncthing--get-widget (point)))
                 (syncthing--list-37-device device)))
           (progn
             (if syncthing--fold-devices
                 (push id syncthing--fold-devices)
               (setq syncthing--fold-devices (list id)))
             (save-excursion
               (widget-delete (syncthing--get-widget (point)))
               (syncthing--list-37-device device)))))
       (if (member id syncthing--fold-devices)
           (syncthing--bold ">")
         (syncthing--bold "v"))))))

(defun syncthing--draw ()
  "Setup buffer and draw widgets."
  (syncthing--list)
  (save-window-excursion
    (switch-to-buffer (get-buffer-create syncthing--session-buffer))
    (widget-setup)
    (let-alist (syncthing--request
                "GET" (syncthing--url "rest/system/version"))
      (setq syncthing--version .version))
    (let-alist (syncthing--request
                "GET" (syncthing--url "rest/system/status"))
      (setq syncthing--my-id
            (substring .myID 0 6))
      (setq syncthing--uptime .uptime))
    (setq header-line-format
          (concat
           " "
           (syncthing--deep-sky-blue " 0B/s")
           " "
           (syncthing--light-green " 0B/s")
           " "
           (syncthing--white
            (format " %d" syncthing--count-local-files))
           " "
           (syncthing--yellow
            (format " %d" syncthing--count-local-folders))
           " "
           (syncthing--light-sea-green
            (format " ~%.1fGiB"
                    (/ syncthing--count-local-bytes
                       (* 1024.0 1024.0 1024.0))))
           " "
           (syncthing--green " 3/3")
           " "
           (syncthing--steel-blue " 4/5")
           " "
           (syncthing--orchid
            (format " %dd %dh %dm"
                    0
                    (/ syncthing--uptime 3600)
                    (* 60 (- (/ syncthing--uptime 3600.0)
                             (/ syncthing--uptime 3600)))))
           " "  ;; bad glyph! :(
           (syncthing--id-blue (format " %s" syncthing--my-id))
           " "
           (format " %s" syncthing--version)))
    ;; messes up with cursor position, reset to 0,0
    (goto-char 0)))

(defun syncthing--init-state ()
  "Reset all variables holding initial state.
Optional argument SKIP-CANCEL Skip removing auto-refresh."
  ;; everything += or appendable has to reset in each update
  (setq syncthing--fold-folders (list))
  (setq syncthing--fold-devices (list))
  (setq syncthing--collapse-after-start
        syncthing-start-collapsed)
  (setq syncthing--count-local-files 0)
  (setq syncthing--count-local-folders 0)
  (setq syncthing--count-local-bytes 0)
  (setq syncthing--version "")
  (setq syncthing--uptime 0))

(defun syncthing--cleanup (&rest _ignore)
  "Stop auto-refresh and clean resources, if any."
  ;; known timer
  (when syncthing--auto-refresh-timer
    (cancel-timer syncthing--auto-refresh-timer)
    (setq syncthing--auto-refresh-timer nil))

  ;; possible leak due to some strange behavior/bug
  (dolist (timer timer-list)
    (let ((text (format "%s" timer)))
      (when (and (string-match "syncthing-timer" text)
                 (string-match "killed buffer" text))
        (message "Syncthing cleanup: Canceling dangling timer '%s'" timer)
        (cancel-timer timer)))))

(defun syncthing--next-buffer-id ()
  "Check all buffers and return next ID for multi-session Syncthing client."
  (let ((id 1)
        (buf-name ""))
    (dolist (buf (buffer-list))
      (setq buf-name (buffer-name buf))
      (when (string-match
             (replace-regexp-in-string
              "ID" "\\\\([0-9]+\\\\)"
              syncthing-buffer)
             buf-name)
        (setq id (1+ id))))
    id))

(defun syncthing--update ()
  "Update function for every refresh iteration."
  (let ((inhibit-read-only t))
    (erase-buffer))

  (syncthing--init-state)
  (syncthing--draw)
  (setq syncthing--collapse-after-start nil)
  (switch-to-buffer (get-buffer-create syncthing--session-buffer)))

(defun syncthing--buffer-with-timer (name)
  "Find out whether there's a timer associated with a buffer NAME."
  ;; Necessary with major mode killing local variables of which one holds the
  ;; timer reference. Global var would prevent multiple sessions or would be
  ;; overall annoying.
  (let ((any nil))
    (dolist (timer timer-list)
      (let ((text (format "%s" timer)))
        (when (and (string-match "syncthing-timer" text)
                   (string-match (regexp-quote name) text))
          (setq any t))))
    any))

;; modes for client's session buffer(s)
(define-derived-mode syncthing-mode
  special-mode
  "Syncthing"
  "Launch Syncthing client in the current window."
  :group 'syncthing
  (add-hook 'kill-buffer-hook
            #'syncthing--cleanup
            syncthing-cleanup-priority t)
  (syncthing--cleanup)

  ;; current buffer, new one is created via `(syncthing)'
  ;;
  ;; make sure it's initialized only once, otherwise (current-buffer) fetches
  ;; value from any other window currently in focus causing a bit of a mess
  (when (string-equal "" syncthing--session-buffer)
    (setq syncthing--session-buffer (current-buffer)))

  ;; custom handler for RET / widget input handler
  ;; (keymap-local-set "RET" #'syncthing--newline)
  ;; (keymap-local-set "?" #'describe-bindings)
  (use-local-map syncthing-mode-map)

  (syncthing--update)
  ;; schedule only when configured and only once
  (when (and syncthing-start-with-auto-refresh
             (not syncthing--auto-refresh-timer)
             (not (syncthing--buffer-with-timer
                   (buffer-name syncthing--session-buffer))))
    (syncthing-auto-refresh-mode 1)))

(define-minor-mode syncthing-auto-refresh-mode
  "Enable auto-refreshing state for `syncthing-mode'."
  :lighter " Auto-refresh"
  (if (not syncthing-auto-refresh-mode)
      (when syncthing--auto-refresh-timer
        (cancel-timer syncthing--auto-refresh-timer)
        (setq syncthing--auto-refresh-timer nil))
    (setq syncthing--auto-refresh-timer
          (run-at-time
           t syncthing-auto-refresh-interval-sec
           ;; name is for filtering in timers to prevent duplicate scheduling,
           ;; object is to detectt a dangling buffer (killed) in (list-timers)
           (let ((baked-value "syncthing-timer")
                 (buf-name (buffer-name syncthing--session-buffer))
                 (buf-obj syncthing--session-buffer))
             (ignore buf-name baked-value)  ;; do not remove
             (lambda (&rest _ignore)
               (save-window-excursion
                 (if (not (buffer-name buf-obj))
                     ;; killed buffer,  cancel timer
                     (when syncthing--auto-refresh-timer
                       (cancel-timer syncthing--auto-refresh-timer)
                       (setq syncthing--auto-refresh-timer nil))
                   (switch-to-buffer (get-buffer-create buf-obj))
                   (syncthing--update)))))))))

(defun syncthing ()
  "Launch Syncthing client's instance in a new buffer."
  (interactive)
  ;; switch first, assign later, buffer-local variable gets cleared otherwise
  (switch-to-buffer
   (get-buffer-create (replace-regexp-in-string
                       "ID"
                       (number-to-string (syncthing--next-buffer-id))
                       syncthing-buffer)))
  (syncthing-mode))

(provide 'syncthing)
;;; syncthing.el ends here
