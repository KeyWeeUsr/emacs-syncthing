;;; syncthing.el --- Emacs client for Syncthing -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Peter Badida

;; Author: Peter Badida <keyweeusr@gmail.com>
;; Keywords: convenience, syncthing, sync, client, view
;; Version: 1.1.0
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

;; customization values
(defcustom syncthing-buffer
  "*syncthing(ADDR)*"
  "Syncthing's buffer name with special =ADDR= placeholder."
  :group 'syncthing-startup
  :type 'string)

(defcustom syncthing-base-url
  "https://127.0.0.1:8384"
  "Base URL for Syncthing REST API endpoint."
  :group 'syncthing
  :type 'string)

(defcustom syncthing-format-perc
  "%6.2f%%"
  "Format for displaying process percentage."
  :group 'syncthing
  :type 'string)

(defcustom syncthing-cleanup-priority
  0
  "`add-hook' priority."
  :group 'syncthing
  :type 'number)

(defcustom syncthing-token
  nil
  "Syncthing REST API token."
  :group 'syncthing
  :type 'string)

(defcustom syncthing-start-collapsed
  t
  "Start all items collapsed."
  :group 'syncthing-startup
  :type 'boolean)

(defcustom syncthing-start-with-auto-refresh
  t
  "Start with auto-refresh enabled."
  :group 'syncthing-startup
  :type 'boolean)

(defcustom syncthing-auto-refresh-interval
  10
  "Number of seconds to wait before refreshing client buffer."
  :group 'syncthing
  :type 'number)

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

(defface syncthing-progress-100
  '((((class color) (background dark))
     (:foreground "green"))
    (((class color) (background light))
     (:foreground "green"))
    (t (:foreground "green")))
  "Face for 100% progress."
  :group 'syncthing-faces)

(defface syncthing-progress-75
  '((((class color) (background dark))
     (:foreground "lightgreen"))
    (((class color) (background light))
     (:foreground "lightgreen"))
    (t (:foreground "lightgreen")))
  "Face for 75%-100% progress."
  :group 'syncthing-faces)

(defface syncthing-progress-50
  '((((class color) (background dark))
     (:foreground "yellow"))
    (((class color) (background light))
     (:foreground "yellow"))
    (t (:foreground "yellow")))
  "Face for 50%-75% progress."
  :group 'syncthing-faces)

(defface syncthing-progress-25
  '((((class color) (background dark))
     (:foreground "orange"))
    (((class color) (background light))
     (:foreground "orange"))
    (t (:foreground "orange")))
  "Face for 25%-50% progress."
  :group 'syncthing-faces)

(defface syncthing-progress-0
  '((((class color) (background dark))
     (:foreground "red"))
    (((class color) (background light))
     (:foreground "red"))
    (t (:foreground "red")))
  "Face for 0%-25% progress."
  :group 'syncthing-faces)

(defface syncthing-rate-download
  '((((class color) (background dark))
     (:foreground "deep sky blue"))
    (((class color) (background light))
     (:foreground "deep sky blue"))
    (t (:foreground "deep sky blue")))
  "Face for current download rate."
  :group 'syncthing-faces)

(defface syncthing-rate-upload
  '((((class color) (background dark))
     (:foreground "lightgreen"))
    (((class color) (background light))
     (:foreground "lightgreen"))
    (t (:foreground "lightgreen")))
  "Face for current upload rate."
  :group 'syncthing-faces)

(defface syncthing-count-local-files
  '((((class color) (background dark))
     (:foreground "white"))
    (((class color) (background light))
     (:foreground "white"))
    (t (:foreground "white")))
  "Face for local files counter."
  :group 'syncthing-faces)

(defface syncthing-count-local-folders
  '((((class color) (background dark))
     (:foreground "yellow"))
    (((class color) (background light))
     (:foreground "yellow"))
    (t (:foreground "yellow")))
  "Face for local folder counter."
  :group 'syncthing-faces)

(defface syncthing-count-local-bytes
  '((((class color) (background dark))
     (:foreground "light sea green"))
    (((class color) (background light))
     (:foreground "light sea green"))
    (t (:foreground "light sea green")))
  "Face for local bytes counter."
  :group 'syncthing-faces)

(defface syncthing-count-listeners
  '((((class color) (background dark))
     (:foreground "green"))
    (((class color) (background light))
     (:foreground "green"))
    (t (:foreground "green")))
  "Face for listeners' counter."
  :group 'syncthing-faces)

(defface syncthing-count-discovery
  '((((class color) (background dark))
     (:foreground "steel blue"))
    (((class color) (background light))
     (:foreground "steel blue"))
    (t (:foreground "steel blue")))
  "Face for discovery counter."
  :group 'syncthing-faces)

(defface syncthing-uptime
  '((((class color) (background dark))
     (:foreground "orchid"))
    (((class color) (background light))
     (:foreground "orchid"))
    (t (:foreground "orchid")))
  "Face for uptime counter."
  :group 'syncthing-faces)

(defface syncthing-my-id
  '((((class color) (background dark))
     (:foreground "#3498db"))
    (((class color) (background light))
     (:foreground "#3498db"))
    (t (:foreground "#3498db")))
  "Face for this device's ID."
  :group 'syncthing-faces)

;; local/state variables
(defvar-local syncthing-session-base-url
  ""
  "Tmp to hold session's base URL.")

(defvar-local syncthing--state-session-buffer
  ""
  "Tmp to hold session's buffer instance/object.")

(defvar-local syncthing--state-fold-folders
  nil
  "Tmp list to hold IDs of folds.")

(defvar-local syncthing--state-fold-devices
  nil
  "Tmp list to hold IDs of folds.")

(defvar-local syncthing--state-collapse-after-start
  nil
  "Tmp to hold collapse toggle.")

(defvar-local syncthing--state-count-local-files
  0
  "Tmp to hold local state.")

(defvar-local syncthing--state-count-local-folders
  0
  "Tmp to hold local state.")

(defvar-local syncthing--state-count-local-bytes
  0
  "Tmp to hold local state.")

(defvar-local syncthing--state-version
  ""
  "Tmp to hold local state.")

(defvar-local syncthing--state-my-id
  ""
  "Tmp to hold local state.")

(defvar-local syncthing--state-uptime
  0
  "Tmp to hold local state.")

(defvar-local syncthing--state-auto-refresh
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

(defun syncthing--rate-download (text)
  "Format TEXT as download rate."
  (propertize text 'face 'syncthing-rate-download))

(defun syncthing--rate-upload (text)
  "Format TEXT as upload rate."
  (propertize text 'face 'syncthing-progress-75))

(defun syncthing--count-local-files (text)
  "Format TEXT as local files count."
  (propertize text 'face 'syncthing-count-local-files))

(defun syncthing--count-local-folders (text)
  "Format TEXT as local folders count."
  (propertize text 'face 'syncthing-count-local-folders))

(defun syncthing--count-local-bytes (text)
  "Format TEXT as local bytes count."
  (propertize text 'face 'syncthing-count-local-bytes))

(defun syncthing--count-listeners (text)
  "Format TEXT as listeners count."
  (propertize text 'face 'syncthing-count-listeners))

(defun syncthing--count-discovery (text)
  "Format TEXT as discovery count."
  (propertize text 'face 'syncthing-count-discovery))

(defun syncthing--uptime (text)
  "Format TEXT as uptime."
  (propertize text 'face 'syncthing-uptime))

(defun syncthing--my-id (text)
  "Format TEXT as Syncthing ID."
  (propertize text 'face 'syncthing-my-id))

(defun syncthing--list ()
  "List all resources."
  (let-alist (syncthing--request
              "GET" (syncthing--url "rest/config"))
    (cond ((eq .version 37)
           (save-window-excursion
             (switch-to-buffer (get-buffer-create syncthing--state-session-buffer))
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
             (switch-to-buffer (get-buffer-create syncthing--state-session-buffer))
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
             (when syncthing--state-collapse-after-start
               (push id syncthing--state-fold-folders)))
            ((string-equal "type" (car item))
             (setq type (cdr item)))
            ((string-equal "path" (car item))
             (setq path (cdr item)))
            ((string-equal "devices" (car item))
             (setq devices (cdr item)))))
    (let-alist (syncthing--request
                "GET" (syncthing--url
                       (format "rest/db/status?folder=%s" id)))
      (setq syncthing--state-count-local-files
            (+ syncthing--state-count-local-files .localFiles))
      (setq syncthing--state-count-local-bytes
            (+ syncthing--state-count-local-bytes .localBytes))
      (setq syncthing--state-count-local-folders
            (+ syncthing--state-count-local-folders .localDirectories)))
    (dolist (item (syncthing--request
                   "GET" (syncthing--url
                          (format "rest/db/completion?folder=%s" id))))
      (cond ((string-equal "completion" (car item))
             (setq perc (cdr item)))))
    (save-window-excursion
      (switch-to-buffer (get-buffer-create syncthing--state-session-buffer))
      (widget-create
       'push-button
       :button-face "menu"
       :button-suffix
       (concat
        " "
        (syncthing--color-perc perc)
        (syncthing--bold (format " %s\n" name))
        (unless (member id syncthing--state-fold-folders)
          (syncthing--prop (format "\t%s\n\t%s\n\t%s\n\t%s\n"
                                   id type path devices))))
       :action
       (lambda (&rest _event)
         (if (member id syncthing--state-fold-folders)
             (progn
               (setq syncthing--state-fold-folders
                     (delete id syncthing--state-fold-folders))
               (save-excursion
                 (widget-delete (syncthing--get-widget (point)))
                 (syncthing--list-37-folder folder)))
           (progn
             (if syncthing--state-fold-folders
                 (push id syncthing--state-fold-folders)
               (setq syncthing--state-fold-folders (list id)))
             (save-excursion
               (widget-delete (syncthing--get-widget (point)))
               (syncthing--list-37-folder folder)))))
       (if (member id syncthing--state-fold-folders)
           (syncthing--bold ">")
         (syncthing--bold "v"))))))

(defun syncthing--color-perc (perc)
  "Colorize PERC float."
  (propertize
   (format syncthing-format-perc perc)
   'face
   (cond ((< perc 25)
          'syncthing-progress-0)
         ((and (>= perc 25) (< perc 50))
          'syncthing-progress-25)
         ((and (>= perc 50) (< perc 75))
          'syncthing-progress-50)
         ((and (>= perc 75) (< perc 100))
          'syncthing-progress-75)
         ((>= perc 100)
          'syncthing-progress-100))))

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
             (when syncthing--state-collapse-after-start
               (push id syncthing--state-fold-devices)))
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
      (switch-to-buffer (get-buffer-create syncthing--state-session-buffer))
      (widget-create
       'push-button
       :button-face "menu"
       :button-suffix
       (concat
        " "
        (syncthing--color-perc perc)
        (syncthing--bold (format " %s\n" name))
        (unless (member id syncthing--state-fold-devices)
          (syncthing--prop (format "\t%s\n\t%s\n\t%s\n"
                                   id paused addresses))))
       :action
       (lambda (&rest _event)
         (if (member id syncthing--state-fold-devices)
             (progn
               (setq syncthing--state-fold-devices
                     (delete id syncthing--state-fold-devices))
               (save-excursion
                 (widget-delete (syncthing--get-widget (point)))
                 (syncthing--list-37-device device)))
           (progn
             (if syncthing--state-fold-devices
                 (push id syncthing--state-fold-devices)
               (setq syncthing--state-fold-devices (list id)))
             (save-excursion
               (widget-delete (syncthing--get-widget (point)))
               (syncthing--list-37-device device)))))
       (if (member id syncthing--state-fold-devices)
           (syncthing--bold ">")
         (syncthing--bold "v"))))))

(defun syncthing--draw ()
  "Setup buffer and draw widgets."
  (syncthing--list)
  (save-window-excursion
    (switch-to-buffer (get-buffer-create syncthing--state-session-buffer))
    (widget-setup)
    (let-alist (syncthing--request
                "GET" (syncthing--url "rest/system/version"))
      (setq syncthing--state-version .version))
    (let-alist (syncthing--request
                "GET" (syncthing--url "rest/system/status"))
      (setq syncthing--state-my-id
            (substring .myID 0 6))
      (setq syncthing--state-uptime .uptime))
    (setq header-line-format
          (concat
           " "
           (syncthing--rate-download " 0B/s")
           " "
           (syncthing--rate-upload " 0B/s")
           " "
           (syncthing--count-local-files
            (format " %d" syncthing--state-count-local-files))
           " "
           (syncthing--count-local-folders
            (format " %d" syncthing--state-count-local-folders))
           " "
           (syncthing--count-local-bytes
            (format " ~%.1fGiB"
                    (/ syncthing--state-count-local-bytes
                       (* 1024.0 1024.0 1024.0))))
           " "
           (syncthing--count-listeners " 3/3")
           " "
           (syncthing--count-discovery " 4/5")
           " "
           (syncthing--uptime
            (format " %dd %dh %dm"
                    0
                    (/ syncthing--state-uptime 3600)
                    (* 60 (- (/ syncthing--state-uptime 3600.0)
                             (/ syncthing--state-uptime 3600)))))
           " "  ;; bad glyph! :(
           (syncthing--my-id (format " %s" syncthing--state-my-id))
           " "
           (format " %s" syncthing--state-version)))
    ;; messes up with cursor position, reset to 0,0
    (goto-char 0)))

(defun syncthing--init-state ()
  "Reset all variables holding initial state.
Optional argument SKIP-CANCEL Skip removing auto-refresh."
  ;; everything += or appendable has to reset in each update
  (setq syncthing--state-fold-folders (list))
  (setq syncthing--state-fold-devices (list))
  (setq syncthing--state-collapse-after-start
        syncthing-start-collapsed)
  (setq syncthing--state-count-local-files 0)
  (setq syncthing--state-count-local-folders 0)
  (setq syncthing--state-count-local-bytes 0)
  (setq syncthing--state-version "")
  (setq syncthing--state-uptime 0))

(defun syncthing--update (&rest _)
  "Update function for every refresh iteration."
  (let ((inhibit-read-only t))
    (erase-buffer))

  (syncthing--init-state)
  (syncthing--draw)
  (setq syncthing--state-collapse-after-start nil)
  (switch-to-buffer (get-buffer-create syncthing--state-session-buffer)))

;; modes for client's session buffer(s)
(define-derived-mode syncthing-mode special-mode "Syncthing"
  "Launch Syncthing client in the current window."
  :group 'syncthing
  ;; current buffer, new one is created via `(syncthing)'
  ;;
  ;; make sure it's initialized only once, otherwise (current-buffer) fetches
  ;; value from any other window currently in focus causing a bit of a mess
  (when (string-equal "" syncthing--state-session-buffer)
    (setq syncthing--state-session-buffer (current-buffer)))

  ;; custom handler for RET / widget input handler
  ;; (keymap-local-set "RET" #'syncthing--newline)
  ;; (keymap-local-set "?" #'describe-bindings)
  (use-local-map syncthing-mode-map)

  ;; Hook to auto-revert mode for refreshing
  (setq-local revert-buffer-function #'syncthing--update)

  (syncthing--update)
  (when syncthing-start-with-auto-refresh
    (syncthing-auto-refresh-mode 1)))

(define-minor-mode syncthing-auto-refresh-mode
  "Refresh client view every `syncthing-auto-refresh-interval' seconds."
  :lighter " Auto-refresh"
  (unless (derived-mode-p 'syncthing-mode)
    (user-error "Buffer not in `syncthing-mode'"))
  (setq-local
   buffer-stale-function
   (when syncthing-auto-refresh-mode #'(lambda (&rest _) t))
   auto-revert-interval
   (when syncthing-auto-refresh-mode syncthing-auto-refresh-interval))
  (auto-revert-mode (if syncthing-auto-refresh-mode 1 -1)))

(defun syncthing--switch-to-new-buffer (base-url)
  "Create buffer name from BASE-URL."
  (switch-to-buffer
   (get-buffer-create
    (generate-new-buffer
     (replace-regexp-in-string
      "ADDR" base-url
      syncthing-buffer
      t)))))

(defun syncthing-with-base (base-url)
  "Launch Syncthing client's instance for BASE-URL in a new buffer."
  (interactive "sSyncthing REST API base URL: ")
  (syncthing--switch-to-new-buffer base-url)
  (syncthing-mode))

(defun syncthing ()
  "Launch Syncthing client's instance in a new buffer."
  (interactive)
  ;; switch first, assign later, buffer-local variable gets cleared otherwise
  (syncthing--switch-to-new-buffer syncthing-base-url)
  (syncthing-mode))

(provide 'syncthing)
;;; syncthing.el ends here
