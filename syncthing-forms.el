;;; syncthing-forms.el --- Client for Syncthing -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Code:

(require 'widget)
(require 'wid-edit)
(require 'syncthing-common)
(require 'syncthing-custom)
(require 'syncthing-network)
(require 'syncthing-state)
(require 'syncthing-faces)


(defun syncthing-add-folder--submit (server form)
  "Submit 'Add Folder' form.
Argument SERVER instance of `syncthing-server'.
Argument FORM Alist mapping to the request data."
  (syncthing--request
   "POST" (format "%s/rest/config/folders" (syncthing-server-url server))
   (syncthing-server-token server)
   (json-encode-alist form)))

(defun syncthing-add-folder-interactive ()
  "Form 'Add Folder' as in Web GUI.
Interactive version picks up buffer-local value of
`syncthing-server'.  For non-interactive mode use `syncthing-add-folder'."
  (interactive)
  (unless (boundp 'syncthing-server)
    (error "Interactive form is runnable only from Syncthing buffer"))

  (syncthing-add-folder syncthing-server))

(defun syncthing-add-folder (server)
  "Form 'Add Folder' as in Web GUI.
Argument SERVER instance of `syncthing-server'."
  (let* ((align 30)
         (width 40)
         (buff (format syncthing-form-format-buffer "dummy"))
         (url-random (format "%s/rest/svc/random/string?length=10"
                             (syncthing-server-url server)))
         (resp-random (syncthing--request
                       "GET" url-random (syncthing-server-token server)))
         (folder-id-raw (downcase (alist-get 'random resp-random)))
         (folder-id (format "%s-%s"
                            (substring folder-id-raw 0 5)
                            (substring folder-id-raw 5 10)))
         ;; TODO: Move me to struct, possibly facade per schema version
         (form `((label . "")
                 (id . ,folder-id)
                 (path . "/tmp")
                 ;; (file-versioning . "No File Versioning")
                 (add-ignore-patterns . nil)
                 (fsWatcherEnabled . t)
                 (type . "sendreceive")
                 (minDiskFree . ((value . 1) (unit . "%")))
                 (syncOwnership . :json-false)
                 (sendOwnership . :json-false)
                 (rescanIntervalS . 3600)
                 (order . "random")
                 (ignorePerms . :json-false)
                 (syncXattrs . :json-false)
                 (sendXattrs . :json-false))))
    (with-current-buffer (get-buffer-create buff)
      (kill-all-local-variables)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (remove-overlays)
      (widget-insert
       (syncthing--title (format "%s Add Folder (%s)"
                                 (syncthing--fallback-ascii "folder")
                                 folder-id)))
      (insert "\n")

      (widget-insert (syncthing--bold "General"))
      (insert "\n")

      (widget-insert "Folder Label ")
      (insert (syncthing--space align))
      (widget-create 'editable-field
                     :size width
                     :value (alist-get 'label form)
                     :notify (lambda (widget &rest _)
                               (setf (alist-get 'label form)
                                     (widget-value widget))))
      (insert "\n")

      (widget-insert "Folder ID")
      (insert (syncthing--space align))
      (widget-create 'editable-field
                     :size width
                     :value (alist-get 'id form)
                     :notify (lambda (widget &rest _)
                               (setf (alist-get 'id form)
                                     (widget-value widget))))
      (insert "\n")

      (widget-insert "Folder Path")
      (insert (syncthing--space align))
      (widget-create 'directory :format "%v"
                     :size width
                     :value (alist-get 'path form)
                     :notify (lambda (widget &rest _)
                               (setf (alist-get 'path form)
                                     (widget-value widget))))
      (insert "\n\n")

      (widget-insert (syncthing--bold "Sharing"))
      (insert "\n")
      (widget-insert "N/A")
      (insert "\n\n")

      ;; TODO: Some weird zero Go values in JSON
      ;; (widget-insert (syncthing--bold "File Versioning"))
      ;; (insert "\n")
      ;; (widget-create 'menu-choice
      ;;                :tag "Select"
      ;;                :value (alist-get 'file-versioning form)
      ;;                :notify (lambda (widget &rest _)
      ;;                          (setf (alist-get 'file-versioning form)
      ;;                                (widget-value widget)))
      ;;                '(choice-item "No File Versioning")
      ;;                '(choice-item "Trash Can File Versioning")
      ;;                '(choice-item "Simple File Versioning")
      ;;                '(choice-item "Staggered File Versioning")
      ;;                '(choice-item "External File Versioning"))
      ;; (insert "\n")

      (widget-insert (syncthing--bold "Ignore Patterns"))
      (insert "\n")

      (widget-create 'checkbox
                     :value (alist-get 'add-ignore-patterns form)
                     :notify (lambda (widget &rest _)
                               (setf (alist-get 'add-ignore-patterns form)
                                     (if (widget-value widget)
                                         t :json-false))))
      (widget-insert " Add ignore patterns")
      (insert "\n\n")

      (widget-insert (syncthing--bold "Advanced"))
      (insert "\n")

      (widget-create 'checkbox
                     :value (alist-get 'fsWatcherEnabled form)
                     :notify (lambda (widget &rest _)
                               (setf (alist-get 'fsWatcherEnabled form)
                                     (if (widget-value widget)
                                         t :json-false))))
      (widget-insert " Watch for Changes")
      (insert "\n\n")

      (widget-insert "Folder Type")
      (insert "\n")

      (widget-create 'menu-choice
                     :tag "Select"
                     :value (alist-get 'type form)
                     :notify (lambda (widget &rest _)
                               (setf (alist-get 'type form)
                                     (widget-value widget)))
                     '(choice-item :tag "Send & Receive" :value "sendreceive")
                     '(choice-item :tag "Send Only" :value "sendonly")
                     '(choice-item :tag "Receive Only" :value "receiveonly")
                     '(choice-item :tag "Receive Encrypted"
                                   :value "receiveencrypted"))
      (insert "\n")

      (widget-insert "Minimum Free Disk Space")
      (insert (syncthing--space align))
      (widget-create 'number :format "%v"
                     :size width
                     :value (alist-get 'value (alist-get 'minDiskFree form))
                     :notify (lambda (widget &rest _)
                               (setf (alist-get 'value
                                                (alist-get 'minDiskFree form))
                                     (widget-value widget))))
      (insert "\n")

      (widget-create 'menu-choice
                     :tag "Select"
                     :value (alist-get 'unit (alist-get 'minDiskFree form))
                     :notify (lambda (widget &rest _)
                               (setf (alist-get 'unit
                                                (alist-get 'minDiskFree form))
                                     (widget-value widget)))
                     '(choice-item "%")
                     '(choice-item "kB")
                     '(choice-item "MB")
                     '(choice-item "GB")
                     '(choice-item "TB"))
      (insert "\n")

      (widget-insert "Ownership")
      (insert "\n")
      (widget-create 'checkbox
                     :value (alist-get 'syncOwnership form)
                     :notify (lambda (widget &rest _)
                               (setf (alist-get 'syncOwnership form)
                                     (if (widget-value widget)
                                         t :json-false))))
      (widget-insert " Sync Ownership")
      (insert "\n")

      (widget-create 'checkbox
                     :value (alist-get 'sendOwnership form)
                     :notify (lambda (widget &rest _)
                               (setf (alist-get 'sendOwnership form)
                                     (if (widget-value widget)
                                         t :json-false))))
      (widget-insert " Send Ownership")
      (insert "\n\n")

      (widget-insert "Full Rescan Interval (s)")
      (insert (syncthing--space align))
      (widget-create 'number :format "%v"
                     :size width
                     :value (alist-get 'rescanIntervalS form)
                     :notify (lambda (widget &rest _)
                               (setf (alist-get 'rescanIntervalS form)
                                     (widget-value widget))))
      (insert "\n\n")

      (widget-insert "File Pull Order")
      (insert "\n")

      (widget-create 'menu-choice
                     :tag "Select"
                     :value (alist-get 'order form)
                     :notify (lambda (widget &rest _)
                               (setf (alist-get 'order form)
                                     (widget-value widget)))
                     '(choice-item :tag "Random" :value "random")
                     '(choice-item :tag "Alphabetic" :value "alphabetic")
                     '(choice-item :tag "Smallest First"
                                   :value "smallestFirst")
                     '(choice-item :tag "Largest First" :value "largestFirst")
                     '(choice-item :tag "Oldest First" :value "oldestFirst")
                     '(choice-item :tag "Newest First" :value "newestFirst"))
      (insert "\n")

      (widget-create 'checkbox
                     :value (alist-get 'ignorePerms form)
                     :notify (lambda (widget &rest _)
                               (setf (alist-get 'ignorePerms form)
                                     (if (widget-value widget)
                                         t :json-false))))
      (widget-insert " Ignore Permissions")
      (insert "\n\n")

      (widget-insert "Extended Attributes")
      (insert "\n")

      (widget-create 'checkbox
                     :value (alist-get 'syncXattrs form)
                     :notify (lambda (widget &rest _)
                               (setf (alist-get 'syncXattrs form)
                                     (if (widget-value widget)
                                         t :json-false))))
      (widget-insert " Sync Extended Attributes")
      (insert "\n")

      (widget-create 'checkbox
                     :value (alist-get 'sendXattrs form)
                     :notify (lambda (widget &rest _)
                               (setf (alist-get 'sendXattrs form)
                                     (if (widget-value widget)
                                         t :json-false))))
      (widget-insert " Send Extended Attributes")
      (insert "\n\n")

      (widget-create 'push-button
                     :notify (lambda (&rest _)
                               (syncthing-add-folder--submit server form))
                     "Save")
      (widget-insert " ")
      (widget-create 'push-button "Close")
      (use-local-map widget-keymap)
      (widget-setup))
    (pop-to-buffer-same-window buff)))


(provide 'syncthing-forms)
;;; syncthing-forms.el ends here
