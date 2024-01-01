;;; syncthing-faces.el --- Client for Syncthing -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Code:

(require 'syncthing-common)


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

(defun syncthing--title (text)
  "Format TEXT as title."
  (syncthing-trace)
  (propertize text 'face 'syncthing-title))

(defun syncthing--prop (text)
  "Format TEXT as property."
  (syncthing-trace)
  (propertize text 'face 'syncthing-prop))

(defun syncthing--bold (text)
  "Format TEXT as bold."
  (syncthing-trace)
  (propertize text 'face 'syncthing-bold))

(defun syncthing--italic (text)
  "Format TEXT as italic."
  (syncthing-trace)
  (propertize text 'face 'syncthing-italic))

(defun syncthing--rate-download (text)
  "Format TEXT as download rate."
  (syncthing-trace)
  (propertize text 'face 'syncthing-rate-download))

(defun syncthing--rate-upload (text)
  "Format TEXT as upload rate."
  (syncthing-trace)
  (propertize text 'face 'syncthing-progress-75))

(defun syncthing--count-local-files (text)
  "Format TEXT as local files count."
  (syncthing-trace)
  (propertize text 'face 'syncthing-count-local-files))

(defun syncthing--count-local-folders (text)
  "Format TEXT as local folders count."
  (syncthing-trace)
  (propertize text 'face 'syncthing-count-local-folders))

(defun syncthing--count-local-bytes (text)
  "Format TEXT as local bytes count."
  (syncthing-trace)
  (propertize text 'face 'syncthing-count-local-bytes))

(defun syncthing--count-listeners (text)
  "Format TEXT as listeners count."
  (syncthing-trace)
  (propertize text 'face 'syncthing-count-listeners))

(defun syncthing--count-discovery (text)
  "Format TEXT as discovery count."
  (syncthing-trace)
  (propertize text 'face 'syncthing-count-discovery))

(defun syncthing--uptime (text)
  "Format TEXT as uptime."
  (syncthing-trace)
  (propertize text 'face 'syncthing-uptime))

(defun syncthing--my-id (text)
  "Format TEXT as Syncthing ID."
  (syncthing-trace)
  (propertize text 'face 'syncthing-my-id))

(provide 'syncthing-faces)
;;; syncthing-faces.el ends here
