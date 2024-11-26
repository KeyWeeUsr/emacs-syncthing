;;; syncthing-keyboard.el --- Client for Syncthing -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Code:

(require 'wid-edit)

(require 'syncthing-common)
(require 'syncthing-draw)
(require 'syncthing-custom)


(defvar-local syncthing-keyboard-map
  (let ((map (make-keymap)))
    (set-keymap-parent map widget-keymap)

    ;; custom handler for RET / widget input handler
    (define-key map (kbd "RET") #'syncthing--newline)
    (define-key map (kbd "SPC") #'syncthing--newline)

    ;; X11
    (when (eq window-system 'x)
      (define-key function-key-map
        [(control shift iso-lefttab)] [(control shift tab)])
      (define-key function-key-map
        [(meta shift iso-lefttab)] [(meta shift tab)])
      (define-key function-key-map
        [(meta control shift iso-lefttab)] [(meta control shift tab)]))

    (if syncthing-old-tab-behavior
        (progn
          (define-key map (kbd "C-<tab>") nil)
          (define-key map (kbd "C-<backtab>") nil)
          (define-key map (kbd "TAB") #'syncthing--newline)
          (define-key map (kbd "<backtab>") #'syncthing--tab))
      (define-key map (kbd "C-<tab>") #'syncthing--newline)
      (define-key map (kbd "C-<backtab>") #'syncthing--tab)
      (define-key map (kbd "TAB") #'widget-forward)
      (define-key map (kbd "<backtab>") #'widget-backward))
    (define-key map (kbd "<mouse-1>") #'syncthing--newline)
    (define-key map (kbd "?") #'describe-bindings)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    map)
  "`syncthing-mode' key map.

* `RET'/`SPC'/`TAB' when `point' is on a foldable widget toggles the fold.
* `<backtab>' or Shift+`TAB' toggles all foldable widgets.
* `n'/`p' for navigation down/up in the buffer.")

(defun syncthing--newline (pos &optional event)
  "RET/Enter/newline-keypress handler.
Argument POS Incoming EVENT position."
  (interactive "@d")
  (syncthing-trace)
  (let ((button (syncthing--get-widget pos)))
    (if button
	    (widget-apply-action button event)
      (message "%s: %s" syncthing-prefix syncthing-msg-cant-edit-buffer))))

(defun syncthing--tab (&rest _)
  "TAB handler.
Argument POS Incoming EVENT position."
  (interactive "@d")
  (syncthing-trace)
  (let* ((data (syncthing-server-data syncthing-server))
         (folders (alist-get 'folders data))
         (devices (alist-get 'devices data))
         (fold-folders (syncthing-buffer-fold-folders syncthing-buffer))
         (fold-devices (syncthing-buffer-fold-devices syncthing-buffer))
         (skip-folders (syncthing-buffer-skip-fold-folders syncthing-buffer))
         (skip-devices (syncthing-buffer-skip-fold-devices syncthing-buffer)))
    (if (eq (length skip-folders) (length folders))
        (progn (setf skip-folders nil)
               (setf fold-folders nil)
               (dolist (folder folders)
                 (push (alist-get 'id folder) fold-folders)))
      (setf skip-folders nil)
      (setf fold-folders nil)
      (dolist (folder folders)
        (push (alist-get 'id folder) skip-folders)))
    (setf (syncthing-buffer-fold-folders syncthing-buffer) fold-folders
          (syncthing-buffer-skip-fold-folders syncthing-buffer) skip-folders)

    (if (eq (length skip-devices) (length devices))
        (progn (setf skip-devices nil)
               (setf fold-devices nil)
               (dolist (device devices)
                 (push (alist-get 'deviceID device) fold-devices)))
      (setf skip-devices nil)
      (setf fold-devices nil)
      (dolist (device devices)
        (push (alist-get 'deviceID device) skip-devices)))
    (setf (syncthing-buffer-fold-devices syncthing-buffer) fold-devices
          (syncthing-buffer-skip-fold-devices syncthing-buffer) skip-devices))
  (syncthing--draw syncthing-server))

(provide 'syncthing-keyboard)
;;; syncthing-keyboard.el ends here
