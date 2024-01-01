;;; syncthing-keyboard-tests.el -- tests -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Code:

(require 'ert)
(require 'widget)

(require 'syncthing-common)
(require 'syncthing-constants)
(require 'syncthing-errors)
(require 'syncthing-keyboard)


(defun syncthing-ert-cleanup ()
  (should (eq nil syncthing--servers))
  (dolist (buf (buffer-list))
    (unless (or (string-match-p (regexp-quote "*Messages*") (buffer-name buf)))
      (kill-buffer buf))))

(ert-deftest syncthing-keyboard-newline-apply ()
  "Call widget's `:action' when `point' is on top of it."
  (syncthing-ert-cleanup)
  (with-temp-buffer
    (let (called)
      (widget-create 'push-button :action (lambda (&rest _) (setq called t)))
      (widget-setup)
      (should (string= "[]" (buffer-string)))
      (syncthing--newline 1)  ; Move into the button area first
      (should called))))

(ert-deftest syncthing-keyboard-newline-apply-non-widget ()
  "Throw an error when interacting with a non-widget."
  (syncthing-ert-cleanup)
  (with-temp-buffer
    (let (called args)
      (advice-add 'message
                  :override
                  (lambda (&rest largs) (setq called t) (setq args largs)))
      (syncthing--newline 1)  ; Move into the button area first
      (advice-remove 'message
                     (lambda (&rest largs) (setq called t) (setq args largs)))
      (should called)
      (should (string= (caddr args) syncthing-msg-cant-edit-buffer))
      (should called))))

(ert-deftest syncthing-keyboard-tab-fold-all ()
  "Fold all foldable widgets."
  (syncthing-ert-cleanup)
  (with-temp-buffer
    (let* ((skip-folders '(a b c))
           (skip-devices '(x y z))
           (syncthing-server
            (syncthing--server
             :data (list (cons 'folders (list (list (cons 'id "a"))
                                              (list (cons 'id "b"))
                                              (list (cons 'id "c"))))
                         (cons 'devices (list (list (cons 'deviceID "x"))
                                              (list (cons 'deviceID "y"))
                                              (list (cons 'deviceID "z")))))))
           (syncthing-buffer
            (syncthing--buffer :fold-folders nil
                               :fold-devices nil
                               :skip-fold-folders skip-folders
                               :skip-fold-devices skip-devices))
           redrawn)
      (should (null (syncthing-buffer-fold-folders syncthing-buffer)))
      (should (null (syncthing-buffer-fold-devices syncthing-buffer)))
      (should (string= (format "%s" (syncthing-buffer-skip-fold-folders
                                     syncthing-buffer))
                       (format "%s" skip-folders)))
      (should (string= (format "%s" (syncthing-buffer-skip-fold-devices
                                     syncthing-buffer))
                       (format "%s" skip-devices)))
      (advice-add 'syncthing--draw
                  :override
                  (lambda (&rest _) (setq redrawn t)))
      (syncthing--tab)
      (should (string= (format "%s" (syncthing-buffer-fold-folders
                                     syncthing-buffer))
                       (format "%s" (reverse skip-folders))))
      (should (string= (format "%s" (syncthing-buffer-fold-devices
                                     syncthing-buffer))
                       (format "%s" (reverse skip-devices))))
      (should (null (syncthing-buffer-skip-fold-folders syncthing-buffer)))
      (should (null (syncthing-buffer-skip-fold-devices syncthing-buffer)))
      (advice-remove 'syncthing--draw (lambda (&rest _) (setq redrawn t)))
      (should redrawn))))

(ert-deftest syncthing-keyboard-tab-unfold-all ()
  "Unfold all foldable widgets."
  (syncthing-ert-cleanup)
  (with-temp-buffer
    (let* ((skip-folders '(a b c))
           (skip-devices '(x y z))
           (syncthing-server
            (syncthing--server
             :data (list (cons 'folders (list (list (cons 'id "a"))
                                              (list (cons 'id "b"))
                                              (list (cons 'id "c"))))
                         (cons 'devices (list (list (cons 'deviceID "x"))
                                              (list (cons 'deviceID "y"))
                                              (list (cons 'deviceID "z")))))))
           (syncthing-buffer
            (syncthing--buffer :fold-folders skip-folders
                               :fold-devices skip-devices
                               :skip-fold-folders '(a)
                               :skip-fold-devices '(x)))
           redrawn)
      (should-not (null (syncthing-buffer-fold-folders syncthing-buffer)))
      (should-not (null (syncthing-buffer-fold-devices syncthing-buffer)))
      (should (string= (format "%s" (syncthing-buffer-fold-folders
                                     syncthing-buffer))
                       (format "%s" skip-folders)))
      (should (string= (format "%s" (syncthing-buffer-fold-devices
                                     syncthing-buffer))
                       (format "%s" skip-devices)))
      (advice-add 'syncthing--draw
                  :override
                  (lambda (&rest _) (setq redrawn t)))
      (syncthing--tab)
      (should (string= (format "%s" (syncthing-buffer-skip-fold-folders
                                     syncthing-buffer))
                       (format "%s" (reverse skip-folders))))
      (should (string= (format "%s" (syncthing-buffer-skip-fold-devices
                                     syncthing-buffer))
                       (format "%s" (reverse skip-devices))))
      (should (null (syncthing-buffer-fold-folders syncthing-buffer)))
      (should (null (syncthing-buffer-fold-devices syncthing-buffer)))
      (advice-remove 'syncthing--draw (lambda (&rest _) (setq redrawn t)))
      (should redrawn))))

(ert-deftest syncthing-keyboard-tab-fold-folders-unfold-devices ()
  "Fold all folders if some are open, unfold all devices when all are fold."
  (syncthing-ert-cleanup)
  (with-temp-buffer
    (let* ((skip-folders '(a b c))
           (skip-devices '(x y z))
           (syncthing-server
            (syncthing--server
             :data (list (cons 'folders (list (list (cons 'id "a"))
                                              (list (cons 'id "b"))
                                              (list (cons 'id "c"))))
                         (cons 'devices (list (list (cons 'deviceID "x"))
                                              (list (cons 'deviceID "y"))
                                              (list (cons 'deviceID "z")))))))
           (syncthing-buffer
            (syncthing--buffer :fold-folders nil
                               :fold-devices skip-devices
                               :skip-fold-folders skip-folders
                               :skip-fold-devices '(x)))
           redrawn)
      (should (null (syncthing-buffer-fold-folders syncthing-buffer)))
      (should (string= (format "%s" (syncthing-buffer-fold-devices
                                     syncthing-buffer))
                       (format "%s" skip-devices)))
      (should (string= (format "%s" (syncthing-buffer-skip-fold-folders
                                     syncthing-buffer))
                       (format "%s" skip-folders)))
      (should (string= (format "%s" (syncthing-buffer-skip-fold-devices
                                     syncthing-buffer))
                       (format "%s" '(x))))
      (advice-add 'syncthing--draw
                  :override
                  (lambda (&rest _) (setq redrawn t)))
      (syncthing--tab)
      (should (string= (format "%s" (syncthing-buffer-fold-folders
                                     syncthing-buffer))
                       (format "%s" (reverse skip-folders))))
      (should (null (syncthing-buffer-fold-devices syncthing-buffer)))
      (should (null (syncthing-buffer-skip-fold-folders syncthing-buffer)))
      (should (string= (format "%s" (syncthing-buffer-skip-fold-devices
                                     syncthing-buffer))
                       (format "%s" (reverse skip-devices))))
      (advice-remove 'syncthing--draw (lambda (&rest _) (setq redrawn t)))
      (should redrawn))))

(ert-deftest syncthing-keyboard-tab-unfold-folders-fold-devices ()
  "Unfold all folders if all are fold, fold all devices when some are open."
  (syncthing-ert-cleanup)
  (with-temp-buffer
    (let* ((skip-folders '(a b c))
           (skip-devices '(x y z))
           (syncthing-server
            (syncthing--server
             :data (list (cons 'folders (list (list (cons 'id "a"))
                                              (list (cons 'id "b"))
                                              (list (cons 'id "c"))))
                         (cons 'devices (list (list (cons 'deviceID "x"))
                                              (list (cons 'deviceID "y"))
                                              (list (cons 'deviceID "z")))))))
           (syncthing-buffer
            (syncthing--buffer :fold-folders skip-folders
                               :fold-devices nil
                               :skip-fold-folders '(a)
                               :skip-fold-devices skip-devices))
           redrawn)
      (should (string= (format "%s" (syncthing-buffer-fold-folders
                                     syncthing-buffer))
                       (format "%s" skip-folders)))
      (should (null (syncthing-buffer-fold-devices syncthing-buffer)))
      (should (string= (format "%s" (syncthing-buffer-skip-fold-folders
                                     syncthing-buffer))
                       (format "%s" '(a))))
      (should (string= (format "%s" (syncthing-buffer-skip-fold-devices
                                     syncthing-buffer))
                       (format "%s" skip-devices)))
      (advice-add 'syncthing--draw
                  :override
                  (lambda (&rest _) (setq redrawn t)))
      (syncthing--tab)
      (should (null (syncthing-buffer-fold-folders syncthing-buffer)))
      (should (string= (format "%s" (syncthing-buffer-fold-devices
                                     syncthing-buffer))
                       (format "%s" (reverse skip-devices))))
      (should (string= (format "%s" (syncthing-buffer-skip-fold-folders
                                     syncthing-buffer))
                       (format "%s" (reverse skip-folders))))
      (should (null (syncthing-buffer-skip-fold-devices syncthing-buffer)))
      (advice-remove 'syncthing--draw (lambda (&rest _) (setq redrawn t)))
      (should redrawn))))

(provide 'syncthing-keyboard-tests)
;;; syncthing-keyboard-tests.el ends here
