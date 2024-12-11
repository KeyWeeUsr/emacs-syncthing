;;; syncthing-common-tests.el -- tests -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Code:

(require 'ert)

(require 'syncthing-common)
(require 'syncthing-custom)
(require 'syncthing-keyboard)
(require 'syncthing-state)


(defun syncthing-ert-cleanup ()
  (should (eq nil syncthing--servers))
  (mapc
   (lambda (buff)
     (unless
         (or (string-match-p (regexp-quote "*Messages*") (buffer-name buff))
             (string-match-p (regexp-quote " *code-conversion-work*")
                             (buffer-name buff))
             (string-match-p (regexp-quote " *code-converting-work*")
                             (buffer-name buff))
             (string-match-p "syncthing.*\\.el" (buffer-name buff)))
       (kill-buffer buff)))
   (buffer-list)))

(ert-deftest syncthing-alist-var-access ()
  "Convert 'a' (string) to 'a (symbol) and access value with alist-get."
  (syncthing-ert-cleanup)
  (let ((data '((key . 1) (key2 . 2)))
        (var 'key)
        (var-str "key"))
    (should (string= "((key . 1) (key2 . 2))" (format "%s" data)))
    (should (string= "cons" (format "%s" (type-of data))))
    (should (eq 1 (alist-get 'key data)))
    (should (eq 1 (alist-get `,var data)))
    (should (eq 'key (intern `,var-str)))
    (should (eq var (intern `,var-str)))
    (should (eq 1 (alist-get (intern `,var-str) data)))))

(defun dummy (num str sym lst cns &optional opt &key opt-key)
  (syncthing-trace)
  (ignore num str sym lst cns opt opt-key))

(ert-deftest syncthing-trace-get-prev-func ()
  "Retrieve full func call and insert it into own buffer."
  (condition-case nil (require 'undercover) (t nil))
  (when (fboundp 'undercover)
    (ert-skip "Undercover messing up with the call stack..."))
  (syncthing-ert-cleanup)
  (let* ((name "dummy")
         (syncthing-server (syncthing--server :name name))
         (syncthing-debug t)
         (repr "(dummy 1 \"2\" 3 (4) (5 . 6) 7 :opt-key 8)")
         prev-func)
    (advice-add 'syncthing--previous-func
                :filter-return
                (lambda (ret) (setq prev-func ret) prev-func))
    (dummy 1 "2" '3 (list 4) (cons 5 6) 7 :opt-key 8)
    (should-not (null prev-func))
    (should (eq (type-of prev-func) 'cons))
    (should (string= (format "%S" prev-func) repr))
    (should (string=
             (format "%s\n" repr)
             (with-current-buffer
                 (get-buffer (format syncthing-trace-format-buffer name))
               (buffer-string))))
    (advice-remove 'syncthing--previous-func
                   (lambda (ret) (setq prev-func ret) prev-func))))

(ert-deftest syncthing-trace-own-buffer-insert ()
  "Insert previous func into own buffer on `syncthing-debug'."
  (syncthing-ert-cleanup)
  (let ((name "trace")
        (syncthing-trace-format-buffer "*syncthing test(%s)*")
        (syncthing-server (syncthing--server :name "trace"))
        (old-buffs (format "%s" (buffer-list)))
        (prev-func '(some-func))
        syncthing-debug called)
    (advice-add 'syncthing--previous-func
                :override
                (lambda (&rest _) (setq called t) prev-func))
    (syncthing-trace)
    (should (string= (format "%s" (buffer-list)) old-buffs))
    (should-not called)

    (setq syncthing-debug t)
    (syncthing-trace)
    (should-not (string= (format "%s" (buffer-list)) old-buffs))
    (should (member (format syncthing-trace-format-buffer name)
                    (mapcar (lambda (buff) (buffer-name buff))
                            (buffer-list))))
    (should (string=
             (with-current-buffer
                 (get-buffer (format syncthing-trace-format-buffer name))
               (buffer-string))
             (format "%s\n" prev-func)))
    (advice-remove 'syncthing--previous-func
                   (lambda (&rest _) (setq called t)))))

(ert-deftest syncthing-get-widget-at-pos ()
  "Call widget's `:action' when `point' is on top of it."
  (syncthing-ert-cleanup)
  (with-temp-buffer
    (let (called)
      (widget-create 'push-button :action (lambda (&rest _) (setq called t)))
      (widget-setup)
      (should (string= "[]" (buffer-string)))
      (syncthing--newline 1)  ; Move into the button area first
      (should called))))

(ert-deftest syncthing-sort-generic ()
  "Sort objects' values accessed by the same key."
  (syncthing-ert-cleanup)
  (let* ((one (list (cons 'key "1")))
         (two (list (cons 'key "2")))
         (three (list (cons 'key "3")))
         (before (list three one two))
         (after (list one two three)))
    (should (string= (format "%s" (sort (copy-sequence before)
                                        (lambda (left right)
                                          (syncthing--flat-string-sort
                                           "key" left right))))
                     (format "%s" after)))
    (should (eq (car before) (car (reverse after))))))

(ert-deftest syncthing-sort-folders ()
  (syncthing-ert-cleanup)
  (let* ((one (list (cons 'label "1")))
         (two (list (cons 'label "2")))
         (three (list (cons 'label "3")))
         (before (list three one two))
         (after (list one two three)))
    (should (string= (format "%s" (sort (copy-sequence before)
                                        #'syncthing--sort-folders))
                     (format "%s" after)))
    (should (eq (car before) (car (reverse after))))))

(ert-deftest syncthing-sort-devices ()
  "Sort objects' values accessed by the same key."
  (syncthing-ert-cleanup)
  (let* ((one (list (cons 'name "1")))
         (two (list (cons 'name "2")))
         (three (list (cons 'name "3")))
         (before (list three one two))
         (after (list one two three)))
    (should (string= (format "%s" (sort (copy-sequence before)
                                        #'syncthing--sort-devices))
                     (format "%s" after)))
    (should (eq (car before) (car (reverse after))))))

(ert-deftest syncthing-calc-uptime-short ()
  "Calculates uptime string properly in shortened form."
  (syncthing-ert-cleanup)
  (should (string= "" (syncthing--sec-to-uptime 0)))
  (should (string= "1s" (syncthing--sec-to-uptime 1)))
  (should (string= "9s" (syncthing--sec-to-uptime 9)))
  (should (string= "10s" (syncthing--sec-to-uptime 10)))
  (should (string= "59s" (syncthing--sec-to-uptime 59)))
  (should (string= "1m" (syncthing--sec-to-uptime 60)))

  (should (string= "1m 1s" (syncthing--sec-to-uptime (+ 60 1))))
  (should (string= "1m 9s" (syncthing--sec-to-uptime (+ 60 9))))
  (should (string= "1m 10s" (syncthing--sec-to-uptime (+ 60 10))))
  (should (string= "1m 59s" (syncthing--sec-to-uptime (+ 60 59))))
  (should (string= "9m 1s" (syncthing--sec-to-uptime (+ (* 60 9) 1))))
  (should (string= "9m 9s" (syncthing--sec-to-uptime (+ (* 60 9) 9))))
  (should (string= "9m 10s" (syncthing--sec-to-uptime (+ (* 60 9) 10))))
  (should (string= "9m 59s" (syncthing--sec-to-uptime (+ (* 60 9) 59))))
  (should (string= "10m 1s" (syncthing--sec-to-uptime (+ (* 60 10) 1))))
  (should (string= "10m 9s" (syncthing--sec-to-uptime (+ (* 60 10) 9))))
  (should (string= "10m 10s" (syncthing--sec-to-uptime (+ (* 60 10) 10))))
  (should (string= "10m 59s" (syncthing--sec-to-uptime (+ (* 60 10) 59))))
  (should (string= "59m 1s" (syncthing--sec-to-uptime (+ (* 60 59) 1))))
  (should (string= "59m 9s" (syncthing--sec-to-uptime (+ (* 60 59) 9))))
  (should (string= "59m 10s" (syncthing--sec-to-uptime (+ (* 60 59) 10))))
  (should (string= "59m 59s" (syncthing--sec-to-uptime (+ (* 60 59) 59))))
  (should (string= "1h" (syncthing--sec-to-uptime (+ (* 60 60)))))

  (should (string= "1h 1m 1s" (syncthing--sec-to-uptime
                               (+ (* 60 60 1) (* 60 1) 1))))
  (should (string= "1h 1m 9s" (syncthing--sec-to-uptime
                               (+ (* 60 60 1) (* 60 1) 9))))
  (should (string= "1h 1m 10s" (syncthing--sec-to-uptime
                                (+ (* 60 60 1) (* 60 1) 10))))
  (should (string= "1h 1m 59s" (syncthing--sec-to-uptime
                                (+ (* 60 60 1) (* 60 1) 59))))
  (should (string= "1h 9m 1s" (syncthing--sec-to-uptime
                               (+ (* 60 60 1) (* 60 9) 1))))
  (should (string= "1h 9m 9s" (syncthing--sec-to-uptime
                               (+ (* 60 60 1) (* 60 9) 9))))
  (should (string= "1h 9m 10s" (syncthing--sec-to-uptime
                                (+ (* 60 60 1) (* 60 9) 10))))
  (should (string= "1h 9m 59s" (syncthing--sec-to-uptime
                                (+ (* 60 60 1) (* 60 9) 59))))
  (should (string= "1h 10m 1s" (syncthing--sec-to-uptime
                                (+ (* 60 60 1) (* 60 10) 1))))
  (should (string= "1h 10m 9s" (syncthing--sec-to-uptime
                                (+ (* 60 60 1) (* 60 10) 9))))
  (should (string= "1h 10m 10s" (syncthing--sec-to-uptime
                                 (+ (* 60 60 1) (* 60 10) 10))))
  (should (string= "1h 10m 59s" (syncthing--sec-to-uptime
                                 (+ (* 60 60 1) (* 60 10) 59))))
  (should (string= "1h 59m 1s" (syncthing--sec-to-uptime
                                (+ (* 60 60 1) (* 60 59) 1))))
  (should (string= "1h 59m 9s" (syncthing--sec-to-uptime
                                (+ (* 60 60 1) (* 60 59) 9))))
  (should (string= "1h 59m 10s" (syncthing--sec-to-uptime
                                 (+ (* 60 60 1) (* 60 59) 10))))
  (should (string= "1h 59m 59s" (syncthing--sec-to-uptime
                                 (+ (* 60 60 1) (* 60 59) 59))))

  (should (string= "9h 1m 1s" (syncthing--sec-to-uptime
                               (+ (* 60 60 9) (* 60 1) 1))))
  (should (string= "9h 1m 9s" (syncthing--sec-to-uptime
                               (+ (* 60 60 9) (* 60 1) 9))))
  (should (string= "9h 1m 10s" (syncthing--sec-to-uptime
                                (+ (* 60 60 9) (* 60 1) 10))))
  (should (string= "9h 1m 59s" (syncthing--sec-to-uptime
                                (+ (* 60 60 9) (* 60 1) 59))))
  (should (string= "9h 9m 1s" (syncthing--sec-to-uptime
                               (+ (* 60 60 9) (* 60 9) 1))))
  (should (string= "9h 9m 9s" (syncthing--sec-to-uptime
                               (+ (* 60 60 9) (* 60 9) 9))))
  (should (string= "9h 9m 10s" (syncthing--sec-to-uptime
                                (+ (* 60 60 9) (* 60 9) 10))))
  (should (string= "9h 9m 59s" (syncthing--sec-to-uptime
                                (+ (* 60 60 9) (* 60 9) 59))))
  (should (string= "9h 10m 1s" (syncthing--sec-to-uptime
                                (+ (* 60 60 9) (* 60 10) 1))))
  (should (string= "9h 10m 9s" (syncthing--sec-to-uptime
                                (+ (* 60 60 9) (* 60 10) 9))))
  (should (string= "9h 10m 10s" (syncthing--sec-to-uptime
                                 (+ (* 60 60 9) (* 60 10) 10))))
  (should (string= "9h 10m 59s" (syncthing--sec-to-uptime
                                 (+ (* 60 60 9) (* 60 10) 59))))
  (should (string= "9h 59m 1s" (syncthing--sec-to-uptime
                                (+ (* 60 60 9) (* 60 59) 1))))
  (should (string= "9h 59m 9s" (syncthing--sec-to-uptime
                                (+ (* 60 60 9) (* 60 59) 9))))
  (should (string= "9h 59m 10s" (syncthing--sec-to-uptime
                                 (+ (* 60 60 9) (* 60 59) 10))))
  (should (string= "9h 59m 59s" (syncthing--sec-to-uptime
                                 (+ (* 60 60 9) (* 60 59) 59))))

  (should (string= "10h 1m 1s" (syncthing--sec-to-uptime
                                (+ (* 60 60 10) (* 60 1) 1))))
  (should (string= "10h 1m 9s" (syncthing--sec-to-uptime
                                (+ (* 60 60 10) (* 60 1) 9))))
  (should (string= "10h 1m 10s" (syncthing--sec-to-uptime
                                 (+ (* 60 60 10) (* 60 1) 10))))
  (should (string= "10h 1m 59s" (syncthing--sec-to-uptime
                                 (+ (* 60 60 10) (* 60 1) 59))))
  (should (string= "10h 9m 1s" (syncthing--sec-to-uptime
                                (+ (* 60 60 10) (* 60 9) 1))))
  (should (string= "10h 9m 9s" (syncthing--sec-to-uptime
                                (+ (* 60 60 10) (* 60 9) 9))))
  (should (string= "10h 9m 10s" (syncthing--sec-to-uptime
                                 (+ (* 60 60 10) (* 60 9) 10))))
  (should (string= "10h 9m 59s" (syncthing--sec-to-uptime
                                 (+ (* 60 60 10) (* 60 9) 59))))
  (should (string= "10h 10m 1s" (syncthing--sec-to-uptime
                                 (+ (* 60 60 10) (* 60 10) 1))))
  (should (string= "10h 10m 9s" (syncthing--sec-to-uptime
                                 (+ (* 60 60 10) (* 60 10) 9))))
  (should (string= "10h 10m 10s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 10) (* 60 10) 10))))
  (should (string= "10h 10m 59s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 10) (* 60 10) 59))))
  (should (string= "10h 59m 1s" (syncthing--sec-to-uptime
                                 (+ (* 60 60 10) (* 60 59) 1))))
  (should (string= "10h 59m 9s" (syncthing--sec-to-uptime
                                 (+ (* 60 60 10) (* 60 59) 9))))
  (should (string= "10h 59m 10s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 10) (* 60 59) 10))))
  (should (string= "10h 59m 59s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 10) (* 60 59) 59))))

  (should (string= "23h 1m 1s" (syncthing--sec-to-uptime
                                (+ (* 60 60 23) (* 60 1) 1))))
  (should (string= "23h 1m 9s" (syncthing--sec-to-uptime
                                (+ (* 60 60 23) (* 60 1) 9))))
  (should (string= "23h 1m 10s" (syncthing--sec-to-uptime
                                 (+ (* 60 60 23) (* 60 1) 10))))
  (should (string= "23h 1m 59s" (syncthing--sec-to-uptime
                                 (+ (* 60 60 23) (* 60 1) 59))))
  (should (string= "23h 9m 1s" (syncthing--sec-to-uptime
                                (+ (* 60 60 23) (* 60 9) 1))))
  (should (string= "23h 9m 9s" (syncthing--sec-to-uptime
                                (+ (* 60 60 23) (* 60 9) 9))))
  (should (string= "23h 9m 10s" (syncthing--sec-to-uptime
                                 (+ (* 60 60 23) (* 60 9) 10))))
  (should (string= "23h 9m 59s" (syncthing--sec-to-uptime
                                 (+ (* 60 60 23) (* 60 9) 59))))
  (should (string= "23h 10m 1s" (syncthing--sec-to-uptime
                                 (+ (* 60 60 23) (* 60 10) 1))))
  (should (string= "23h 10m 9s" (syncthing--sec-to-uptime
                                 (+ (* 60 60 23) (* 60 10) 9))))
  (should (string= "23h 10m 10s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 23) (* 60 10) 10))))
  (should (string= "23h 10m 59s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 23) (* 60 10) 59))))
  (should (string= "23h 59m 1s" (syncthing--sec-to-uptime
                                 (+ (* 60 60 23) (* 60 59) 1))))
  (should (string= "23h 59m 9s" (syncthing--sec-to-uptime
                                 (+ (* 60 60 23) (* 60 59) 9))))
  (should (string= "23h 59m 10s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 23) (* 60 59) 10))))
  (should (string= "23h 59m 59s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 23) (* 60 59) 59))))
  (should (string= "1d" (syncthing--sec-to-uptime
                         (* 60 60 24))))

  (should (string= "1d 1h 1m 1s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 1) 1))))
  (should (string= "1d 1h 1m 9s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 1) 9))))
  (should (string= "1d 1h 1m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 1) 10))))
  (should (string= "1d 1h 1m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 1) 59))))
  (should (string= "1d 1h 9m 1s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 9) 1))))
  (should (string= "1d 1h 9m 9s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 9) 9))))
  (should (string= "1d 1h 9m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 9) 10))))
  (should (string= "1d 1h 9m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 9) 59))))
  (should (string= "1d 1h 10m 1s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 10) 1))))
  (should (string= "1d 1h 10m 9s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 10) 9))))
  (should (string= "1d 1h 10m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 10) 10))))
  (should (string= "1d 1h 10m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 10) 59))))
  (should (string= "1d 1h 59m 1s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 59) 1))))
  (should (string= "1d 1h 59m 9s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 59) 9))))
  (should (string= "1d 1h 59m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 59) 10))))
  (should (string= "1d 1h 59m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 59) 59))))

  (should (string= "32d 1h 1m 1s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 1) 1))))
  (should (string= "32d 1h 1m 9s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 1) 9))))
  (should (string= "32d 1h 1m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 1) 10))))
  (should (string= "32d 1h 1m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 1) 59))))
  (should (string= "32d 1h 9m 1s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 9) 1))))
  (should (string= "32d 1h 9m 9s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 9) 9))))
  (should (string= "32d 1h 9m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 9) 10))))
  (should (string= "32d 1h 9m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 9) 59))))
  (should (string= "32d 1h 10m 1s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 10) 1))))
  (should (string= "32d 1h 10m 9s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 10) 9))))
  (should (string= "32d 1h 10m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 10) 10))))
  (should (string= "32d 1h 10m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 10) 59))))
  (should (string= "32d 1h 59m 1s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 59) 1))))
  (should (string= "32d 1h 59m 9s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 59) 9))))
  (should (string= "32d 1h 59m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 59) 10))))
  (should (string= "32d 1h 59m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 59) 59))))

  (should (string= "367d 1h 1m 1s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 1) 1))))
  (should (string= "367d 1h 1m 9s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 1) 9))))
  (should (string= "367d 1h 1m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 1) 10))))
  (should (string= "367d 1h 1m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 1) 59))))
  (should (string= "367d 1h 9m 1s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 9) 1))))
  (should (string= "367d 1h 9m 9s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 9) 9))))
  (should (string= "367d 1h 9m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 9) 10))))
  (should (string= "367d 1h 9m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 9) 59))))
  (should (string= "367d 1h 10m 1s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 10) 1))))
  (should (string= "367d 1h 10m 9s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 10) 9))))
  (should (string= "367d 1h 10m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 10) 10))))
  (should (string= "367d 1h 10m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 10) 59))))
  (should (string= "367d 1h 59m 1s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 59) 1))))
  (should (string= "367d 1h 59m 9s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 59) 9))))
  (should (string= "367d 1h 59m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 59) 10))))
  (should (string= "367d 1h 59m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 59) 59)))))

(ert-deftest syncthing-calc-uptime-full ()
  "Calculates uptime string properly in full form."
  (syncthing-ert-cleanup)
  (should (string= "0d 0h 0m 0s" (syncthing--sec-to-uptime 0 :full t)))
  (should (string= "0d 0h 0m 1s" (syncthing--sec-to-uptime 1 :full t)))
  (should (string= "0d 0h 0m 9s" (syncthing--sec-to-uptime 9 :full t)))
  (should (string= "0d 0h 0m 10s" (syncthing--sec-to-uptime 10 :full t)))
  (should (string= "0d 0h 0m 59s" (syncthing--sec-to-uptime 59 :full t)))
  (should (string= "0d 0h 1m 0s" (syncthing--sec-to-uptime 60 :full t)))

  (should (string= "0d 0h 1m 1s" (syncthing--sec-to-uptime (+ 60 1) :full t)))
  (should (string= "0d 0h 1m 9s" (syncthing--sec-to-uptime (+ 60 9) :full t)))
  (should (string= "0d 0h 1m 10s" (syncthing--sec-to-uptime
                                   (+ 60 10) :full t)))
  (should (string= "0d 0h 1m 59s" (syncthing--sec-to-uptime
                                   (+ 60 59) :full t)))
  (should (string= "0d 0h 9m 1s" (syncthing--sec-to-uptime
                                  (+ (* 60 9) 1) :full t)))
  (should (string= "0d 0h 9m 9s" (syncthing--sec-to-uptime
                                  (+ (* 60 9) 9) :full t)))
  (should (string= "0d 0h 9m 10s" (syncthing--sec-to-uptime
                                   (+ (* 60 9) 10) :full t)))
  (should (string= "0d 0h 9m 59s" (syncthing--sec-to-uptime
                                   (+ (* 60 9) 59) :full t)))
  (should (string= "0d 0h 10m 1s" (syncthing--sec-to-uptime
                                   (+ (* 60 10) 1) :full t)))
  (should (string= "0d 0h 10m 9s" (syncthing--sec-to-uptime
                                   (+ (* 60 10) 9) :full t)))
  (should (string= "0d 0h 10m 10s" (syncthing--sec-to-uptime
                                    (+ (* 60 10) 10) :full t)))
  (should (string= "0d 0h 10m 59s" (syncthing--sec-to-uptime
                                    (+ (* 60 10) 59) :full t)))
  (should (string= "0d 0h 59m 1s" (syncthing--sec-to-uptime
                                   (+ (* 60 59) 1) :full t)))
  (should (string= "0d 0h 59m 9s" (syncthing--sec-to-uptime
                                   (+ (* 60 59) 9) :full t)))
  (should (string= "0d 0h 59m 10s" (syncthing--sec-to-uptime
                                    (+ (* 60 59) 10) :full t)))
  (should (string= "0d 0h 59m 59s" (syncthing--sec-to-uptime
                                    (+ (* 60 59) 59) :full t)))
  (should (string= "0d 1h 0m 0s" (syncthing--sec-to-uptime
                                  (+ (* 60 60)) :full t)))

  (should (string= "0d 1h 1m 1s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 1) (* 60 1) 1) :full t)))
  (should (string= "0d 1h 1m 9s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 1) (* 60 1) 9) :full t)))
  (should (string= "0d 1h 1m 10s" (syncthing--sec-to-uptime
                                   (+ (* 60 60 1) (* 60 1) 10) :full t)))
  (should (string= "0d 1h 1m 59s" (syncthing--sec-to-uptime
                                   (+ (* 60 60 1) (* 60 1) 59) :full t)))
  (should (string= "0d 1h 9m 1s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 1) (* 60 9) 1) :full t)))
  (should (string= "0d 1h 9m 9s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 1) (* 60 9) 9) :full t)))
  (should (string= "0d 1h 9m 10s" (syncthing--sec-to-uptime
                                   (+ (* 60 60 1) (* 60 9) 10) :full t)))
  (should (string= "0d 1h 9m 59s" (syncthing--sec-to-uptime
                                   (+ (* 60 60 1) (* 60 9) 59) :full t)))
  (should (string= "0d 1h 10m 1s" (syncthing--sec-to-uptime
                                   (+ (* 60 60 1) (* 60 10) 1) :full t)))
  (should (string= "0d 1h 10m 9s" (syncthing--sec-to-uptime
                                   (+ (* 60 60 1) (* 60 10) 9) :full t)))
  (should (string= "0d 1h 10m 10s" (syncthing--sec-to-uptime
                                    (+ (* 60 60 1) (* 60 10) 10) :full t)))
  (should (string= "0d 1h 10m 59s" (syncthing--sec-to-uptime
                                    (+ (* 60 60 1) (* 60 10) 59) :full t)))
  (should (string= "0d 1h 59m 1s" (syncthing--sec-to-uptime
                                   (+ (* 60 60 1) (* 60 59) 1) :full t)))
  (should (string= "0d 1h 59m 9s" (syncthing--sec-to-uptime
                                   (+ (* 60 60 1) (* 60 59) 9) :full t)))
  (should (string= "0d 1h 59m 10s" (syncthing--sec-to-uptime
                                    (+ (* 60 60 1) (* 60 59) 10) :full t)))
  (should (string= "0d 1h 59m 59s" (syncthing--sec-to-uptime
                                    (+ (* 60 60 1) (* 60 59) 59) :full t)))

  (should (string= "0d 9h 1m 1s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 9) (* 60 1) 1) :full t)))
  (should (string= "0d 9h 1m 9s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 9) (* 60 1) 9) :full t)))
  (should (string= "0d 9h 1m 10s" (syncthing--sec-to-uptime
                                   (+ (* 60 60 9) (* 60 1) 10) :full t)))
  (should (string= "0d 9h 1m 59s" (syncthing--sec-to-uptime
                                   (+ (* 60 60 9) (* 60 1) 59) :full t)))
  (should (string= "0d 9h 9m 1s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 9) (* 60 9) 1) :full t)))
  (should (string= "0d 9h 9m 9s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 9) (* 60 9) 9) :full t)))
  (should (string= "0d 9h 9m 10s" (syncthing--sec-to-uptime
                                   (+ (* 60 60 9) (* 60 9) 10) :full t)))
  (should (string= "0d 9h 9m 59s" (syncthing--sec-to-uptime
                                   (+ (* 60 60 9) (* 60 9) 59) :full t)))
  (should (string= "0d 9h 10m 1s" (syncthing--sec-to-uptime
                                   (+ (* 60 60 9) (* 60 10) 1) :full t)))
  (should (string= "0d 9h 10m 9s" (syncthing--sec-to-uptime
                                   (+ (* 60 60 9) (* 60 10) 9) :full t)))
  (should (string= "0d 9h 10m 10s" (syncthing--sec-to-uptime
                                    (+ (* 60 60 9) (* 60 10) 10) :full t)))
  (should (string= "0d 9h 10m 59s" (syncthing--sec-to-uptime
                                    (+ (* 60 60 9) (* 60 10) 59) :full t)))
  (should (string= "0d 9h 59m 1s" (syncthing--sec-to-uptime
                                   (+ (* 60 60 9) (* 60 59) 1) :full t)))
  (should (string= "0d 9h 59m 9s" (syncthing--sec-to-uptime
                                   (+ (* 60 60 9) (* 60 59) 9) :full t)))
  (should (string= "0d 9h 59m 10s" (syncthing--sec-to-uptime
                                    (+ (* 60 60 9) (* 60 59) 10) :full t)))
  (should (string= "0d 9h 59m 59s" (syncthing--sec-to-uptime
                                    (+ (* 60 60 9) (* 60 59) 59) :full t)))

  (should (string= "0d 10h 1m 1s" (syncthing--sec-to-uptime
                                   (+ (* 60 60 10) (* 60 1) 1) :full t)))
  (should (string= "0d 10h 1m 9s" (syncthing--sec-to-uptime
                                   (+ (* 60 60 10) (* 60 1) 9) :full t)))
  (should (string= "0d 10h 1m 10s" (syncthing--sec-to-uptime
                                    (+ (* 60 60 10) (* 60 1) 10) :full t)))
  (should (string= "0d 10h 1m 59s" (syncthing--sec-to-uptime
                                    (+ (* 60 60 10) (* 60 1) 59) :full t)))
  (should (string= "0d 10h 9m 1s" (syncthing--sec-to-uptime
                                   (+ (* 60 60 10) (* 60 9) 1) :full t)))
  (should (string= "0d 10h 9m 9s" (syncthing--sec-to-uptime
                                   (+ (* 60 60 10) (* 60 9) 9) :full t)))
  (should (string= "0d 10h 9m 10s" (syncthing--sec-to-uptime
                                    (+ (* 60 60 10) (* 60 9) 10) :full t)))
  (should (string= "0d 10h 9m 59s" (syncthing--sec-to-uptime
                                    (+ (* 60 60 10) (* 60 9) 59) :full t)))
  (should (string= "0d 10h 10m 1s" (syncthing--sec-to-uptime
                                    (+ (* 60 60 10) (* 60 10) 1) :full t)))
  (should (string= "0d 10h 10m 9s" (syncthing--sec-to-uptime
                                    (+ (* 60 60 10) (* 60 10) 9) :full t)))
  (should (string= "0d 10h 10m 10s" (syncthing--sec-to-uptime
                                     (+ (* 60 60 10) (* 60 10) 10) :full t)))
  (should (string= "0d 10h 10m 59s" (syncthing--sec-to-uptime
                                     (+ (* 60 60 10) (* 60 10) 59) :full t)))
  (should (string= "0d 10h 59m 1s" (syncthing--sec-to-uptime
                                    (+ (* 60 60 10) (* 60 59) 1) :full t)))
  (should (string= "0d 10h 59m 9s" (syncthing--sec-to-uptime
                                    (+ (* 60 60 10) (* 60 59) 9) :full t)))
  (should (string= "0d 10h 59m 10s" (syncthing--sec-to-uptime
                                     (+ (* 60 60 10) (* 60 59) 10) :full t)))
  (should (string= "0d 10h 59m 59s" (syncthing--sec-to-uptime
                                     (+ (* 60 60 10) (* 60 59) 59) :full t)))

  (should (string= "0d 23h 1m 1s" (syncthing--sec-to-uptime
                                   (+ (* 60 60 23) (* 60 1) 1) :full t)))
  (should (string= "0d 23h 1m 9s" (syncthing--sec-to-uptime
                                   (+ (* 60 60 23) (* 60 1) 9) :full t)))
  (should (string= "0d 23h 1m 10s" (syncthing--sec-to-uptime
                                    (+ (* 60 60 23) (* 60 1) 10) :full t)))
  (should (string= "0d 23h 1m 59s" (syncthing--sec-to-uptime
                                    (+ (* 60 60 23) (* 60 1) 59) :full t)))
  (should (string= "0d 23h 9m 1s" (syncthing--sec-to-uptime
                                   (+ (* 60 60 23) (* 60 9) 1) :full t)))
  (should (string= "0d 23h 9m 9s" (syncthing--sec-to-uptime
                                   (+ (* 60 60 23) (* 60 9) 9) :full t)))
  (should (string= "0d 23h 9m 10s" (syncthing--sec-to-uptime
                                    (+ (* 60 60 23) (* 60 9) 10) :full t)))
  (should (string= "0d 23h 9m 59s" (syncthing--sec-to-uptime
                                    (+ (* 60 60 23) (* 60 9) 59) :full t)))
  (should (string= "0d 23h 10m 1s" (syncthing--sec-to-uptime
                                    (+ (* 60 60 23) (* 60 10) 1) :full t)))
  (should (string= "0d 23h 10m 9s" (syncthing--sec-to-uptime
                                    (+ (* 60 60 23) (* 60 10) 9) :full t)))
  (should (string= "0d 23h 10m 10s" (syncthing--sec-to-uptime
                                     (+ (* 60 60 23) (* 60 10) 10) :full t)))
  (should (string= "0d 23h 10m 59s" (syncthing--sec-to-uptime
                                     (+ (* 60 60 23) (* 60 10) 59) :full t)))
  (should (string= "0d 23h 59m 1s" (syncthing--sec-to-uptime
                                    (+ (* 60 60 23) (* 60 59) 1) :full t)))
  (should (string= "0d 23h 59m 9s" (syncthing--sec-to-uptime
                                    (+ (* 60 60 23) (* 60 59) 9) :full t)))
  (should (string= "0d 23h 59m 10s" (syncthing--sec-to-uptime
                                     (+ (* 60 60 23) (* 60 59) 10) :full t)))
  (should (string= "0d 23h 59m 59s" (syncthing--sec-to-uptime
                                     (+ (* 60 60 23) (* 60 59) 59) :full t)))
  (should (string= "1d 0h 0m 0s" (syncthing--sec-to-uptime
                                  (* 60 60 24) :full t)))

  (should (string= "1d 1h 1m 1s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 1) 1) :full t)))
  (should (string= "1d 1h 1m 9s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 1) 9) :full t)))
  (should (string= "1d 1h 1m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 1) 10) :full t)))
  (should (string= "1d 1h 1m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 1) 59) :full t)))
  (should (string= "1d 1h 9m 1s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 9) 1) :full t)))
  (should (string= "1d 1h 9m 9s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 9) 9) :full t)))
  (should (string= "1d 1h 9m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 9) 10) :full t)))
  (should (string= "1d 1h 9m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 9) 59) :full t)))
  (should (string= "1d 1h 10m 1s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 10) 1) :full t)))
  (should (string= "1d 1h 10m 9s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 10) 9) :full t)))
  (should (string= "1d 1h 10m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 10) 10) :full t)))
  (should (string= "1d 1h 10m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 10) 59) :full t)))
  (should (string= "1d 1h 59m 1s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 59) 1) :full t)))
  (should (string= "1d 1h 59m 9s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 59) 9) :full t)))
  (should (string= "1d 1h 59m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 59) 10) :full t)))
  (should (string= "1d 1h 59m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 59) 59) :full t)))

  (should (string= "32d 1h 1m 1s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 1) 1) :full t)))
  (should (string= "32d 1h 1m 9s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 1) 9) :full t)))
  (should (string= "32d 1h 1m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 1) 10) :full t)))
  (should (string= "32d 1h 1m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 1) 59) :full t)))
  (should (string= "32d 1h 9m 1s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 9) 1) :full t)))
  (should (string= "32d 1h 9m 9s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 9) 9) :full t)))
  (should (string= "32d 1h 9m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 9) 10) :full t)))
  (should (string= "32d 1h 9m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 9) 59) :full t)))
  (should (string= "32d 1h 10m 1s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 10) 1) :full t)))
  (should (string= "32d 1h 10m 9s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 10) 9) :full t)))
  (should (string= "32d 1h 10m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 10) 10) :full t)))
  (should (string= "32d 1h 10m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 10) 59) :full t)))
  (should (string= "32d 1h 59m 1s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 59) 1) :full t)))
  (should (string= "32d 1h 59m 9s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 59) 9) :full t)))
  (should (string= "32d 1h 59m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 59) 10) :full t)))
  (should (string= "32d 1h 59m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 59) 59) :full t)))

  (should (string= "367d 1h 1m 1s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 1) 1) :full t)))
  (should (string= "367d 1h 1m 9s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 1) 9) :full t)))
  (should (string= "367d 1h 1m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 1) 10) :full t)))
  (should (string= "367d 1h 1m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 1) 59) :full t)))
  (should (string= "367d 1h 9m 1s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 9) 1) :full t)))
  (should (string= "367d 1h 9m 9s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 9) 9) :full t)))
  (should (string= "367d 1h 9m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 9) 10) :full t)))
  (should (string= "367d 1h 9m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 9) 59) :full t)))
  (should (string= "367d 1h 10m 1s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 10) 1) :full t)))
  (should (string= "367d 1h 10m 9s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 10) 9) :full t)))
  (should (string= "367d 1h 10m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 10) 10) :full t)))
  (should (string= "367d 1h 10m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 10) 59) :full t)))
  (should (string= "367d 1h 59m 1s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 59) 1) :full t)))
  (should (string= "367d 1h 59m 9s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 59) 9) :full t)))
  (should (string= "367d 1h 59m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 59) 10) :full t)))
  (should (string= "367d 1h 59m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 59) 59) :full t))))

(ert-deftest syncthing-calc-uptime-short-pad ()
  "Calculates uptime string properly in padded short form."
  (syncthing-ert-cleanup)
  (should (string= "" (syncthing--sec-to-uptime 0 :pad t)))
  (should (string= "01s" (syncthing--sec-to-uptime 1 :pad t)))
  (should (string= "09s" (syncthing--sec-to-uptime 9 :pad t)))
  (should (string= "10s" (syncthing--sec-to-uptime 10 :pad t)))
  (should (string= "59s" (syncthing--sec-to-uptime 59 :pad t)))
  (should (string= "01m" (syncthing--sec-to-uptime 60 :pad t)))

  (should (string= "01m 01s" (syncthing--sec-to-uptime (+ 60 1) :pad t)))
  (should (string= "01m 09s" (syncthing--sec-to-uptime (+ 60 9) :pad t)))
  (should (string= "01m 10s" (syncthing--sec-to-uptime (+ 60 10) :pad t)))
  (should (string= "01m 59s" (syncthing--sec-to-uptime (+ 60 59) :pad t)))
  (should (string= "09m 01s" (syncthing--sec-to-uptime
                              (+ (* 60 9) 1) :pad t)))
  (should (string= "09m 09s" (syncthing--sec-to-uptime
                              (+ (* 60 9) 9) :pad t)))
  (should (string= "09m 10s" (syncthing--sec-to-uptime
                              (+ (* 60 9) 10) :pad t)))
  (should (string= "09m 59s" (syncthing--sec-to-uptime
                              (+ (* 60 9) 59) :pad t)))
  (should (string= "10m 01s" (syncthing--sec-to-uptime
                              (+ (* 60 10) 1) :pad t)))
  (should (string= "10m 09s" (syncthing--sec-to-uptime
                              (+ (* 60 10) 9) :pad t)))
  (should (string= "10m 10s" (syncthing--sec-to-uptime
                              (+ (* 60 10) 10) :pad t)))
  (should (string= "10m 59s" (syncthing--sec-to-uptime
                              (+ (* 60 10) 59) :pad t)))
  (should (string= "59m 01s" (syncthing--sec-to-uptime
                              (+ (* 60 59) 1) :pad t)))
  (should (string= "59m 09s" (syncthing--sec-to-uptime
                              (+ (* 60 59) 9) :pad t)))
  (should (string= "59m 10s" (syncthing--sec-to-uptime
                              (+ (* 60 59) 10) :pad t)))
  (should (string= "59m 59s" (syncthing--sec-to-uptime
                              (+ (* 60 59) 59) :pad t)))
  (should (string= "01h" (syncthing--sec-to-uptime (+ (* 60 60)) :pad t)))

  (should (string= "01h 01m 01s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 1) (* 60 1) 1) :pad t)))
  (should (string= "01h 01m 09s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 1) (* 60 1) 9) :pad t)))
  (should (string= "01h 01m 10s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 1) (* 60 1) 10) :pad t)))
  (should (string= "01h 01m 59s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 1) (* 60 1) 59) :pad t)))
  (should (string= "01h 09m 01s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 1) (* 60 9) 1) :pad t)))
  (should (string= "01h 09m 09s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 1) (* 60 9) 9) :pad t)))
  (should (string= "01h 09m 10s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 1) (* 60 9) 10) :pad t)))
  (should (string= "01h 09m 59s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 1) (* 60 9) 59) :pad t)))
  (should (string= "01h 10m 01s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 1) (* 60 10) 1) :pad t)))
  (should (string= "01h 10m 09s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 1) (* 60 10) 9) :pad t)))
  (should (string= "01h 10m 10s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 1) (* 60 10) 10) :pad t)))
  (should (string= "01h 10m 59s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 1) (* 60 10) 59) :pad t)))
  (should (string= "01h 59m 01s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 1) (* 60 59) 1) :pad t)))
  (should (string= "01h 59m 09s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 1) (* 60 59) 9) :pad t)))
  (should (string= "01h 59m 10s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 1) (* 60 59) 10) :pad t)))
  (should (string= "01h 59m 59s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 1) (* 60 59) 59) :pad t)))

  (should (string= "09h 01m 01s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 9) (* 60 1) 1) :pad t)))
  (should (string= "09h 01m 09s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 9) (* 60 1) 9) :pad t)))
  (should (string= "09h 01m 10s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 9) (* 60 1) 10) :pad t)))
  (should (string= "09h 01m 59s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 9) (* 60 1) 59) :pad t)))
  (should (string= "09h 09m 01s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 9) (* 60 9) 1) :pad t)))
  (should (string= "09h 09m 09s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 9) (* 60 9) 9) :pad t)))
  (should (string= "09h 09m 10s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 9) (* 60 9) 10) :pad t)))
  (should (string= "09h 09m 59s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 9) (* 60 9) 59) :pad t)))
  (should (string= "09h 10m 01s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 9) (* 60 10) 1) :pad t)))
  (should (string= "09h 10m 09s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 9) (* 60 10) 9) :pad t)))
  (should (string= "09h 10m 10s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 9) (* 60 10) 10) :pad t)))
  (should (string= "09h 10m 59s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 9) (* 60 10) 59) :pad t)))
  (should (string= "09h 59m 01s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 9) (* 60 59) 1) :pad t)))
  (should (string= "09h 59m 09s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 9) (* 60 59) 9) :pad t)))
  (should (string= "09h 59m 10s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 9) (* 60 59) 10) :pad t)))
  (should (string= "09h 59m 59s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 9) (* 60 59) 59) :pad t)))

  (should (string= "10h 01m 01s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 10) (* 60 1) 1) :pad t)))
  (should (string= "10h 01m 09s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 10) (* 60 1) 9) :pad t)))
  (should (string= "10h 01m 10s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 10) (* 60 1) 10) :pad t)))
  (should (string= "10h 01m 59s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 10) (* 60 1) 59) :pad t)))
  (should (string= "10h 09m 01s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 10) (* 60 9) 1) :pad t)))
  (should (string= "10h 09m 09s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 10) (* 60 9) 9) :pad t)))
  (should (string= "10h 09m 10s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 10) (* 60 9) 10) :pad t)))
  (should (string= "10h 09m 59s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 10) (* 60 9) 59) :pad t)))
  (should (string= "10h 10m 01s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 10) (* 60 10) 1) :pad t)))
  (should (string= "10h 10m 09s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 10) (* 60 10) 9) :pad t)))
  (should (string= "10h 10m 10s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 10) (* 60 10) 10) :pad t)))
  (should (string= "10h 10m 59s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 10) (* 60 10) 59) :pad t)))
  (should (string= "10h 59m 01s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 10) (* 60 59) 1) :pad t)))
  (should (string= "10h 59m 09s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 10) (* 60 59) 9) :pad t)))
  (should (string= "10h 59m 10s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 10) (* 60 59) 10) :pad t)))
  (should (string= "10h 59m 59s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 10) (* 60 59) 59) :pad t)))

  (should (string= "23h 01m 01s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 23) (* 60 1) 1) :pad t)))
  (should (string= "23h 01m 09s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 23) (* 60 1) 9) :pad t)))
  (should (string= "23h 01m 10s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 23) (* 60 1) 10) :pad t)))
  (should (string= "23h 01m 59s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 23) (* 60 1) 59) :pad t)))
  (should (string= "23h 09m 01s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 23) (* 60 9) 1) :pad t)))
  (should (string= "23h 09m 09s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 23) (* 60 9) 9) :pad t)))
  (should (string= "23h 09m 10s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 23) (* 60 9) 10) :pad t)))
  (should (string= "23h 09m 59s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 23) (* 60 9) 59) :pad t)))
  (should (string= "23h 10m 01s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 23) (* 60 10) 1) :pad t)))
  (should (string= "23h 10m 09s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 23) (* 60 10) 9) :pad t)))
  (should (string= "23h 10m 10s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 23) (* 60 10) 10) :pad t)))
  (should (string= "23h 10m 59s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 23) (* 60 10) 59) :pad t)))
  (should (string= "23h 59m 01s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 23) (* 60 59) 1) :pad t)))
  (should (string= "23h 59m 09s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 23) (* 60 59) 9) :pad t)))
  (should (string= "23h 59m 10s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 23) (* 60 59) 10) :pad t)))
  (should (string= "23h 59m 59s" (syncthing--sec-to-uptime
                                  (+ (* 60 60 23) (* 60 59) 59) :pad t)))
  (should (string= "001d" (syncthing--sec-to-uptime
                           (* 60 60 24) :pad t)))

  (should (string= "001d 01h 01m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 1) 1) :pad t)))
  (should (string= "001d 01h 01m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 1) 9) :pad t)))
  (should (string= "001d 01h 01m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 1) 10) :pad t)))
  (should (string= "001d 01h 01m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 1) 59) :pad t)))
  (should (string= "001d 01h 09m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 9) 1) :pad t)))
  (should (string= "001d 01h 09m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 9) 9) :pad t)))
  (should (string= "001d 01h 09m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 9) 10) :pad t)))
  (should (string= "001d 01h 09m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 9) 59) :pad t)))
  (should (string= "001d 01h 10m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 10) 1) :pad t)))
  (should (string= "001d 01h 10m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 10) 9) :pad t)))
  (should (string= "001d 01h 10m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 10) 10) :pad t)))
  (should (string= "001d 01h 10m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 10) 59) :pad t)))
  (should (string= "001d 01h 59m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 59) 1) :pad t)))
  (should (string= "001d 01h 59m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 59) 9) :pad t)))
  (should (string= "001d 01h 59m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 59) 10) :pad t)))
  (should (string= "001d 01h 59m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 59) 59) :pad t)))

  (should (string= "032d 01h 01m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 1) 1) :pad t)))
  (should (string= "032d 01h 01m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 1) 9) :pad t)))
  (should (string= "032d 01h 01m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 1) 10) :pad t)))
  (should (string= "032d 01h 01m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 1) 59) :pad t)))
  (should (string= "032d 01h 09m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 9) 1) :pad t)))
  (should (string= "032d 01h 09m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 9) 9) :pad t)))
  (should (string= "032d 01h 09m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 9) 10) :pad t)))
  (should (string= "032d 01h 09m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 9) 59) :pad t)))
  (should (string= "032d 01h 10m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 10) 1) :pad t)))
  (should (string= "032d 01h 10m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 10) 9) :pad t)))
  (should (string= "032d 01h 10m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 10) 10) :pad t)))
  (should (string= "032d 01h 10m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 10) 59) :pad t)))
  (should (string= "032d 01h 59m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 59) 1) :pad t)))
  (should (string= "032d 01h 59m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 59) 9) :pad t)))
  (should (string= "032d 01h 59m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 59) 10) :pad t)))
  (should (string= "032d 01h 59m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 59) 59) :pad t)))

  (should (string= "367d 01h 01m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 1) 1) :pad t)))
  (should (string= "367d 01h 01m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 1) 9) :pad t)))
  (should (string= "367d 01h 01m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 1) 10) :pad t)))
  (should (string= "367d 01h 01m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 1) 59) :pad t)))
  (should (string= "367d 01h 09m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 9) 1) :pad t)))
  (should (string= "367d 01h 09m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 9) 9) :pad t)))
  (should (string= "367d 01h 09m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 9) 10) :pad t)))
  (should (string= "367d 01h 09m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 9) 59) :pad t)))
  (should (string= "367d 01h 10m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 10) 1) :pad t)))
  (should (string= "367d 01h 10m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 10) 9) :pad t)))
  (should (string= "367d 01h 10m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 10) 10) :pad t)))
  (should (string= "367d 01h 10m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 10) 59) :pad t)))
  (should (string= "367d 01h 59m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 59) 1) :pad t)))
  (should (string= "367d 01h 59m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 59) 9) :pad t)))
  (should (string= "367d 01h 59m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 59) 10) :pad t)))
  (should (string= "367d 01h 59m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 59) 59) :pad t))))

(ert-deftest syncthing-calc-uptime-full-pad ()
  "Calculates uptime string properly in padded full form."
  (syncthing-ert-cleanup)
  (should (string= "000d 00h 00m 00s" (syncthing--sec-to-uptime
                                       0 :full t :pad t)))
  (should (string= "000d 00h 00m 01s" (syncthing--sec-to-uptime
                                       1 :full t :pad t)))
  (should (string= "000d 00h 00m 09s" (syncthing--sec-to-uptime
                                       9 :full t :pad t)))
  (should (string= "000d 00h 00m 10s" (syncthing--sec-to-uptime
                                       10 :full t :pad t)))
  (should (string= "000d 00h 00m 59s" (syncthing--sec-to-uptime
                                       59 :full t :pad t)))
  (should (string= "000d 00h 01m 00s" (syncthing--sec-to-uptime
                                       60 :full t :pad t)))

  (should (string= "000d 00h 01m 01s" (syncthing--sec-to-uptime
                                       (+ 60 1) :full t :pad t)))
  (should (string= "000d 00h 01m 09s" (syncthing--sec-to-uptime
                                       (+ 60 9) :full t :pad t)))
  (should (string= "000d 00h 01m 10s" (syncthing--sec-to-uptime
                                       (+ 60 10) :full t :pad t)))
  (should (string= "000d 00h 01m 59s" (syncthing--sec-to-uptime
                                       (+ 60 59) :full t :pad t)))
  (should (string= "000d 00h 09m 01s" (syncthing--sec-to-uptime
                                       (+ (* 60 9) 1) :full t :pad t)))
  (should (string= "000d 00h 09m 09s" (syncthing--sec-to-uptime
                                       (+ (* 60 9) 9) :full t :pad t)))
  (should (string= "000d 00h 09m 10s" (syncthing--sec-to-uptime
                                       (+ (* 60 9) 10) :full t :pad t)))
  (should (string= "000d 00h 09m 59s" (syncthing--sec-to-uptime
                                       (+ (* 60 9) 59) :full t :pad t)))
  (should (string= "000d 00h 10m 01s" (syncthing--sec-to-uptime
                                       (+ (* 60 10) 1) :full t :pad t)))
  (should (string= "000d 00h 10m 09s" (syncthing--sec-to-uptime
                                       (+ (* 60 10) 9) :full t :pad t)))
  (should (string= "000d 00h 10m 10s" (syncthing--sec-to-uptime
                                       (+ (* 60 10) 10) :full t :pad t)))
  (should (string= "000d 00h 10m 59s" (syncthing--sec-to-uptime
                                       (+ (* 60 10) 59) :full t :pad t)))
  (should (string= "000d 00h 59m 01s" (syncthing--sec-to-uptime
                                       (+ (* 60 59) 1) :full t :pad t)))
  (should (string= "000d 00h 59m 09s" (syncthing--sec-to-uptime
                                       (+ (* 60 59) 9) :full t :pad t)))
  (should (string= "000d 00h 59m 10s" (syncthing--sec-to-uptime
                                       (+ (* 60 59) 10) :full t :pad t)))
  (should (string= "000d 00h 59m 59s" (syncthing--sec-to-uptime
                                       (+ (* 60 59) 59) :full t :pad t)))
  (should (string= "000d 01h 00m 00s" (syncthing--sec-to-uptime
                                       (+ (* 60 60)) :full t :pad t)))

  (should (string= "000d 01h 01m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 1) (* 60 1) 1) :full t :pad t)))
  (should (string= "000d 01h 01m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 1) (* 60 1) 9) :full t :pad t)))
  (should (string= "000d 01h 01m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 1) (* 60 1) 10) :full t :pad t)))
  (should (string= "000d 01h 01m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 1) (* 60 1) 59) :full t :pad t)))
  (should (string= "000d 01h 09m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 1) (* 60 9) 1) :full t :pad t)))
  (should (string= "000d 01h 09m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 1) (* 60 9) 9) :full t :pad t)))
  (should (string= "000d 01h 09m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 1) (* 60 9) 10) :full t :pad t)))
  (should (string= "000d 01h 09m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 1) (* 60 9) 59) :full t :pad t)))
  (should (string= "000d 01h 10m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 1) (* 60 10) 1) :full t :pad t)))
  (should (string= "000d 01h 10m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 1) (* 60 10) 9) :full t :pad t)))
  (should (string= "000d 01h 10m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 1) (* 60 10) 10) :full t :pad t)))
  (should (string= "000d 01h 10m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 1) (* 60 10) 59) :full t :pad t)))
  (should (string= "000d 01h 59m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 1) (* 60 59) 1) :full t :pad t)))
  (should (string= "000d 01h 59m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 1) (* 60 59) 9) :full t :pad t)))
  (should (string= "000d 01h 59m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 1) (* 60 59) 10) :full t :pad t)))
  (should (string= "000d 01h 59m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 1) (* 60 59) 59) :full t :pad t)))

  (should (string= "000d 09h 01m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 9) (* 60 1) 1) :full t :pad t)))
  (should (string= "000d 09h 01m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 9) (* 60 1) 9) :full t :pad t)))
  (should (string= "000d 09h 01m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 9) (* 60 1) 10) :full t :pad t)))
  (should (string= "000d 09h 01m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 9) (* 60 1) 59) :full t :pad t)))
  (should (string= "000d 09h 09m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 9) (* 60 9) 1) :full t :pad t)))
  (should (string= "000d 09h 09m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 9) (* 60 9) 9) :full t :pad t)))
  (should (string= "000d 09h 09m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 9) (* 60 9) 10) :full t :pad t)))
  (should (string= "000d 09h 09m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 9) (* 60 9) 59) :full t :pad t)))
  (should (string= "000d 09h 10m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 9) (* 60 10) 1) :full t :pad t)))
  (should (string= "000d 09h 10m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 9) (* 60 10) 9) :full t :pad t)))
  (should (string= "000d 09h 10m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 9) (* 60 10) 10) :full t :pad t)))
  (should (string= "000d 09h 10m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 9) (* 60 10) 59) :full t :pad t)))
  (should (string= "000d 09h 59m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 9) (* 60 59) 1) :full t :pad t)))
  (should (string= "000d 09h 59m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 9) (* 60 59) 9) :full t :pad t)))
  (should (string= "000d 09h 59m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 9) (* 60 59) 10) :full t :pad t)))
  (should (string= "000d 09h 59m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 9) (* 60 59) 59) :full t :pad t)))

  (should (string= "000d 10h 01m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 10) (* 60 1) 1) :full t :pad t)))
  (should (string= "000d 10h 01m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 10) (* 60 1) 9) :full t :pad t)))
  (should (string= "000d 10h 01m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 10) (* 60 1) 10) :full t :pad t)))
  (should (string= "000d 10h 01m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 10) (* 60 1) 59) :full t :pad t)))
  (should (string= "000d 10h 09m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 10) (* 60 9) 1) :full t :pad t)))
  (should (string= "000d 10h 09m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 10) (* 60 9) 9) :full t :pad t)))
  (should (string= "000d 10h 09m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 10) (* 60 9) 10) :full t :pad t)))
  (should (string= "000d 10h 09m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 10) (* 60 9) 59) :full t :pad t)))
  (should (string= "000d 10h 10m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 10) (* 60 10) 1) :full t :pad t)))
  (should (string= "000d 10h 10m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 10) (* 60 10) 9) :full t :pad t)))
  (should (string= "000d 10h 10m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 10) (* 60 10) 10) :full t :pad t)))
  (should (string= "000d 10h 10m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 10) (* 60 10) 59) :full t :pad t)))
  (should (string= "000d 10h 59m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 10) (* 60 59) 1) :full t :pad t)))
  (should (string= "000d 10h 59m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 10) (* 60 59) 9) :full t :pad t)))
  (should (string= "000d 10h 59m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 10) (* 60 59) 10) :full t :pad t)))
  (should (string= "000d 10h 59m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 10) (* 60 59) 59) :full t :pad t)))

  (should (string= "000d 23h 01m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 23) (* 60 1) 1) :full t :pad t)))
  (should (string= "000d 23h 01m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 23) (* 60 1) 9) :full t :pad t)))
  (should (string= "000d 23h 01m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 23) (* 60 1) 10) :full t :pad t)))
  (should (string= "000d 23h 01m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 23) (* 60 1) 59) :full t :pad t)))
  (should (string= "000d 23h 09m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 23) (* 60 9) 1) :full t :pad t)))
  (should (string= "000d 23h 09m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 23) (* 60 9) 9) :full t :pad t)))
  (should (string= "000d 23h 09m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 23) (* 60 9) 10) :full t :pad t)))
  (should (string= "000d 23h 09m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 23) (* 60 9) 59) :full t :pad t)))
  (should (string= "000d 23h 10m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 23) (* 60 10) 1) :full t :pad t)))
  (should (string= "000d 23h 10m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 23) (* 60 10) 9) :full t :pad t)))
  (should (string= "000d 23h 10m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 23) (* 60 10) 10) :full t :pad t)))
  (should (string= "000d 23h 10m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 23) (* 60 10) 59) :full t :pad t)))
  (should (string= "000d 23h 59m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 23) (* 60 59) 1) :full t :pad t)))
  (should (string= "000d 23h 59m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 23) (* 60 59) 9) :full t :pad t)))
  (should (string= "000d 23h 59m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 23) (* 60 59) 10) :full t :pad t)))
  (should (string= "000d 23h 59m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 23) (* 60 59) 59) :full t :pad t)))
  (should (string= "001d 00h 00m 00s"
                   (syncthing--sec-to-uptime
                    (* 60 60 24) :full t :pad t)))

  (should (string= "001d 01h 01m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 1) 1)
                    :full t :pad t)))
  (should (string= "001d 01h 01m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 1) 9)
                    :full t :pad t)))
  (should (string= "001d 01h 01m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 1) 10)
                    :full t :pad t)))
  (should (string= "001d 01h 01m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 1) 59)
                    :full t :pad t)))
  (should (string= "001d 01h 09m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 9) 1)
                    :full t :pad t)))
  (should (string= "001d 01h 09m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 9) 9)
                    :full t :pad t)))
  (should (string= "001d 01h 09m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 9) 10)
                    :full t :pad t)))
  (should (string= "001d 01h 09m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 9) 59)
                    :full t :pad t)))
  (should (string= "001d 01h 10m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 10) 1)
                    :full t :pad t)))
  (should (string= "001d 01h 10m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 10) 9)
                    :full t :pad t)))
  (should (string= "001d 01h 10m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 10) 10)
                    :full t :pad t)))
  (should (string= "001d 01h 10m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 10) 59)
                    :full t :pad t)))
  (should (string= "001d 01h 59m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 59) 1)
                    :full t :pad t)))
  (should (string= "001d 01h 59m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 59) 9)
                    :full t :pad t)))
  (should (string= "001d 01h 59m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 59) 10)
                    :full t :pad t)))
  (should (string= "001d 01h 59m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 1) (* 60 60 1) (* 60 59) 59)
                    :full t :pad t)))

  (should (string= "032d 01h 01m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 1) 1)
                    :full t :pad t)))
  (should (string= "032d 01h 01m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 1) 9)
                    :full t :pad t)))
  (should (string= "032d 01h 01m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 1) 10)
                    :full t :pad t)))
  (should (string= "032d 01h 01m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 1) 59)
                    :full t :pad t)))
  (should (string= "032d 01h 09m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 9) 1)
                    :full t :pad t)))
  (should (string= "032d 01h 09m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 9) 9)
                    :full t :pad t)))
  (should (string= "032d 01h 09m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 9) 10)
                    :full t :pad t)))
  (should (string= "032d 01h 09m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 9) 59)
                    :full t :pad t)))
  (should (string= "032d 01h 10m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 10) 1)
                    :full t :pad t)))
  (should (string= "032d 01h 10m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 10) 9)
                    :full t :pad t)))
  (should (string= "032d 01h 10m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 10) 10)
                    :full t :pad t)))
  (should (string= "032d 01h 10m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 10) 59)
                    :full t :pad t)))
  (should (string= "032d 01h 59m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 59) 1)
                    :full t :pad t)))
  (should (string= "032d 01h 59m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 59) 9)
                    :full t :pad t)))
  (should (string= "032d 01h 59m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 59) 10)
                    :full t :pad t)))
  (should (string= "032d 01h 59m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 32) (* 60 60 1) (* 60 59) 59)
                    :full t :pad t)))

  (should (string= "367d 01h 01m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 1) 1)
                    :full t :pad t)))
  (should (string= "367d 01h 01m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 1) 9)
                    :full t :pad t)))
  (should (string= "367d 01h 01m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 1) 10)
                    :full t :pad t)))
  (should (string= "367d 01h 01m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 1) 59)
                    :full t :pad t)))
  (should (string= "367d 01h 09m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 9) 1)
                    :full t :pad t)))
  (should (string= "367d 01h 09m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 9) 9)
                    :full t :pad t)))
  (should (string= "367d 01h 09m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 9) 10)
                    :full t :pad t)))
  (should (string= "367d 01h 09m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 9) 59)
                    :full t :pad t)))
  (should (string= "367d 01h 10m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 10) 1)
                    :full t :pad t)))
  (should (string= "367d 01h 10m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 10) 9)
                    :full t :pad t)))
  (should (string= "367d 01h 10m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 10) 10)
                    :full t :pad t)))
  (should (string= "367d 01h 10m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 10) 59)
                    :full t :pad t)))
  (should (string= "367d 01h 59m 01s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 59) 1)
                    :full t :pad t)))
  (should (string= "367d 01h 59m 09s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 59) 9)
                    :full t :pad t)))
  (should (string= "367d 01h 59m 10s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 59) 10)
                    :full t :pad t)))
  (should (string= "367d 01h 59m 59s"
                   (syncthing--sec-to-uptime
                    (+ (* 60 60 24 367) (* 60 60 1) (* 60 59) 59)
                    :full t :pad t))))

(ert-deftest syncthing-convert-to-float ()
  "Convert to float if valid decimal places are specified."
  (syncthing-ert-cleanup)
  (should (eq (type-of (syncthing--maybe-float 0 0)) 'integer))
  (should (eq (type-of (syncthing--maybe-float 0 1)) 'float))
  (should (eq (type-of (syncthing--maybe-float 0 -1)) 'integer))
  (should (eq (type-of (syncthing--maybe-float 1 0)) 'integer))
  (should (eq (type-of (syncthing--maybe-float 1 1)) 'float))
  (should (eq (type-of (syncthing--maybe-float 1 -1)) 'integer))
  (should (eq (type-of (syncthing--maybe-float -1 0)) 'integer))
  (should (eq (type-of (syncthing--maybe-float -1 1)) 'float))
  (should (eq (type-of (syncthing--maybe-float -1 -1)) 'integer)))

(ert-deftest syncthing-calc-scale ()
  "Calculates scaled units properly."
  (syncthing-ert-cleanup)
  (should (string= (syncthing--scale-bytes 0 0) "0B"))
  (should (string= (syncthing--scale-bytes 0 1) "0.0B"))
  (should (string= (syncthing--scale-bytes 0 2) "0.00B"))
  (should (string= (syncthing--scale-bytes 0 3) "0.000B"))

  (should (string= (syncthing--scale-bytes 10 0) "10B"))
  (should (string= (syncthing--scale-bytes 10 1) "10.0B"))
  (should (string= (syncthing--scale-bytes 10 2) "10.00B"))
  (should (string= (syncthing--scale-bytes 10 3) "10.000B"))

  (should (string= (syncthing--scale-bytes 100 0) "100B"))
  (should (string= (syncthing--scale-bytes 100 1) "100.0B"))
  (should (string= (syncthing--scale-bytes 100 2) "100.00B"))
  (should (string= (syncthing--scale-bytes 100 3) "100.000B"))

  (should (string= (syncthing--scale-bytes 1000 0) "1000B"))
  (should (string= (syncthing--scale-bytes 1000 1) "1000.0B"))
  (should (string= (syncthing--scale-bytes 1000 2) "1000.00B"))
  (should (string= (syncthing--scale-bytes 1000 3) "1000.000B"))

  (should (string= (syncthing--scale-bytes 1024 0) "1KiB"))
  (should (string= (syncthing--scale-bytes 1024 1) "1.0KiB"))
  (should (string= (syncthing--scale-bytes 1024 2) "1.00KiB"))
  (should (string= (syncthing--scale-bytes 1024 3) "1.000KiB"))

  (should (string= (syncthing--scale-bytes 1123 0) "1KiB"))
  (should (string= (syncthing--scale-bytes 1123 1) "1.1KiB"))
  (should (string= (syncthing--scale-bytes 1123 2) "1.10KiB"))
  (should (string= (syncthing--scale-bytes 1123 3) "1.097KiB"))

  (should (string= (syncthing--scale-bytes 1567 0) "1KiB"))
  (should (string= (syncthing--scale-bytes 1567 1) "1.5KiB"))
  (should (string= (syncthing--scale-bytes 1567 2) "1.53KiB"))
  (should (string= (syncthing--scale-bytes 1567 3) "1.530KiB"))

  (should (string= (syncthing--scale-bytes (expt 1024 2) 0) "1MiB"))
  (should (string= (syncthing--scale-bytes (expt 1024 2) 1) "1.0MiB"))
  (should (string= (syncthing--scale-bytes (expt 1024 2) 2) "1.00MiB"))
  (should (string= (syncthing--scale-bytes (expt 1024 2) 3) "1.000MiB"))

  (should (string= (syncthing--scale-bytes (expt 1123 2) 0) "1MiB"))
  (should (string= (syncthing--scale-bytes (expt 1123 2) 1) "1.2MiB"))
  (should (string= (syncthing--scale-bytes (expt 1123 2) 2) "1.20MiB"))
  (should (string= (syncthing--scale-bytes (expt 1123 2) 3) "1.203MiB"))

  (should (string= (syncthing--scale-bytes (expt 1567 2) 0) "2MiB"))
  (should (string= (syncthing--scale-bytes (expt 1567 2) 1) "2.3MiB"))
  (should (string= (syncthing--scale-bytes (expt 1567 2) 2) "2.34MiB"))
  (should (string= (syncthing--scale-bytes (expt 1567 2) 3) "2.342MiB"))

  (should (string= (syncthing--scale-bytes (expt 1024 3) 0) "1GiB"))
  (should (string= (syncthing--scale-bytes (expt 1024 3) 1) "1.0GiB"))
  (should (string= (syncthing--scale-bytes (expt 1024 3) 2) "1.00GiB"))
  (should (string= (syncthing--scale-bytes (expt 1024 3) 3) "1.000GiB"))

  (should (string= (syncthing--scale-bytes (expt 1123 3) 0) "1GiB"))
  (should (string= (syncthing--scale-bytes (expt 1123 3) 1) "1.3GiB"))
  (should (string= (syncthing--scale-bytes (expt 1123 3) 2) "1.32GiB"))
  (should (string= (syncthing--scale-bytes (expt 1123 3) 3) "1.319GiB"))

  (should (string= (syncthing--scale-bytes (expt 1567 3) 0) "3GiB"))
  (should (string= (syncthing--scale-bytes (expt 1567 3) 1) "3.6GiB"))
  (should (string= (syncthing--scale-bytes (expt 1567 3) 2) "3.58GiB"))
  (should (string= (syncthing--scale-bytes (expt 1567 3) 3) "3.583GiB"))

  (should (string= (syncthing--scale-bytes (expt 1024 4) 0) "1024GiB"))
  (should (string= (syncthing--scale-bytes (expt 1024 4) 1) "1024.0GiB"))
  (should (string= (syncthing--scale-bytes (expt 1024 4) 2) "1024.00GiB"))
  (should (string= (syncthing--scale-bytes (expt 1024 4) 3) "1024.000GiB"))

  (should (string= (syncthing--scale-bytes (expt 1123 4) 0) "1481GiB"))
  (should (string= (syncthing--scale-bytes (expt 1123 4) 1) "1481.2GiB"))
  (should (string= (syncthing--scale-bytes (expt 1123 4) 2) "1481.22GiB"))
  (should (string= (syncthing--scale-bytes (expt 1123 4) 3) "1481.219GiB"))

  (should (string= (syncthing--scale-bytes (expt 1567 4) 0) "5615GiB"))
  (should (string= (syncthing--scale-bytes (expt 1567 4) 1) "5615.3GiB"))
  (should (string= (syncthing--scale-bytes (expt 1567 4) 2) "5615.34GiB"))
  (should (string= (syncthing--scale-bytes (expt 1567 4) 3) "5615.341GiB")))

(ert-deftest syncthing-convert-to-speed-rate ()
  "Convert number to approximate speed rate."
  (syncthing-ert-cleanup)
  (should (string= (syncthing--bytes-to-rate 0) "0B/s"))
  (should (string= (syncthing--bytes-to-rate 0.0) "0B/s"))
  (should (string= (syncthing--bytes-to-rate -0.0) "-0B/s"))

  (should (string= (syncthing--bytes-to-rate 10) "10B/s"))
  (should (string= (syncthing--bytes-to-rate 100) "100B/s"))
  (should (string= (syncthing--bytes-to-rate 1000) "1000B/s"))
  (should (string= (syncthing--bytes-to-rate 1024) "1KiB/s"))
  (should (string= (syncthing--bytes-to-rate 1123) "1KiB/s"))
  (should (string= (syncthing--bytes-to-rate 1567) "1KiB/s"))
  (should (string= (syncthing--bytes-to-rate (expt 1024 2)) "1MiB/s"))
  (should (string= (syncthing--bytes-to-rate (expt 1123 2)) "1MiB/s"))
  (should (string= (syncthing--bytes-to-rate (expt 1567 2)) "2MiB/s"))
  (should (string= (syncthing--bytes-to-rate (expt 1024 3)) "1GiB/s"))
  (should (string= (syncthing--bytes-to-rate (expt 1123 3)) "1GiB/s"))
  (should (string= (syncthing--bytes-to-rate (expt 1567 3)) "3GiB/s"))
  (should (string= (syncthing--bytes-to-rate (expt 1024 4)) "1024GiB/s"))
  (should (string= (syncthing--bytes-to-rate (expt 1123 4)) "1481GiB/s"))
  (should (string= (syncthing--bytes-to-rate (expt 1567 4)) "5615GiB/s")))

(ert-deftest syncthing-number-grouping ()
  "Group digits properly with separators."
  (syncthing-ert-cleanup)
  (should (string= (syncthing--num-group nil) ""))
  (should (string= (syncthing--num-group 0) "0"))
  (should (string= (syncthing--num-group 10) "10"))
  (should (string= (syncthing--num-group 100) "100"))
  (should (string= (syncthing--num-group 1000) "1 000"))
  (should (string= (syncthing--num-group 10000) "10 000"))
  (should (string= (syncthing--num-group 100000) "100 000"))
  (should (string= (syncthing--num-group 1000000) "1 000 000"))
  (should (string= (syncthing--num-group 10000000) "10 000 000"))
  (should (string= (syncthing--num-group 100000000) "100 000 000"))
  (should (string= (syncthing--num-group 1000000000) "1 000 000 000"))

  (should (string= (syncthing--num-group 0.0) "0.0"))
  (should (string= (syncthing--num-group 10.01) "10.01"))
  (should (string= (syncthing--num-group 100.001) "100.001"))
  (should (string= (syncthing--num-group 1000.0001) "1 000.000 1"))
  (should (string= (syncthing--num-group 10000.00001) "10 000.000 01"))
  (should (string= (syncthing--num-group 100000.000001) "100 000.000 001"))
  (should (string= (syncthing--num-group
                    1234567.8901234 :dec-sep ".") "1 234 567.890 123 4"))

  (should (string= (syncthing--num-group 0 :ths-sep ",") "0"))
  (should (string= (syncthing--num-group 10 :ths-sep ",") "10"))
  (should (string= (syncthing--num-group 100 :ths-sep ",") "100"))
  (should (string= (syncthing--num-group 1000 :ths-sep ",") "1,000"))
  (should (string= (syncthing--num-group 10000 :ths-sep ",") "10,000"))
  (should (string= (syncthing--num-group 100000 :ths-sep ",") "100,000"))
  (should (string= (syncthing--num-group 1000000 :ths-sep ",") "1,000,000"))
  (should (string= (syncthing--num-group 10000000 :ths-sep ",") "10,000,000"))
  (should (string= (syncthing--num-group
                    100000000 :ths-sep ",") "100,000,000"))
  (should (string= (syncthing--num-group
                    1000000000 :ths-sep ",") "1,000,000,000")))

(provide 'syncthing-common-tests)
;;; syncthing-common-tests.el ends here
