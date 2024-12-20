;;; syncthing-network-tests.el -- tests -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Code:

(require 'ert)
(require 'cl-lib)

(require 'syncthing-errors)
(require 'syncthing-network)
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

(ert-deftest syncthing-ping-failure ()
  "Throw an auth error on bad token."
  (syncthing-ert-cleanup)
  (let ((server (syncthing--server)) thrown)
    (advice-add 'url-insert-file-contents
                :override
                (lambda (&rest _) (signal 'file-error "")))
    (condition-case err
        (syncthing--ping server)
      (syncthing-error
       (should (string= (error-message-string err)
                        (get 'syncthing-error-cant-authenticate
                             'error-message)))
             (setq thrown t)))
    (should thrown)
    (advice-remove 'url-insert-file-contents
                   (lambda (&rest _) (signal 'file-error "")))))

(ert-deftest syncthing-ping-success ()
  "Check for successful pass."
  (syncthing-ert-cleanup)
  (let ((server (syncthing--server)) (ret "dummy") thrown)
    (advice-add 'url-insert-file-contents
                :override
                `(lambda (&rest _) (insert ,ret)))
    (condition-case nil
        (should (string= (syncthing--ping server) ret))
      (syncthing-error (setq thrown t)))
    (should-not thrown)
    (advice-remove 'url-insert-file-contents
                   `(lambda (&rest _) (insert ,ret)))))

(ert-deftest syncthing-request-url-failure ()
  "Throw a failed response error."
  (syncthing-ert-cleanup)
  (let ((url "some-url") thrown)
    (advice-add 'url-insert-file-contents
                :override
                (lambda (&rest _) (signal 'file-error "")))
    (condition-case err
        (syncthing--request "GET" url "token" nil)
      (syncthing-error
       (should (string= (error-message-string err)
                        (get 'syncthing-error-failed-response
                             'error-message)))
       (should (string= (cdr err) url))
       (setq thrown t)))
    (should thrown)
    (advice-remove 'url-insert-file-contents
                   (lambda (&rest _) (signal 'file-error "")))))

(ert-deftest syncthing-request-url-success ()
  "Check for successful pass."
  (syncthing-ert-cleanup)
  (let ((url "some-url")
        (json "{\"result\": \"ok\"}")
        thrown)
    (advice-add 'url-insert-file-contents
                :override
                (lambda (&rest _) (insert json) (goto-char 0)))
    (condition-case nil
        (should (string=
                 (format "%s" (syncthing--request "GET" url "token" nil))
                 (format "%s" '((result . "ok")))))
      (syncthing-error (setq thrown t)))
    (should-not thrown)
    (advice-remove 'url-insert-file-contents
                   (lambda (&rest _) (insert json) (goto-char 0)))))

(ert-deftest syncthing-request-call-success ()
  "Throw an auth error on bad token."
  (syncthing-ert-cleanup)
  (let* ((method "GET")
         (endpoint "rest/some")
         (url "some-url")
         (token "some-token")
         (data "some-data")
         (server (syncthing--server :url url :token token))
         args)
    (advice-add 'syncthing--request
                :override
                (lambda (&rest largs) (setq args largs)))
    (syncthing-request server method endpoint data)
    (should (string= (format "%s" args)
                     (format "(%s %s/%s %s %s)"
                             method url endpoint token data)))
    (advice-remove 'syncthing--request
                   (lambda (&rest largs) (setq args largs)))))

(provide 'syncthing-network-tests)
;;; syncthing-network-tests.el ends here
