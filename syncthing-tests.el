;;; syncthing-tests.el -- tests for syncthing -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Code:

(require 'ert)

(require 'syncthing-custom)
(require 'syncthing-network)
(require 'syncthing-state)

(require 'syncthing)


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

(ert-deftest syncthing-run-customize ()
  "Run `customize-variable' on missing API token."
  (let (called args throw)
    (syncthing-ert-cleanup)
    (advice-add 'syncthing--interactive-common
                :override
                (lambda (&rest rest) (setq args rest)))
    (advice-add 'customize-variable
                :override
                (lambda (&rest _) (setq called t)))
    (syncthing)
    (advice-remove 'syncthing--interactive-common
                   (lambda (&rest rest) (setq args rest)))
    (advice-remove 'customize-variable
                   (lambda (&rest _) (setq called t)))
    (should
     (string= (format "%s" args)
         (format "%s"`(,syncthing-default-name ,syncthing-base-url ""))))
    (should called)
    (condition-case nil
        (apply 'syncthing--interactive-common args)
      (user-error (setq throw t)))
    (should throw)))

(ert-deftest syncthing-run-customize-keep-focused ()
  "Run `customize-variable' on missing API token."
  (let (args throw)
    (syncthing-ert-cleanup)
    (advice-add 'syncthing--interactive-common
                :override
                (lambda (&rest rest) (setq args rest)))
    ;; Creating customization items... etc noise
    (advice-add 'message :override (lambda (&rest _)))
    (syncthing)
    (advice-remove 'syncthing--interactive-common
                   (lambda (&rest rest) (setq args rest)))
    (advice-remove 'message (lambda (&rest _)))
    (should
     (string= (format "%s" args)
         (format "%s"`(,syncthing-default-name ,syncthing-base-url ""))))
    (condition-case nil
        (apply 'syncthing--interactive-common args)
      (user-error (setq throw t)))
    (should throw)
    (should (string= "*Customize Option: Syncthing Default Server Token*"
                     (buffer-name (current-buffer))))))

(ert-deftest syncthing-use-default-token ()
  "If set to non-empty, use default token."
  (let* ((dummy "meow")
         (syncthing-default-server-token dummy)
         called args)
    (syncthing-ert-cleanup)
    (advice-add 'syncthing--interactive-common
                :override
                (lambda (&rest rest) (setq args rest)))
    (advice-add 'customize-variable
                :override
                (lambda (&rest _) (setq called t)))
    (syncthing)
    (advice-remove 'syncthing--interactive-common
                   (lambda (&rest rest) (setq args rest)))
    (advice-remove 'customize-variable
                   (lambda (&rest _) (setq called t)))
    (should
     (string= (format "%s" args)
         (format "%s"`(,syncthing-default-name ,syncthing-base-url ,dummy))))
    (should (not called))))

(ert-deftest syncthing-refresh-reject ()
  "Reject activating the refresh mode if not in `syncthing-mode'."
  (with-temp-buffer
    (syncthing-ert-cleanup)
    (let (called)
      (condition-case err
          (progn
            (advice-add 'auto-revert-mode
                        :override
                        (lambda (&rest _) (setq called t)))
            (syncthing-auto-refresh-mode)
            (advice-remove 'auto-revert-mode
                           (lambda (&rest _) (setq called t))))
        (user-error (should (string-match "not .*?syncthing-mode.*?"
                                          (error-message-string err)))
                    (setq called t))
        (t (message "Wrong error: %s" (error-message-string err))
           (should nil)))
      (should called))))

(ert-deftest syncthing-refresh-activate ()
  "Check whether `auto-revert-mode' gets activated."
  (with-temp-buffer
    (syncthing-ert-cleanup)
    (let (called)
      (advice-add 'auto-revert-mode
                  :override
                  (lambda (&rest _) (setq called t)))
      (syncthing-mode)
      (syncthing-auto-refresh-mode)
      (advice-remove 'auto-revert-mode
                     (lambda (&rest _) (setq called t)))
      (should called))))

(ert-deftest syncthing-request-apply ()
  "Check endpoint formatting for `syncthing--request'."
  (let* ((name "dummy-name")
         (token "dummy-token")
         (url "https://base-url.tld:1234")
         (server (syncthing--server :name name :url url :token token))
         args)
    (syncthing-ert-cleanup)
    (advice-add 'syncthing--request
                :override
                (lambda (&rest largs) (setq args largs)))
    (syncthing-request server "GET" "something")
    (should (string= (format "%s" args)
                     (format "(GET %s/something %s nil)" url token)))
    (syncthing-request server "GET" "/something")
    (should (string= (format "%s" args)
                     (format "(GET %s//something %s nil)" url token)))
    (advice-remove 'syncthing--request
                   (lambda (&rest largs) (setq args largs)))))

(ert-deftest syncthing-multi-client-bug ()
  "Ensure a new client buffer creates own `syncthing-server' instance."
  (syncthing-ert-cleanup)
  (should (not syncthing--servers))

  (advice-add 'auto-revert-mode :override (lambda (&rest _)))
  (syncthing-with-base "dummy1" "url1" "token1")
  (set-buffer (window-buffer (selected-window)))
  (should
   (not (eq nil (string-match
                 "dummy1"
                 (buffer-name (current-buffer))))))
  (should (string= "dummy1" (syncthing-server-name syncthing-server)))
  (should (eq 1 (length syncthing--servers)))

  (syncthing-with-base "dummy2" "url2" "token2")
  (set-buffer (window-buffer (selected-window)))
  (should
   (not (eq nil (string-match
                 "dummy2"
                 (buffer-name (current-buffer))))))
  (should (string= "dummy2" (syncthing-server-name syncthing-server)))
  (should (eq 2 (length syncthing--servers)))

  (advice-add 'message :override (lambda (&rest _)))
  (kill-buffer (get-buffer (syncthing-buffer-name syncthing-buffer)))
  (advice-remove 'message (lambda (&rest _)))
  (should (eq 1 (length syncthing--servers)))

  (advice-add 'message :override (lambda (&rest _)))
  (kill-buffer (get-buffer (syncthing-buffer-name syncthing-buffer)))
  (advice-remove 'message (lambda (&rest _)))
  (should (eq 0 (length syncthing--servers)))

  (should (eq nil syncthing--servers))
  (advice-remove 'auto-revert-mode (lambda (&rest _))))

(provide 'syncthing-tests)
;;; syncthing-tests.el ends here
