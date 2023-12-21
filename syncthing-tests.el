;;; syncthing-tests.el -- tests for syncthing

;;; Code:

(require 'ert)
(require 'syncthing)


(defun syncthing-cleanup ()
  (dolist (buf (buffer-list))
    (unless (or (string-match-p (regexp-quote "*Messages*") (buffer-name buf)))
      (kill-buffer buf))))

(ert-deftest syncthing-calc-scale ()
  "Ensure `syncthing--scale-bytes' calculates units properly."
  (syncthing-cleanup)
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

  (should (string= (syncthing--scale-bytes 1048576 0) "1MiB"))
  (should (string= (syncthing--scale-bytes 1048576 1) "1.0MiB"))
  (should (string= (syncthing--scale-bytes 1048576 2) "1.00MiB"))
  (should (string= (syncthing--scale-bytes 1048576 3) "1.000MiB"))

  (should (string= (syncthing--scale-bytes 1261129 0) "1MiB"))
  (should (string= (syncthing--scale-bytes 1261129 1) "1.2MiB"))
  (should (string= (syncthing--scale-bytes 1261129 2) "1.20MiB"))
  (should (string= (syncthing--scale-bytes 1261129 3) "1.203MiB"))

  (should (string= (syncthing--scale-bytes 2455489 0) "2MiB"))
  (should (string= (syncthing--scale-bytes 2455489 1) "2.3MiB"))
  (should (string= (syncthing--scale-bytes 2455489 2) "2.34MiB"))
  (should (string= (syncthing--scale-bytes 2455489 3) "2.342MiB"))

  (should (string= (syncthing--scale-bytes 1073741824 0) "1GiB"))
  (should (string= (syncthing--scale-bytes 1073741824 1) "1.0GiB"))
  (should (string= (syncthing--scale-bytes 1073741824 2) "1.00GiB"))
  (should (string= (syncthing--scale-bytes 1073741824 3) "1.000GiB"))

  (should (string= (syncthing--scale-bytes 1416247867 0) "1GiB"))
  (should (string= (syncthing--scale-bytes 1416247867 1) "1.3GiB"))
  (should (string= (syncthing--scale-bytes 1416247867 2) "1.32GiB"))
  (should (string= (syncthing--scale-bytes 1416247867 3) "1.319GiB"))

  (should (string= (syncthing--scale-bytes 3847751263 0) "3GiB"))
  (should (string= (syncthing--scale-bytes 3847751263 1) "3.6GiB"))
  (should (string= (syncthing--scale-bytes 3847751263 2) "3.58GiB"))
  (should (string= (syncthing--scale-bytes 3847751263 3) "3.583GiB"))

  (should (string= (syncthing--scale-bytes 1099511627776 0) "1024GiB"))
  (should (string= (syncthing--scale-bytes 1099511627776 1) "1024.0GiB"))
  (should (string= (syncthing--scale-bytes 1099511627776 2) "1024.00GiB"))
  (should (string= (syncthing--scale-bytes 1099511627776 3) "1024.000GiB"))

  (should (string= (syncthing--scale-bytes 1590446354641 0) "1481GiB"))
  (should (string= (syncthing--scale-bytes 1590446354641 1) "1481.2GiB"))
  (should (string= (syncthing--scale-bytes 1590446354641 2) "1481.22GiB"))
  (should (string= (syncthing--scale-bytes 1590446354641 3) "1481.219GiB"))

  (should (string= (syncthing--scale-bytes 6029426229121 0) "5615GiB"))
  (should (string= (syncthing--scale-bytes 6029426229121 1) "5615.3GiB"))
  (should (string= (syncthing--scale-bytes 6029426229121 2) "5615.34GiB"))
  (should (string= (syncthing--scale-bytes 6029426229121 3) "5615.341GiB")))

(ert-deftest syncthing-run-customize ()
  "Run `customize-variable' on missing API token."
  (let (called args throw)
    (syncthing-cleanup)
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

(ert-deftest syncthing-use-default-token ()
  "If set to non-empty, use default token."
  (let* ((dummy "meow")
         (syncthing-default-server-token dummy)
         called args)
    (syncthing-cleanup)
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
    (syncthing-cleanup)
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
    (syncthing-cleanup)
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
    (syncthing-cleanup)
    (advice-add 'syncthing--request
                :override
                (lambda (&rest largs) (setq args largs)))
    (syncthing-request server "GET" "something")
    (should (string= (format "%s" args)
                     (format "(GET %s/something %s)" url token)))
    (syncthing-request server "GET" "/something")
    (should (string= (format "%s" args)
                     (format "(GET %s//something %s)" url token)))
    (advice-remove 'syncthing--request
                   (lambda (&rest largs) (setq args largs)))))

(provide 'syncthing-tests)

;;; syncthing-tests.el ends here
