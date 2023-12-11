;;; syncthing-tests.el -- tests for syncthing

;;; Code:

(require 'ert)
(require 'syncthing)


(defun syncthing-cleanup ()
  (dolist (buf (buffer-list))
    (unless (or (string-match-p (regexp-quote "*Messages*") (buffer-name buf)))
      (kill-buffer buf))))

(ert-deftest syncthing-run-customize ()
  "Run `customize-variable' on missing API token."
  (let (called args)
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
    (should called)))

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
