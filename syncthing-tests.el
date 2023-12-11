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

(provide 'syncthing-tests)

;;; syncthing-tests.el ends here
