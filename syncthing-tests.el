;;; syncthing-tests.el -- tests for syncthing

;;; Code:

(require 'ert)
(require 'syncthing)

(ert-deftest syncthing-new-buffer ()
  (save-window-excursion
    (should (syncthing--switch-to-new-buffer "some-addr"))
    (should (let ((found nil))
              (dolist (buf (buffer-list))
                (when (string-equal "*syncthing(some-addr)*"
                                    (buffer-name buf))
                  (setq found t)))
              found))))

(provide 'syncthing-tests)

;;; syncthing-tests.el ends here
