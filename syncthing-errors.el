;;; syncthing-errors.el --- Client for Syncthing -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Code:

(define-error 'syncthing-error "Generic syncthing error")

(define-error 'syncthing-error-cant-edit-buffer
  "You can not edit this part of the Syncthing buffer"
  'syncthing-error)

(define-error 'syncthing-error-cant-authenticate
  "Failed to authenticate, check the token!"
  'syncthing-error)

(define-error 'syncthing-error-failed-response
  "Failed to handle response for %s"
  'syncthing-error)

(provide 'syncthing-errors)
;;; syncthing-errors.el ends here
