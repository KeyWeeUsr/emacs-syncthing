;;; syncthing-network.el --- Client for Syncthing -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Code:

(require 'url-handlers)

(require 'syncthing-common)
(require 'syncthing-custom)
(require 'syncthing-errors)
(require 'syncthing-state)

(defun syncthing--ping (server)
  "Check whether we can use the API at SERVER with TOKEN."
  (syncthing-trace)
  (let ((url-request-method "GET")
        (url-request-extra-headers
         `(("X-Api-Key" . ,(syncthing-server-token server))))
        (url-show-status (null syncthing-no-upstream-noise)))
    (ignore url-request-method url-request-extra-headers)
    (condition-case nil
        (with-temp-buffer
          ;; TODO: fetch status and message, compare, throw on mismatch
          (url-insert-file-contents
           (format "%s/rest/system/ping" (syncthing-server-url server)))
          (buffer-string))
      (file-error (signal 'syncthing-error-cant-authenticate nil)))))

(defun syncthing--request (method url token &rest data)
  "Send authenticated HTTP request to Syncthing REST API.
Argument METHOD HTTP method/verb.
Argument URL API to call.
Optional argument DATA Data to send.
Argument TOKEN API token."
  (syncthing-trace)
  (let ((url-request-method method)
        (url-request-data data)
        (url-request-extra-headers `(("X-Api-Key" . ,token)))
        (url-show-status (null syncthing-no-upstream-noise)))
    (ignore url-request-method method
            url-request-data data
            url-request-extra-headers)
    (condition-case nil
        (with-temp-buffer
          ;; TODO: fetch status and message, compare, throw on mismatch
          (url-insert-file-contents url)
          (json-parse-buffer :object-type 'alist
                             :array-type 'list
                             :null-object nil
                             :false-object nil))
      (file-error (signal 'syncthing-error-failed-response url)))))

(defun syncthing-request (server method endpoint &rest data)
  "Return SERVER response for METHOD at ENDPOINT for request with DATA."
  (syncthing-trace)
  (apply #'syncthing--request
         (append (list method
                       (format "%s/%s" (syncthing-server-url server) endpoint)
                       (syncthing-server-token server))
                 data)))

(provide 'syncthing-network)
;;; syncthing-network.el ends here
