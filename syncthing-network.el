(defun syncthing--ping (server)
  "Check whether we can use the API at SERVER with TOKEN."
  (syncthing-trace)
  (let ((url-request-method "GET")
        (url-request-extra-headers
         `(("X-Api-Key" . ,(syncthing-server-token server))))
        (url-show-status (null syncthing-no-upstream-noise)))
    (ignore url-request-method url-request-extra-headers)
    (condition-case-unless-debug nil
        (with-temp-buffer
          (url-insert-file-contents
           (format "%s/rest/system/ping" (syncthing-server-url server))))
      (file-error (error "Failed to authenticate, check the token!")))))

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
    (condition-case-unless-debug nil
        (with-temp-buffer
          (url-insert-file-contents url)
          (json-parse-buffer :object-type 'alist
                             :array-type 'list
                             :null-object nil
                             :false-object nil))
      (file-error (error "Failed to handle response for %s" url)))))

(provide 'syncthing-network)
