(defun demo-server-load-handlers ()
  (let (handlers)
    (dolist (item (directory-files "../testdata"))
      (when (string-suffix-p ".json" item)
        (with-temp-buffer
          (insert-file-contents (file-name-concat ".." "testdata" item))
          (let ((tmp (buffer-substring-no-properties
                      (line-beginning-position) (1+ (line-end-position)))))
            (delete-region (line-beginning-position) (1+ (line-end-position)))

            (when (string-match (rx line-start (group (+? digit)) (literal "|")
                                    (group (+? anychar)) line-end)
                                tmp)
              (push `(,(match-string 2 tmp)
                      ,(string-to-number (match-string 1 tmp))
                      ,(buffer-string))
                    handlers))))))
    handlers))

(defun demo-server-eval-sexprs (data)
  (with-temp-buffer
    (insert data)
    (goto-char (point-min))
    (let ((last 0) tmp)
      (while (string-match
              (rx line-start (*? anychar)
                  (group (literal "#(") (*? anychar) (literal ")"))
                  (? ",") line-end)
              (buffer-string) last)
        (setq last (match-beginning 1))
        (setq tmp (match-string 1 (buffer-string)))
        (goto-char (1+ (match-beginning 1)))
        (delete-region (1+ (match-beginning 1)) (1+ (match-end 1)))
        (insert (format "%s" (eval (read (substring tmp 1)))))))
    (buffer-string)))

(defun demo-server-router (proc req-line)
  (unwind-protect
      (process-send-string
       proc
       ;; somewhat-(cond)
       (let (default code)
         (catch 'done
           (dolist (handler (or (get 'demo-server-load-handlers 'cache)
                                (progn
                                  (put 'demo-server-load-handlers 'cache
                                       (demo-server-load-handlers))
                                  (get 'demo-server-load-handlers 'cache))))
             (let ((path (nth 0 handler)) (data (nth 2 handler)))
               (when (string-match path req-line)
                 (setq code (nth 1 handler))
                 (setq default
                       (string-join `(,(format "HTTP/1.1 %s" code)
                                      "Content-Type: application/json" ""
                                      ;; re-eval per request
                                      ,(demo-server-eval-sexprs data))
                                    "\r\n"))
                 (throw 'done t)))))
         (message "%s %s %s"
                  (format-time-string "[%d/%b/%Y %H:%M:%S]" (current-time))
                  (when (string-match "\n" req-line)
                    (substring req-line 0 (match-beginning 0)))
                  (or code "404"))
         (or default
             (string-join '("HTTP/1.1 404 Not Found"
                            "Content-Type: text/plain" ""
                            "Page not found!\n") "\r\n"))))
    (process-send-eof proc)))

(defun demo-server-filter (proc string)
  "Handle the incoming data from the network connection."
  (let ((req-line (decode-coding-string string 'utf-8))
        (handlers ))
    (demo-server-router proc req-line)))

(defun demo-server-sentinel (proc event)
  "Handle the closing of a network connection."
  (when (string-match "closed" event)
    (message "Connection closed: %s" proc)))

(let ((proc-name "syncthing-demo-server"))
  (mapcar (lambda (proc)
            (when (string= (process-name proc) proc-name)
              (message "Restart!")
              (delete-process proc)))
          (process-list))
  (put 'demo-server-load-handlers 'cache nil)
  (make-network-process
   :name proc-name
   :server t
   :host (or (getenv "DEMO_HOST") "127.0.0.1")
   :service (string-to-number (or (getenv "DEMO_PORT") "5000"))
   :family 'ipv4
   :sentinel 'demo-server-sentinel
   :filter 'demo-server-filter))
