;; -*- lexical-binding: t; -*-
(defconst empty "<empty>")
(defconst token-apikey "<apikey>")
(defconst version "<version>")
(defconst default-buff-name "*syncthing(Default Localhost)*")
(defconst launch-fail "<fail>")
(defconst launch-success "<success>")

(defconst set-how-customize "<customize>")
(defconst set-how-manual "<manual>")
(defconst set-how-setq "<setq>")

(Given "^Server \"\\([^\"]+\\)\" is running in the background$"
  (lambda (version)
    (setq ecukes-syncthing-container (make-temp-name "ecukes-syncthing-"))
    (setq ecukes-syncthing-mode nil)
    (let* ((waiting-for-start t)
           (wait-for-start-attempts 60)
           (cont-name ecukes-syncthing-container)
           (proc (start-process "ecukes-syncthing" nil
                                "docker" "run" "--rm" "--name" cont-name
                                "--publish" (format "127.0.0.1:%s:8384"
                                                    ecukes-syncthing-port)
                                (ecukes-syncthing-image version)))
           config)
      (push cont-name ecukes-syncthing-containers)

      (while (and waiting-for-start (> wait-for-start-attempts 0))
        (setq wait-for-start-attempts (1- wait-for-start-attempts))
        (let ((url-request-method "GET") (url-show-status nil) resp)
          (ignore url-request-method url-show-status)
          (with-temp-buffer
            (condition-case nil
                (progn
                  (url-insert-file-contents
                   (format "%s://%s:%s/rest/noauth/health"
                           ecukes-syncthing-proto
                           ecukes-syncthing-host
                           ecukes-syncthing-port))
                  (setq resp (json-parse-buffer :object-type 'alist))
                  (when (string= "OK" (alist-get 'status resp))
                    (setq waiting-for-start nil)))
              (t nil))))
        (sleep-for 0.1))

      (should-not waiting-for-start)

      (with-current-buffer (get-buffer-create cont-name)
        (delete-region (point-min) (point-max)))
      (call-process "docker" nil (get-buffer-create cont-name) nil
                    "exec" cont-name "cat" "/var/syncthing/config/config.xml")
      (with-current-buffer (get-buffer-create cont-name)
        (setq config (libxml-parse-xml-region (point-min) (point-max))))
      (should config)

      (should (string= "true" (alist-get 'enabled (cadr (assoc 'gui config)))))
      (let ((tmp (caddr (assoc 'apikey (assoc 'gui config)))))
        (should-not (string= "" tmp))
        (setq ecukes-syncthing-apikey tmp))
      (let ((tmp (cadr (assoc 'device config))))
        (should tmp)
        (setq ecukes-syncthing-device tmp)))

    (let (proc)
      (unwind-protect
          (condition-case err
              (setq proc (open-network-stream ecukes-syncthing-container nil
                                              ecukes-syncthing-host
                                              ecukes-syncthing-port))
            (error (should-not err)))
        (when proc
          (delete-process proc))))))

(And "^I am running client in \"\\([^\"]+\\)\" mode$"
  (lambda (arg)
    (setq ecukes-syncthing-mode arg)))

(When "^I have no API token set$"
  (lambda ()
    (let ((inhibit-message t))
      (customize-set-variable 'syncthing-default-server-token ""))
    (should (string= syncthing-default-server-token ""))))

(And "^I launch client \"\\([^\"]+\\)\"$"
  (lambda (how)
    (if (string= how launch-success)
        (cond ((string= ecukes-syncthing-mode "non-interactive")
               (let ((inhibit-message t)) (syncthing)))
              ((string= ecukes-syncthing-mode "interactive")
               (let ((inhibit-message t)) (call-interactively 'syncthing)))
              (t (error "Bad mode" ecukes-syncthing-mode)))
      (cond ((string= ecukes-syncthing-mode "non-interactive")
             (condition-case nil
                 (progn (let ((inhibit-message t)) (syncthing)) (should nil))
               (user-error t)))
            ((string= ecukes-syncthing-mode "interactive")
             (condition-case nil
                 (progn
                   (let ((inhibit-message t))
                     (call-interactively 'syncthing))
                   (should nil))
               (user-error t)))
            (t (error "Bad mode" ecukes-syncthing-mode))))))

(Then "^client redirects to token customization$"
  (lambda ()
    (should (string-match "Customize Option: Syncthing Default Server Token"
                          (buffer-name (current-buffer))))))

(When "^I set a API token in \"\\([^\"]+\\)\" to \"\\([^\"]+\\)\"$"
  (lambda (how raw-token)
    (should (string= syncthing-default-server-token ""))
    (cond ((string= how set-how-manual)
           (cond ((string= raw-token empty)
                  (let ((inhibit-message t))
                    (customize-set-variable 'syncthing-default-server-token ""))
                  (should (string= syncthing-default-server-token "")))
                 ((string= raw-token token-apikey)
                  (let ((inhibit-message t))
                    (customize-set-variable 'syncthing-default-server-token
                                            ecukes-syncthing-apikey))
                  (should (string= syncthing-default-server-token
                                   ecukes-syncthing-apikey)))
                 (t (error "Unhandled case: '%s'" token))))
          ((string= how set-how-setq)
           (cond ((string= raw-token empty)
                  (setq syncthing-default-server-token "")
                  (should (string= syncthing-default-server-token "")))
                 ((string= raw-token token-apikey)
                  (setq syncthing-default-server-token ecukes-syncthing-apikey)
                  (should (string= syncthing-default-server-token
                                   ecukes-syncthing-apikey)))
                 (t (error "Unhandled case: '%s'" token))))
          ((string= how set-how-customize)
           ;; navigate to token value input in Customize buffer
           (let ((inhibit-message t))
             (dotimes (_ 8)
               (execute-kbd-macro (kbd "TAB"))))

           (cond ((string= raw-token empty)
                  ;; no insert
                  )
                 ((string= raw-token token-apikey)
                  (should-not (string= "" ecukes-syncthing-apikey))
                  (insert ecukes-syncthing-apikey))
                 (t (error "Unhandled case: '%s'" token)))

           ;; navigate to =Apply= button in Customize buffer
           (goto-char (point-min))
           (let ((inhibit-message t))
             (dotimes (_ 6)
               (execute-kbd-macro (kbd "TAB")))
             (when-let* ((wid (widget-at (point)))
                         (active (not (widget-get wid :inactive))))
               ;; apply for the current session only
               (execute-kbd-macro (kbd "RET"))))

           (cond ((string= raw-token empty)
                  (should (string= syncthing-default-server-token "")))
                 ((string= raw-token token-apikey)
                  (should (string= syncthing-default-server-token
                                   ecukes-syncthing-apikey)))
                 (t (error "Unhandled case: '%s'" token)))

           (kill-buffer))
          (t (error "Unhandled case: '%s'" how)))))

(Then "^client buffer header contains \"\\([^\"]+\\)\"$"
  (lambda (header)
    ;; TODO: Why doesn't it switch automatically?
    ;; TODO: Make an interactive func to switch around Syncthing-only buffers
    (dolist (buff (buffer-list))
      (when (string= default-buff-name (buffer-name buff))
        (switch-to-buffer buff)))
    (cond ((string= header empty)
           (should-not header-line-format))
          ((string= header version)
           (string-match ecukes-syncthing-version (or header-line-format "")))
          (t (error "Unhandled case: '%s'" header)))))

(After
 (ignore-error (kill-buffer default-buff-name))
 (let ((cont-name ecukes-syncthing-container))
   (call-process "docker" nil nil nil "kill" cont-name)
   (let* ((waiting-for-end t)
          (wait-for-end-attempts 60))
     (setq ecukes-syncthing-containers
           (delete cont-name ecukes-syncthing-containers))
     (while (and waiting-for-end (> wait-for-end-attempts 0))
       (setq wait-for-end-attempts (1- wait-for-end-attempts))
       (let ((url-request-method "GET") (url-show-status nil) resp)
         (ignore url-request-method url-show-status)
         (url-retrieve
          (format "%s://%s:%s/rest/noauth/health"
                  ecukes-syncthing-proto
                  ecukes-syncthing-host
                  ecukes-syncthing-port)
          (lambda (status)
            (when-let* ((err (plist-get status :error))
                        (is-conn-err (eq (cadr err) 'connection-failed)))
              (setq waiting-for-end nil)))))
       (sleep-for 0.1)))
   (setq syncthing-default-server-token
         (eval (car (get 'syncthing-default-server-token 'standard-value))))

   (makunbound 'ecukes-syncthing-container)))

(Before
 )
