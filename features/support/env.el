;; -*- lexical-binding: t; -*-
(require 'f)

(defvar syncthing-support-path
  (f-dirname load-file-name))

(defvar syncthing-features-path
  (f-parent syncthing-support-path))

(defvar syncthing-root-path
  (f-parent syncthing-features-path))

(add-to-list 'load-path syncthing-root-path)

;; Ensure that we don't load old byte-compiled versions
(let ((load-prefer-newer t))
  (require 'syncthing)
  (require 'espuds)
  (require 'ert))

(defvar ecukes-syncthing-version "1.29.6")
(defvar ecukes-syncthing-image (format "syncthing/syncthing:%s"
                                       ecukes-syncthing-version))
(defvar ecukes-syncthing-proto "http")
(defvar ecukes-syncthing-host "127.0.0.1")
(defvar ecukes-syncthing-port "4567")
(defvar ecukes-syncthing-containers nil)

(Setup
 (setq ecukes-syncthing-proto
       (or (getenv "ECUKES_SYNCTHING_PROTO") "http"))
 (setq ecukes-syncthing-host
       (or (getenv "ECUKES_SYNCTHING_HOST") "127.0.0.1"))
 (setq ecukes-syncthing-port
       (string-to-number (or (getenv "ECUKES_SYNCTHING_PORT") "4567")))
 (setq ecukes-syncthing-containers nil))

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 (dolist (cont ecukes-syncthing-containers)
   (call-process "docker" nil nil nil "kill" cont)))
