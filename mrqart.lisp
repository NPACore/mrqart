;; * MRQART

;; "MR Quality Assurance in Real Time" is software to monitor unwanted or unexpected changes to sequence parameters. It expects to sit on the server side of a samba service and ingest files sent directly from a siemens MR control computer to generate a near real time protocol quality assurance report and alert on protocol changes.

;; ** Technology
;; This file documents code written for [[https://ciel-lang.org][ciel-lang]]'s batteries included Common Lisp package. Lips is an unusual choice but picked as a weekend project, inspired by
;;  * [[https://lispcookbook.github.io/cl-cookbook/websockets.html][easy web sockets]] documented on the common lisp wiki
;;  * ciel bundled [[https://ciel-lang.org/#/scripting?id=auto-reload][inotify]] for finding new files
;;  * tight REPL, no-restart (interative) code changes to non-blocking inotify (and hopefully web server and socket) threads.


;; *** Editor setup
;; This file was created in emacs using ~outorg~ (on top of ~outshine~) to style the comments with org-mode markup.

;; #+begin_src elisp
;; (sly) ; start interactive REPL with sbcl (or ciel binary)
;; (defvar outline-minor-mode-prefix "\M-#")
;; (use-package outorg  :ensure t
;;   :config
;;   (outline-minor-mode 1)
;;   (oushine-mode 1))
;; (outorg-edit-as-org)
;; #+end_src

;; "^L" (page break) also separates sectons. C-x [ or ] to jump around

;; ** Lisp packaging
;; This originally started as a script ~(in-package :ciel-user)~. Switching to ~defpackage~ meant losing the original ~:local-nicknames~ imployed by ciel. Those have been copied back in. ( https://github.com/ciel-lang/CIEL/issues/81)

;; #-ciel (ql:quickload :ciel)

(defpackage mrqart
  (:use :cl :ciel :mrqart-dicom)
  (:import-from :mrqart-dicom *dcmtags*)
  (:import-from :mrqart-servers ws-broad ws-server *ws-port* start-webapp)
  (:local-nicknames
   (:dcm :mrqart-dicom)
   (:filesystem :uiop/filesystem)
   (:notify :org.shirakumo.file-notify)
   (:routes :easy-routes))
  (:export get-dcm-values *dcmtags* *db* launch-all))

;; packages not in ciel
;;(ql:quickload "websocket-driver")
;;(ql:quickload "clack")

(in-package :mrqart)

;; *** Current sessions shim block
(defvar *shimlist*
  (list)
  "list of protocol + B0shim to track shim groups. To be reset for each new project.")

(defvar *sequences*
  (dict)
  "MR sequences. Key is seqnumber. value is dict with count (n dcm), paramaters, ideal parameters")




;; ** filesystem
;; Watching what will be a samba input folder for dicom files.
;; /data/dicomstream/20241016.MRQART_test.24.10.16_16_50_16_DST_1.3.12.2.1107.5.2.43.67078/001_000017_000066.dcm
;; NOTE: we may want to watch more than one scanner at a time!
;; NOTE: we should unwatch directories after some amount of time? (or restart the server)

(defun process-new-dicom (fname)
  (log:info fname)
  (let ((vals (dcm:get-dcm-values fname *dcmtags*)))
    (ws-broad nil (format nil "~A~%" vals))))

(defun on-notify (file event)
  "Act on new files."
  ;; only care when we see something new
  ;; (log:info "inotify sees" event file)
  (cond
    ;; watch folder
    ((and (equal event :create) (filesystem:directory-exists-p file))
     (log:info "new dir" file)
     (notify:watch file))
    ;; DICOM
    ((and (equal event :create)
      (filesystem:file-exists-p file)
      (dicom-fname-p file))
     (log:info "process dicom" file)
     (process-new-dicom file))))


;; ** ENTER/START
;; Start the file system monitor. Start the web server. Start socks server.

;; TODO: sim should be specified by command line arg
(defvar *ws-handler* nil "Clack server for web sockets")
(defun launch-all ()
    "Launch everything!"
    (notify:watch "sim/")
    (notify:with-events (file change :timeout T)
    (on-notify file change))
    (start-webapp)
    (setq *ws-handler* (clack:clackup #'ws-server :port *ws-port*)))
