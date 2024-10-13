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
;; (use-package :outorg :config (outorg-edit-minor-mode))
;; (org-edit-as-org)
;; #+end_src
;; ** Lisp packaging
;; This originally started as a script ~(in-package :ciel-user)~. Switching to ~defpackage~ meant losing the original ~:local-nicknames~ imployed by ciel. Those have been copied back in. ( https://github.com/ciel-lang/CIEL/issues/81)

(defpackage mrqart
  (:use :cl :ciel)
  (:local-nicknames
   (:filesystem :uiop/filesystem)
   (:notify :org.shirakumo.file-notify))
  (:export get-dcm-values *dcmtags*))

;; #-ciel (ql:quickload :ciel)
(in-package :mrqart)

;; 
;; ** Globals
;; These ~defvar~ globals track state

;; *** Database
(defvar *db*
  (dbi:connect :sqlite3
               :database-name "mrqart.sqlite3")
  "Database

;; *** Current sessions shim block


for last seen and expected protocol values.")
(defvar *shimlist*
  (list)
  "list of protocol + B0shim to track shim groups. To be reset for each new project.")

;; 
;; ---- dealing with dicoms (using AFNI's dicom_hinfo)
(defvar *dcmtags*
  (dict
   :AcqTime ","
   :SubID ","
   :iPAT ","
   :B0Shim ","
   :Operator ","
   :Project ","
   :SequenceName ","
   :SequenceType ","
   :PED ","
   :TR "0018,0080"
   :TE "0018,0081"
   :Matrix ","
   :PixelResol ","
   :BWP ","
   :BWPPE ","
   :FA ","
   :TA ","
   :FoV ","
   :MB ","
   :FoVShift ","
   :SlcLeakBlk ","
   :SeqMode ","
   :Coil ","
   :ShimMode ",")
  "DICOM name->tag lookup dictionary. Keys match schema.sql.")

(deftype dcmdict () 'hash-table)
(--> get-dcm-values (pathname dcmdict) dcmdict)
(defun get-dcm-values (file tagdict)
  "Read file tags of tagdict(key->tag). Returns dict key->value."
  (let* ((tags_str (str:join " -tag " (hash-table-values tagdict)) )
         (tagvals (cmd:$cmd "dicom_hinfo -no_name -tag " tags_str file))
         (tag_val (dict)))
    ;; tag order and value order matches.
    ;; confirmed in test-dcm-order-invariant
    (mapcar #'(lambda (k v) (setf (access tag_val k) v))
            (hash-table-keys tagdict)
            (str:split " " tagvals))
    tag_val))

(--> dicom-fname-p (pathname) boolean)
(defun dicom-fname-p (fname)
  "Case insensitive matching for known dicom file name patterns."
  (and (ppcre:scan "^MR\\.|(?i)(.dcm|.IMA)$" (file-namestring fname)) t))

;; 
;; ---- database
;; TODO
;; 
;; ---- webserver
;; TODO
;; 
;; ---- filesystem
(defun process-new-dicom (fname)
  (format T "new dicom ~A~%" fname))

(defun on-notify (file event)
  "Act on new files."
  ;; only care when we see something new
  (when (not (equal event :create))
    (return-from on-notify nil))

  ;; recurse into folders or process new dicom
  (if (filesystem:directory-exists-p file)
      (progn
        (format T "watching new directory ~A~%" file)
        (notify:watch file))
      (when(dicom-fname-p file) (process-new-dicom file))))

;; 
;; --- ENTER
;; TODO: sim should be specified by command line arg
(notify:watch "sim/")
(notify:with-events (file change :timeout T)
  (on-notify file change))
