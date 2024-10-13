;; using CIEL-lang
;; also https://lispcookbook.github.io/cl-cookbook/websockets.html
;; https://ciel-lang.org/#/scripting?id=auto-reload
(defpackage mrqart
  (:use :cl :ciel)
  (:local-nicknames
   (:filesystem :uiop/filesystem)
   (:notify :org.shirakumo.file-notify))
  (:export get-dcm-values *dcmtags*))

;; for sly
;; (ql:quickload :ciel)
(in-package :mrqart)


;;; ---- globals for tracking state
(defvar *db*
  (dbi:connect :sqlite3
               :database-name "mrqart.sqlite3")
  "Database for last seen and expected protocol values.")
(defvar *shimlist*
  (list)
  "list of protocol + B0shim to track shim groups. To be reset for each new project.")


;;; ---- dealing with dicoms (using AFNI's dicom_hinfo)
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

(defun get-dcm-values (file tagdict)
  "Read file tags of tagdict(key->tag). Returns dict key->value."
  (let* ((tags_str (str:join " -tag " (hash-table-values tagdict)) )
         (tagvals (cmd:$cmd "dicom_hinfo -no_name -tag " tags_str file))
         (tag_val (dict)))
    (mapcar #'(lambda (k v) (setf (access tag_val k) v))
            (hash-table-keys tagdict) (str:split " " tagvals) )
    tag_val))

(defun dicom-fname-p (fname)
  "Case insensitive matching for known dicom file name patterns."
  (and (ppcre:scan "^MR\\.|(?i)(.dcm|.IMA)$" (file-namestring fname)) t))


;;; ---- filesystem
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


;; TODO: sim should be specified by command line arg
(notify:watch "sim/")
(notify:with-events (file change :timeout T)
  (on-notify file change))
