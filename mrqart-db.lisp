(defpackage mrqart-db
  (:use :cl :ciel)
  (:export *db*))
(in-package :mrqart-db)

(defvar *db*
  (dbi:connect :sqlite3
               :database-name "mrqart.sqlite3")
  "Database for last seen and expected protocol values.")


;; ** Database

;; We use a sqlite database to check the current scan acquisition parameters (via dicom header) against a list of known-good/desired templates.

(defmacro db-query (query &rest params)
  "Fetch-all on the sqlx QUERY for given PARAMS."
  `(dbi:fetch-all (dbi:execute (dbi:prepare *db* (sxql:yield ,query)) ,@params)))
(defun insert-new-session (dicom-dict)
 ;; (hash-table-keys *dcmtags*)
 ;; (:FOV :TA :FA :BWPPE :BWP :PIXELRESOL :MATRIX :TE :TR :PED_MAJOR :SEQUENCETYPE
 ;; :SEQUENCENAME :PROJECT :OPERATOR :COMMENTS :IPAT :SUBID :SERIESNUMBER :ACQDATE
 ;; :ACQTIME)
  (let* ((ses-keys '(:OPERATOR :SUBID :seriesnumber :acqdate :acqtime))
         (ses (dict-subset dicom-dict ses-keys))
         (seq-keys (set-difference (hash-table-keys *dcmtags*) ses-keys))
         (seq (apply #'dict-subset dicom-dict ses-keys)))
    ;; TODO: find or insert sequence
    (dbi:execute (dbi:prepare *db* (sxql:insert-into :acq_param seq-keys ses)))
    ;; add session specific data
    ))

(defun find-ideal-session (proj seqname)
  (db-query (sxql:select :* (sxql:from :acq_param)
              (sxql:where (:and (:like :Project proj)
                                (:like :SequenceName seqname))))
            (list proj seqname)))
