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
;;
;; "^L" (page break) also separates sectons. C-x [ or ] to jump around
;; ** Lisp packaging
;; This originally started as a script ~(in-package :ciel-user)~. Switching to ~defpackage~ meant losing the original ~:local-nicknames~ imployed by ciel. Those have been copied back in. ( https://github.com/ciel-lang/CIEL/issues/81)

; #-ciel (ql:quickload :ciel)

(defpackage mrqart
  (:use :cl :ciel)
  (:local-nicknames
   (:filesystem :uiop/filesystem)
   (:notify :org.shirakumo.file-notify)
   (:routes :easy-routes))
  (:export get-dcm-values *dcmtags*))

; packages not in ciel
(ql:quickload "websocket-driver")
(ql:quickload "clack")

(in-package :mrqart)


;; ** Globals
;; These ~defvar~ globals track state

;; *** Database
(defvar *db*
  (dbi:connect :sqlite3
               :database-name "mrqart.sqlite3")
  "Database for last seen and expected protocol values.")

;; *** Current sessions shim block
(defvar *shimlist*
  (list)
  "list of protocol + B0shim to track shim groups. To be reset for each new project.")

(defvar *sequences*
  (dict)
  "MR sequences. Key is seqnumber. value is dict with count (n dcm), paramaters, ideal parameters")


;; ** dealing with dicoms
;; using AFNI's dicom_hinfo tool instead of trying to read ourselves
;; Some values require aditional computation (phase encode direction)
;; Some values benifit from simplifying BWPPE "Unimplmented" -> 0
;; And some values are in the extra CSA bit (B0 shim)
;; :B0Shim ","
;; :MB  :FoVShift from comments
;; S.CSAImageHeaderInfo.ImaCoilString
;; slcLeakBlk: strfind(ImageComments,'LB')) 'ON' else 'OFF';
;; Coil: S.CSAImageHeaderInfo.ImaCoilString;
;; provider => C2P (ABCD, SIEMENS, CMRR, ...)
;; also see  dicm_dict.m from matlab package submissions
;; https://viewer.mathworks.com/?viewer=plain_code&url=https%3A%2F%2Fes.mathworks.com%2Fmatlabcentral%2Fmlc-downloads%2Fdownloads%2Fsubmissions%2F53745%2Fversions%2F52%2Fcontents%2Fdicm_dict.m

(defun dict-subset (d &rest keys)
  "Subset a hashmap d to just provided keys.
  (dict-subset *dcmtags* :TR :TE)
 [This must exist somewhere. But I couldn't find]"
  (let ((pairs (loop :for k in keys
                     :collect k :into l
                     :collect (access d k) :into l
                     :finally (return l))))
    (apply #'dict pairs)))

(defvar *dcmtags*
  (dict
   :AcqTime "0008,0032"          ; Acquisition Time like 145446.685000
   :AcqDate "0008,0022"          ; like 20241004
   :SeiresNumber "0020,0011"     ; REL Series Number
   :SubID "0010,0010"            ; patient name
   ;; 0051' '1011' 'LO' 'ImaPATModeText'
   :iPAT "0051,1011"             ; PATModeText (private field)
   :Comments "0020,4000" ;REL Image Comments//Unaliased MB3/PE4/LB SENSE1
   :Operator "0008,1070"
   :Project "0008,1030"         ; ID Study Description//Brain^wpc-8620
                                        ; 0040,0254 PRC PPS Description//Brain^wpc-8620
   :SequenceName "0008,103e"            ; series descripton
   :SequenceType "0018,0024"            ; ACQ Sequence Name
                                        ; 0018,0023        ACQ MR Acquisition Type //2D
   :PED_major "0018,1312" ;   ACQ Phase Encoding Direction, ROW or COL
   :TR "0018,0080"
   :TE "0018,0081"
   :Matrix "0018,1310"                  ; ACQ Acquisition Matrix
   :PixelResol "0028,0030" ;  IMG Pixel Spacing//2.2978723049164\2.2978723049164
   ;; https://neurostars.org/t/how-is-bandwidthperpixelphaseencode-calculated/26526 (0021,1153)
   :BWP "0018,0095"        ; ACQ Pixel Bandwidth (?)
   :BWPPE "0019,1028"      ; in matlab S.BandwidthPerPixelPhaseEncode;
   :FA "0018,1314"         ;
   :TA "0051,100a"
   :FoV "0051,100c" ; eg FoV 1617*1727; but actually cocaluated from matrix and spacing?
   )
  "DICOM name->tag lookup dictionary. Keys match schema.sql.")

(defun dcmval-sequence-name (seqname)
  "Parse acquisitoin sequence name into known sequence types."
  (cond ((and (ppcre:scan "epfid" seqname) (ppcre:scan "(?i)asl" seqname) ) "ASL")
        ((ppcre:scan "epfid" seqname) "bold")
        ((ppcre:scan "ep_b"  seqname) "DWI")
        ((ppcre:scan "epse"  seqname) "SE")
        ((ppcre:scan "tgse" seqname) "TGSE") ; asl
        ((ppcre:scan "\\*fm2d2r" seqname) "GRFM") ;GRE field mapping
        ((ppcre:scan "\\*spc_171ns" seqname) "T2SPACE")
        (t "-")))
;; ***
(--> dcmval-phase-encode (string string) string)
(defun dcmval-phase-encode (major ispos)
  "Direction set by InPlanePhaseEncodingDirection (ROW or COL) and s.CSAImageHeaderInfo.PhaseEncodingDirectionPositive (1 or 0). like `R > L`, `P > A`."
  (let* ((start-stop (cond ((equal major "ROW") '("R" "L"))
                           ((equal major "COL") '("A" "P"))
                           (t '("NONE"))))
         (ped (if (equal ispos "1")
                  start-stop
                  (reverse start-stop))))
    (str:join " > " ped)))


(deftype dcmdict () 'hash-table)
(--> get-dcm-values (pathname dcmdict) dcmdict)
(defun get-dcm-values (file tagdict)
  "Read file tags of tagdict(key->tag). Returns dict key->value."
  (let* ((tags_str (str:join " -tag " (hash-table-values tagdict)) )
         (tagvals (cmd:$cmd "dicom_hinfo -full_entry -sepstr '\\t' -no_name -tag " tags_str file))
         (tag_val (dict)))
    ;; tag order and value order matches.
    ;; confirmed in test-dcm-order-invariant
    (prin1 tagvals)
    (mapcar #'(lambda (k v) (setf (access tag_val k) v))
            (hash-table-keys tagdict)
            (str:split "\\t" tagvals))
    tag_val))

(--> dicom-fname-p (pathname) boolean)
(defun dicom-fname-p (fname)
  "Case insensitive matching for known dicom file name patterns."
  (and (ppcre:scan "^MR\\.|(?i)(.dcm|.IMA)$" (file-namestring fname)) t))


;; ---- database
;; TODO
(defmacro db-query (query &rest params)
  `(dbi:fetch-all (dbi:execute (dbi:prepare *db* (sxql:yield ,query)) ,@params)))
(defun insert-new-session ())
(defun find-ideal-session (proj seqname)
  (db-query (sxql:select :* (sxql:from :acq_param)
                                                         (sxql:where (:and (:like :Project proj)
                                                                           (:like :SequenceName seqname)))) (list proj seqname)))

;; ---- webserver
(defvar *http-port* 8080 "port to server http over")
(defvar *web-server* nil)

;; test route
(routes:defroute route-root "/name" (&get name)
  (format nil "Hello ~a!" (or name "lisper")))

;;
(push (hunchentoot:create-static-file-dispatcher-and-handler
      "/" "static/index.html")
      hunchentoot:*dispatch-table*)
(push (hunchentoot:create-static-file-dispatcher-and-handler
      "/main.css" "static/main.css")
      hunchentoot:*dispatch-table*)


(defun start-webapp ()
  (setf *web-server* (make-instance 'routes:easy-routes-acceptor :port *http-port*))
  (hunchentoot:start *web-server*))

;; ----- web socket server
(defvar *ws-port* 5000 "Port to run websocket server")
(defvar *ws-connections* '() "list of connections")
(defun ws-new (con)
  (print "new connection")
  (format T "~A~%" con)
  (pushnew con *ws-connections*))
(defun ws-rm (con) (serapeum:delq con *ws-connections*))
(defun ws-broad (ws-from msg)
  (declare (ignore ws-from))
  (loop :for con :in *ws-connections* :do (websocket-driver:send con msg )))

(defun ws-server (env)
  (let ((ws (websocket-driver:make-server env)))
    (websocket-driver:on :open ws (lambda () (ws-new ws)))
    (websocket-driver:on :message ws (lambda (msg) (ws-broad ws msg)))
    (websocket-driver:on :close ws
                         (lambda (&key code reason)
                           (declare (ignore code reason))
                           (print "close connection!")
                           (ws-rm ws)))
 (lambda (responder)
      (declare (ignore responder))
      ;; Send the handshake:
      (websocket-driver:start-connection ws))))

(defvar *ws-handler* (clack:clackup #'ws-server :port *ws-port*))


;;; testing
;; (clack:stop *ws-handler*) (setq *ws-connections* '())
(defvar *client* (wsd:make-client "ws://127.0.0.1:5000/"))
(wsd:start-connection *client*)
(wsd:on :message *client* (lambda (message) (format t "~&Got: ~A~%" message)))

;; ---- filesystem
(defun process-new-dicom (fname)
  (log:info fname)
  (let ((vals (get-dcm-values fname *dcmtags*)))
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


;; --- ENTER
;; TODO: sim should be specified by command line arg
(notify:watch "sim/")
(notify:with-events (file change :timeout T)
  (on-notify file change))
;;
(start-webapp)
