(defpackage mrqart-dicom
  (:use :cl :ciel)
  (:export *dcmtags* get-dcm-values dicom-fname-p))
(in-package :mrqart-dicom)

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
   :SeriesNumber "0020,0011"     ; REL Series Number
   :SubID "0010,0010"            ; patient name
   ;; 0051' '1011' 'LO' 'ImaPATModeText' -- this is GRAPA acceleration
   ;; private field. unimplemented? get from CSA
   :iPAT "0051,1011"             ; PATModeText
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
(--> get-dcm-tag-values (pathname dcmdict) dcmdict)
(defun get-dcm-tag-values (file tagdict)
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

(defun first-capture (regexp str)
  "Match REGEXP against STR and return first capture group. REGEXP must have a single capture group within like 'x = (\\d+)'."
  (ppcre:register-groups-bind (group) (regexp str) group))

;; *** new-let:cond
;; new-let:cond sees let and makes the locally-scoped variable avialable in the next expression

;; dicom_hdr needs ~-siemens_csa_data~ to get at ~PhaseEncodingDirectionPositive~ and it is on multiple lines. consider gdcmdump instead. It's slower but still < 50ms and much easier to parse
;; #+begin_src bash
;; gdcmdump -dC sim/src/MR.1.3.12.2.1107.5.2.43.167046.2024100414544965356757655 | perl -lne 'BEGIN{%a=(Phase=>"NA", ucPAT=>"NA")} $a{substr($1,0,5)} = $2 if m/(PhaseEncodingDirectionPositive.*Data..|ucPATMode\s+=\s+)(\d+)/; END {print join("\t", @a{qw/Phase ucPAT/})}'
;; #+end_src

(--> read-csa (pathname) dcmdict)
(defun read-csa (file)
  "PATMode, phase encoding, and slicetiming are in hard to get places in the DICOM header: within the CSA.
Also see gdcmdump, spm's https://github.com/neurodebian/spm12/blob/master/spm_dicom_headers.m "
  (let ((csa_output (cmd:$cmd "dicom_hdr -mulfram -sexinfo" file ))
        (d (dict)))
    (loop :for csa_line :in (str:lines csa_output) :do
      (new-let:cond
        ;; sPat.ucPATMode   =      2
        ((let ((ipat (first-capture "sPat.ucPATMode *= *(\\d+)" csa_line))))
         (setf (gethash :iPAT d) ipat))

        ;; TODO: other CSA info
        ((let ((slicetiming (first-capture "xxxx" csa_line))))
         (setf (gethash :SLICE d) slicetiming))

        ;; todo break loop
        ((>= (length (hash-table-keys d)) 2)
         nil)
        ))
    d))


(--> get-dcm-values (pathname dcmdict) dcmdict)
(defun get-dcm-values (file tagdict)
  (merge-tables
   (get-dcm-tag-values file tagdict)
   (read-csa file)))

(--> dicom-fname-p (pathname) boolean)
(defun dicom-fname-p (fname)
  "Case insensitive matching for known dicom file name patterns."
  (and (ppcre:scan "^MR\\.|(?i)(.dcm|.IMA)$" (file-namestring fname)) t))


