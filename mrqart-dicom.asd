(asdf:defsystem #:mrqart-dicom
  :description "MRQART dicom header info and handling"
  :author "Will Foran <foranw@upmc.edu>"
  :license "GPLv3"
  :version "0.20241102.1"
  :depends-on (#:ciel
               #:uiop)
  :serial t ;; load files in order. important for global var definitions
  :components ((:file "mrqart-dicom")))
