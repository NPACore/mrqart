(asdf:defsystem #:mrqart
  :description "MR Quality Assurance in Real Time"
  :author "Will Foran <foranw@upmc.edu>"
  :license "GPLv3"
  :version "0.20241102.1"
  :depends-on (#:ciel
               #:uiop
               #:mrqart-servers
               #:mrqart-db
               #:mrqart-dicom)
  :serial t ;; load files in order. important for global var definitions
  :components ((:file "mrqart")
               ;(:module "tests" :components ((:file "./mrqart-5am.lisp")))
))
