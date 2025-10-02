(asdf:defsystem #:mrqart-servers
  :description "MRQART http and web socket servers"
  :author "Will Foran <foranw@upmc.edu>"
  :license "GPLv3"
  :version "0.20241102.1"
  :depends-on (#:ciel
               #:uiop
               #:websocket-driver
               #:clack
               #:easy-routes)
  :serial t ;; load files in order. important for global var definitions
  :components ((:file "mrqart-servers")))
