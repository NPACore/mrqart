(defpackage mrqart-servers
  (:use :cl :ciel)
  (:local-nicknames
   (:routes :easy-routes))
  (:export ws-broad *web-server* *http-port* *ws-connections* *ws-port* *ws-client* ))
(in-package :mrqart-servers)

;; ** HTTP Web Server
;; The http server hosts a javascript websockets listener. This could be a local file but
;;   - it's easier to point the monitoring computer to a website than copy and keep html synced
;;   - we might want to get feedback from the user via GET or POST for approving a change
;;     * though perhaps this could also be done by reusing the socket

(defvar *http-port* 8080 "port to server http over")
(defvar *web-server* nil "hunchentoot server handler")

(defun start-webapp ()
  "defines routes and populates *web-server*"
  ;; test route. TODO: use to fetch ideal sequence? list all of project?
  (routes:defroute route-root "/name" (&get name)
    (format nil "Hello ~a!" (or name "lisper")))

  ;; Sever is just two static assets. index (with included js) and css
  (push (hunchentoot:create-static-file-dispatcher-and-handler
        "/" "static/index.html")
        hunchentoot:*dispatch-table*)
  (push (hunchentoot:create-static-file-dispatcher-and-handler
        "/main.css" "static/main.css")
        hunchentoot:*dispatch-table*)

  (setf *web-server* (make-instance 'routes:easy-routes-acceptor :port *http-port*))
  (hunchentoot:start *web-server*))


;; ** web socket server
;; The web socket server uses ~clack~. This might be an unnecessary dependency. Does ~hunchentoot~ support ~websocket-driver~?

(defvar *ws-port* 5000 "Port to run websocket server")
(defvar *ws-connections* '() "List of websocket connections to send input file updates.")
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



;; For testing, the server can be stopped and connections cleared like

;; #+begin_src
;; (clack:stop *ws-handler*) (setq *ws-connections* '())
;; #+end_src

;; *** Internal websocket client
(defvar *ws-client*  nil
  "websocket client for debugging/monitoring running servier")

;; TODO: port should be *ws-port*
(defun ws-make-local-client ()
  "Create a local client. Ideally into *ws-client-*"
  (wsd:make-client "ws://127.0.0.1:5000/")
  (wsd:start-connection *ws-client*)
  (wsd:on :message *ws-client* (lambda (message) (format t "~&Got: ~A~%" message))))

