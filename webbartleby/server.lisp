;;;; web-bartleby/server.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package #:webbartleby)

(defvar *server*
  (make-instance 'hunchentoot:easy-acceptor
                 :port 4242
                 :document-root (asdf:system-relative-pathname "bartleby" "webbartleby/www/")))

;;; Launching in the browser:
;;; adopted from https://github.com/eudoxia0/trivial-open-browser, thank you
(defparameter +format-string+
  #+(or win32 mswindows windows)
  "explorer ~S"
  #+(or macos darwin)
  "open ~S"
  #-(or win32 mswindows macos darwin windows)
  "xdg-open ~S")

(defun open-browser (url)
  (uiop:run-program (format nil +format-string+ url)))

(defun start-server ()
  (hunchentoot:start *server*))

(defun launch ()
  (start-server)
  (open-browser "http://127.0.0.1:4242"))

(defun main ()
  "Executable entrypoint"
  (handler-case
      (progn
        (start-server)
        (bt:join-thread
         (find-if (lambda (th)
                    (search "hunchentoot" (bt:thread-name th)))
                  (bt:all-threads))))
    (sb-sys:interactive-interrupt () (progn
                                       (format *error-output* "I would prefer not to~&")
                                       (uiop:quit)))
    (error (c) (format *error-output* "~&An error occured: ~A~&" c))))
