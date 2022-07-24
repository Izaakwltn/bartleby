;;;; web-bartleby/server.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package #:web-bartleby)

(defvar *server*
  (make-instance 'hunchentoot:easy-acceptor
                 :port 4242
                 :document-root (asdf:system-relative-pathname "web-bartleby" "web-bartleby/www/")))
                 
(defun launch ()
  (hunchentoot:start *server*))
