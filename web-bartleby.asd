;;;; web-bartleby.asd
;;;;
;;;; Copyright Izaak Walton (c) 2022

(asdf:defsystem #:web-bartleby
  :version "0.0.1"
  :author "Izaak Walton <izaakw@protonmail.com"
  :license "GNU General Purpose License"
  :description "Webapp for Bartleby Scheduling System"
  :depends-on ("bartleby" "spinneret" "cl-bootstrap" "hunchentoot")
  :serial t
  :components ((:module "web-bartleby"
                :serial t
                :components ((:file "package")
                             (:file "server")))))
                             ;(:file "page-template")))))
