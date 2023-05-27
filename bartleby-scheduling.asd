;;;; bartleby-scheduling.asd
;;;;
;;;; Copyright Izaak Walton (C) 2023

(asdf:defsystem #:bartleby-scheduling
  :version "0.0.1"
  :author "Izaak Walton <izaakw@protonmail.com>"
  :license "GNU General Purpose License"
  :description "Scheduling backend for Bartleby System"
  :depends-on (:local-time :mito :alexa :coalton #+ignore :cl-pdf )
  :serial t
  :components ((:file "sql-config")
               (:module "scheduling"
                :serial t
                :components ((:file "package")
                             (:file "time") ;maybe should be on top level
                             ))))
