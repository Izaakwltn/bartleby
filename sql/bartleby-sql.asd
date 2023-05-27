;;;; sql/bartleby-sql.asd
;;;;
;;;; Copyright (C) 2023 Izaak Walton

(asdf:defsystem #:bartleby-sql
  :version "0.0.1"
  :author "Izaak Walton <izaakw@protonmail.com>"
  :license "GNU General Purpose License"
  :description "SQL handling for Bartleby Scheduling System"
  :depends-on (:mito)
  :serial t
  :components ((:file "sql-config")))
