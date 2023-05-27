;;;; formatting/bartleby-formatting.asd
;;;;
;;;; Copyright (C) 2023 Izaak Walton

(asdf:defsystem #:bartleby-formatting
  :version "0.0.1"
  :author "Izaak Walton <izaakw@protonmail.com>"
  :license "GNU General Purpose License"
  :description "Formatting for Bartleby scheduling objects."
  :depends-on (:bartleby-scheduling)
  :serial t
  :components ((:file "package")))
