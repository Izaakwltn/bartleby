;;;scheduler.asd

(asdf:defsystem #:schedulizer
  :version "0.0.1"
  :author "Izaak Walton <izaakw@protonmail.com>"
  :license "GNU General Purpose License"
  :description "Scheduling System"
  :serial t
  :components ((:file "package")
	       (:file "clients") ;;;change to "clients"
	       (:file "appointments")))
