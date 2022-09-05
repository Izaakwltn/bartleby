;;;; bartleby.asd
;;;;
;;;; Copyright Izaak Walton (c) 2022

(asdf:defsystem #:bartleby
  :version "0.0.1"
  :author "Izaak Walton <izaakw@protonmail.com>"
  :license "GNU General Purpose License"
  :description "Scheduling System"
  :depends-on ("local-time" "mito" "alexa" "cl-pdf")
  :serial t
  :components ((:file "package")
               (:file "sql")
	       (:file "system-generics")
	       ;(:file "sort")
	       ;(:file "search")
	       (:file "time")
	       (:file "dates")
	       (:file "timestamps")
	       ;(:file "calendar") ;;;;maybe later
	       (:file "contact")
	       (:file "clients")
	       (:file "rooms")
	       (:file "employees")
               (:file "services")
	       (:file "appointments")
	       (:file "availability") 
	       (:file "receipts")
	       (:file "check-out")
               (:file "credits")
	       (:file "invoices") ;add check-out.lisp
	       ;(:module "interface"
		;:serial t
		;:components ((:file "lexing")
	         ;            (:file "browse")
		;	     (:file "new")
		;;	     (:file "check-out")
		;	     (:file "help")
		;	     (:file "bartleby")))
	       (:module "test-populations"
		:serial t
		:components ((:file "test-populations")))))
