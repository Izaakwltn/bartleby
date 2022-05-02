;;;bartleby.asd

(asdf:defsystem #:bartleby
  :version "0.0.1"
  :author "Izaak Walton <izaakw@protonmail.com>"
  :license "GNU General Purpose License"
  :description "Scheduling System"
  :depends-on ("local-time")
  :serial t
  :components ((:file "package")
	       (:file "system-generics")
	       (:file "backup")
	       (:file "time")
	       (:file "dates")
	       ;(:file "calendar")
	       (:file "contact")
	       (:file "clients")
	       (:file "rooms")
	       (:file "employees")
	       (:file "appointments")
	       (:file "availability") 
	       (:file "receipts")
	       (:file "invoices")
	       (:file "backup-of-clients")
	       (:file "backup-of-employees")
	       (:file "backup-of-rooms")
	       (:file "backup-of-appointments")
	       (:file "backup-of-receipts")
	       (:file "backup-tweaks")
	       (:module "interface"
		:serial t
		:components ((:file "lexing")
	                     (:file "browse")
			     (:file "new")
			     (:file "check-out")
			     (:file "help")
			     (:file "bartleby")))
	       (:module "tests"
		:serial t
		:components ((:file "client-tests")
			     (:file "employee-tests")
			     (:file "appointment-tests")))))
