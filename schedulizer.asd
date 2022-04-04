;;;scheduler.asd

(asdf:defsystem #:schedulizer
  :version "0.0.1"
  :author "Izaak Walton <izaakw@protonmail.com>"
  :license "GNU General Purpose License"
  :description "Scheduling System"
  :depends-on ("local-time")
  :serial t
  :components ((:file "package")
	       (:file "calendar")
	       (:file "clients")
	       (:file "employees")
	       (:file "appointments")
	       (:file "receipts")
	       (:file "invoices"))) ;add makeups.lisp, printing.lisp,
					;maybe separate out date.lisp, time.lisp, calendar.lisp
                                        ;
