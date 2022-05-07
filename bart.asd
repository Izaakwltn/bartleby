;;;;bart.asd

(asdf:defsystem #:bart ;;;scheduling interface
  :version "0.0.1"
  :author "Izaak Walton <izaakw@protonmail.com>"
  :license "GNU General Purpose License"
  :description "Bart Interface for Bartleby Scheduling System"
  :depends-on ("bartleby")
  :serial t
  :components ((:module "bart"
	       :serial t
	       :components ((:file "package")
			    (:file "lexing")
	                    (:file "browse")
			    (:file "view")
	                    (:file "new")
	                    (:file "check-out")
	                    (:file "help")
	                    (:file "bart")))))

;;;;next: view, edit, invoice, search
