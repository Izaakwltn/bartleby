;;;; bartleby.asd
;;;;
;;;; Copyright Izaak Walton (c) 2022-2023

(asdf:defsystem #:bartleby
  :version "0.0.1"
  :author "Izaak Walton <izaakw@protonmail.com>"
  :license "GNU General Purpose License"
  :description "Scheduling System"
  :depends-on (:local-time :mito :alexa :cl-pdf :coalton)
  :serial t
  :components ((:module "src"
                :serial t
                        :components ((:file "package")
                                     (:file "sql-config")
	                             (:file "system-generics")
	                             (:file "time")
	                             (:file "dates")
	                             (:file "timestamps")
                                     (:file "contact")
	                             (:file "clients")
	                             (:file "rooms")
                                     (:file "employees")
                                     (:file "services")
                                     (:file "appointments")
                                     (:file "makeups")
                                     (:file "availability")
                                     (:file "receipts")
                                     (:file "check-out")
                                     (:file "credits")
                                     (:file "invoices")
                                     (:file "search")
                                     (:module "test-populations"
		                      :serial t
                                      :components ((:file "test-populations")))))))
