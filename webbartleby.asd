;;;; web-bartleby.asd
;;;;
;;;; Copyright Izaak Walton (c) 2022

(asdf:defsystem #:webbartleby
  :version "0.0.1"
  :author "Izaak Walton <izaakw@protonmail.com"
  :license "GNU General Purpose License"
  :description "Webapp for Bartleby Scheduling System"
  :depends-on ("bartleby" "cl-who" "spinneret" "cl-bootstrap" "hunchentoot")
  :serial t
  :build-operation program-op
  :build-pathname "launch-webbartleby"
  :entry-point "webbartleby::main"
  :components ((:module "webbartleby"
                :serial t
                :components ((:file "package")
                             (:file "server")
                             (:file "page-template")
			     (:module "www"
			      :serial t
			      :components ((:file "home-page")
                                           (:file "new-client")
					   (:file "new-appointment")
					   (:file "new-employee")
					   (:file "new-room")
                                           (:file "view-client")
                                           (:file "view-employee")
                                           (:file "view-room")
					   (:file "browse-clients")
					   (:file "browse-employees")
					   (:file "browse-rooms")
					   (:file "browse-appointments")
                                           (:file "daily-calendar")
					   (:file "weekly-calendar")
                                           (:file "monthly-calendar")
					   (:file "check-out")
                                           (:file "search")))))))
