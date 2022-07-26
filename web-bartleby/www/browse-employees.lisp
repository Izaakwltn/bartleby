;;;; web-bartleby/browse-employees.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :web-bartleby)


(hunchentoot:define-easy-handler (browse-employees :uri "/browse-employees") ()
  (with-page (:title "All Employees")
    (:h1 "All Employees")
    (cl-bootstrap:bs-table
      (:thead
       (:tr
	(:th "Name") (:th "Phone") (:th "Email") (:th "Edit")))
      (:tbody
       (spinneret:with-html (loop :for e :in (bartleby::all-employees)
	     :do (:tr (:td (format nil "~a ~a" (bartleby::employee-first-name e)
				   (bartleby::employee-last-name e)))
		      (:td (bartleby::employee-phone e))
		      (:td (bartleby::employee-email e))
		      (:td (:a :href (concatenate 'string "/edit-employee" (write-to-string
									  (mito:object-id e)))
			       "edit")))))))))
