;;;; web-bartleby/browse-appointments.lisp
;;;;
;;;; Copyright Izaak Walton (C) 2022

(in-package :web-bartleby)

(hunchentoot:define-easy-handler (browse-appointments :uri "/browse-appointments") ()
  (with-page (:title "All Appointments")
    (:h1 "All Appointments")
    (cl-bootstrap:bs-table
      (:thead
       (:tr
	(:th "Client") (:th "Employee") (:th "timestamp") (:th "Edit")))
      (:tbody
       (spinneret:with-html (loop :for a :in (bartleby::all-appointments)
				  :do (:tr (:td (format nil "~a ~a"
							(bartleby::client-first-name
							 (bartleby::client-id-search (bartleby::appointment-client-id a)))
							(bartleby::client-last-name
						         (bartleby::client-id-search (bartleby::appointment-client-id a)))))
					   (:td (format nil "~a ~a" (bartleby::employee-first-name
								     (bartleby::employee-id-search
								      (bartleby::appointment-employee-id a)))
					       (bartleby::employee-last-name (bartleby::employee-id-search
							       (bartleby::appointment-employee-id a)))))
		      (:td (format nil "~a" (write-to-string (bartleby::appointment-timestamp a))))
		      (:td (:a :href (concatenate 'string "/edit-appointment" (write-to-string
									  (mito:object-id a)))
			       "edit")))))))))
