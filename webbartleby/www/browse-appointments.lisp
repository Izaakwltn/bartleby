;;;; web-bartleby/browse-appointments.lisp
;;;;
;;;; Copyright Izaak Walton (C) 2022

(in-package :webbartleby)

(defmethod browsable-appointment ((appointment bartleby::appointment))
  (spinneret:with-html
    (:tr (:td (format nil "~a ~a"
                      (bartleby::client-first-name
		       (bartleby::client-id-search (bartleby::appointment-client-id appointment)))
		      (bartleby::client-last-name
		       (bartleby::client-id-search (bartleby::appointment-client-id appointment)))))
	 (:td (format nil "~a ~a" (bartleby::employee-first-name
				   (bartleby::employee-id-search
				    (bartleby::appointment-employee-id appointment)))
	              (bartleby::employee-last-name (bartleby::employee-id-search
                                                     (bartleby::appointment-employee-id appointment)))))
	 (:td (let ((ts (bartleby::timestamp-from-sql
			 (write-to-string
			  (bartleby::appointment-timestamp appointment)))))
                (format nil "~a ~a"
                        (bartleby::pretty-print (bartleby::date-o ts))
                        (bartleby::pretty-print (bartleby::time-o ts)))))
	 (:td (format nil "~a" (bartleby::appointment-duration appointment)))
	 (:td (:a :href (concatenate 'string "/edit-appointment"
                                     (write-to-string
				      (mito:object-id appointment)))
			       "edit")))))
  

(hunchentoot:define-easy-handler (browse-appointments :uri "/browse-appointments") ()
  (with-page (:title "All Appointments")
    (:h1 "All Appointments")
    (cl-bootstrap:bs-table
      (:thead
       (:tr
	(:th "Client") (:th "Employee") (:th "Time/Date") (:th "Duration") (:th "Edit")))
      (:tbody
       (loop :for a :in (bartleby::chronological-appointments (bartleby::all-future-appointments))
             :do (browsable-appointment a))))))
      ; (spinneret:with-html (loop :for a :in (bartleby::chronological-appointments (bartleby::all-future-appointments))
;						   
;				  :do (:tr (:td (format nil "~a ~a"
;							(bartleby::client-first-name
;							 (bartleby::client-id-search (bartleby::appointment-client-id a)))
;							(bartleby::client-last-name
;						         (bartleby::client-id-search (bartleby::appointment-client-id a)))))
;					   (:td (format nil "~a ~a" (bartleby::employee-first-name
;								     (bartleby::employee-id-search
;								      (bartleby::appointment-employee-id a)))
;					       (bartleby::employee-last-name (bartleby::employee-id-search
;							       (bartleby::appointment-employee-id a)))))
;					   (:td (let ((ts (bartleby::timestamp-from-sql
;							   (write-to-string
;							    (bartleby::appointment-timestamp a)))))
;						  (format nil "~a ~a"
;							  (bartleby::pretty-print (bartleby::date-o ts))
 ;                                                         (bartleby::pretty-print (bartleby::time-o ts)))))
;					   (:td (format nil "~a" (bartleby::appointment-duration a)))
;		      (:td (:a :href (concatenate 'string "/edit-appointment" (write-to-string
;									  (mito:object-id a)))
;			       "edit")))))))))
