;;;; web-bartleby/www/check-out.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :web-bartleby)

(defmethod checkable-appointment ((appointment bartleby::appointment))
  (spinneret:with-html
    (:form :action "/appointment-check-out" :id "appointment-check-out"
		    ;(:select :form "appointment-check-out" :name "appointment" (:option :value appointment))
		    (:tr (:td (:select :form "appointment-check-out" :name "appointment-id"
				(:option :value (mito:object-id appointment) (format nil "~a ~a"
				      (bartleby::client-first-name
				       (mito:find-dao 'bartleby::client
						      :id (bartleby::appointment-client-id appointment)))
				      (bartleby::client-last-name
				       (mito:find-dao 'bartleby::client
						      :id (bartleby::appointment-client-id appointment)))))))
			 (:td (format nil "~a ~a"
		                      (bartleby::employee-first-name
		                       (mito:find-dao 'bartleby::employee
						      :id (bartleby::appointment-employee-id appointment)))
				      (bartleby::employee-last-name
				       (mito:find-dao 'bartleby::employee
						      :id (bartleby::appointment-employee-id appointment)))))
			 (:td (let ((ts (bartleby::timestamp-from-sql
				         (write-to-string (bartleby::appointment-timestamp appointment)))))
				(let ((dos (bartleby::date-o ts))
				      (tos (bartleby::time-o ts)))
				  (format nil "~a ~a" dos tos))))
			 (:td (:select :name "attendance" :form "appointment-check-out"
				(loop :for i :in bartleby::attendance-values
			              :do (:option :value i (format nil "~a" i)))))
			 (:td (:input :type "text" :id "notes" :name "notes"))
			 (:td (:button :type "submit" :class "btn btn-default" "Check Out"))))))

(hunchentoot:define-easy-handler (appointments-check-out :uri "/appointments-check-out") ()
  (with-page (:title "Check out Appointments")
    (:h1 "Check out Appointments")
    (:form :action "/appointment-check-out" :id "appointment-check-out"
	   (cl-bootstrap:bs-table
	    (:thead
	     (:th "Client")
	     (:th "Employee")
	     (:th "Date/Time")
	     (:th "Attendance")
	     (:th "Notes")
	     (:th "")
	    (:tbody
	     (spinneret:with-html
		   (loop :for a :in (bartleby::all-past-appointments)
		         :do (checkable-appointment a)))))))))

(hunchentoot:define-easy-handler (appointment-check-out :uri "/appointment-check-out") (appointment-id attendance notes)
  (bartleby::check-out (bartleby::appointment-id-search appointment-id) attendance notes)
  (appointments-check-out))
     ;(:hr)
;     (spinneret:with-html (:select :name "employee" :form "new-appointment"
;	     (loop :for e :in (bartleby:all-employees)
;		   :do (:option :value (mito:object-id e)
;				(format nil "~a ~a" (bartleby::employee-first-name e)
;						    (bartleby::employee-last-name e))))))
 ;    (:hr)
  ;   (spinneret:with-html (:select :name "room" :form "new-appointment"
;	     (loop :for r :in (bartleby:all-rooms)
;		   :do (:option :value (mito:object-id r)
;				(format nil "~a ~a" (bartleby::room-num r)
;					(bartleby::room-name r))))))
 ;    (:hr)
  ;   (spinneret:with-html
   ;    (:select :name "date" :form "new-appointment"
;	 (loop :for i :from 0 :to 30
;	       :do (:option :value (bartleby:add-days
;				    (bartleby::date-o (bartleby::current-timestamp)) i)
;			    (format nil "~a"
;				    (bartleby:add-days (bartleby::date-o (bartleby::current-timestamp)) i))))))
 ;    (spinneret:with-html
  ;     (:select :name "time" :form "new-appointment"
;	 (loop :for i :in time-options
;	       :do (:option :value i
;			    (format nil "~a" i)))))
 ;    (:hr)
  ;   (:hr)
   ;  (:label :for "notes"
;	     "Notes ")
 ;    (:input :type "text" :id "notes" :name "notes")
;	          (cl-bootstrap:bs-form-checkbox "Check me out")
;	     (:button :type "submit" :class "btn btn-default" "Submit"))))
