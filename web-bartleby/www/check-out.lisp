;;;; web-bartleby/www/check-out.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :web-bartleby)

(defmethod checkable-appointment ((appointment bartleby::appointment))
  (spinneret:with-html (:tr (:td (format nil "~a ~a"
		    (bartleby::client-first-name (mito:find-dao 'client :id (bartleby::appointment-client-id appointment)))
		    (bartleby::client-last-name (mito:find-dao 'client :id (bartleby::appointment-client-id appointment)))))
       (:td (format nil "~a ~a"
		    (bartleby::employee-first-name
		     (mito:find-dao 'employee :id (bartleby::appointment-employee-id appointment)))
		    (bartleby::employee-last-name
		     (mito:find-dao 'employee :id (bartleby::appointment-employee-id appointment)))))
       (:td (format nil "~a" (bartleby::timestamp-from-sql (bartleby::appointment-timestamp appointment))))
       (:td (:select (loop :for i :in bartleby::attendance-values
						:do (:option :value i (format nil "~a" i))))))))

(hunchentoot:define-easy-handler (appointments-check-out :uri "/appointments-check-out") ()
  (with-page (:title "Check out Appointments")
    (:h1 "Check out Appointments")
    (:form :action "/appointments-check-out" :id "appointments-check-out"
	   (cl-bootstrap:bs-table
	    (:thead
	     (:th "Client")
	     (:th "Employee")
	     (:th "Date/Time")
	     (:th "Attendance")
	    (:tbody
	     (spinneret:with-html
		   (loop :for a :in (bartleby::all-past-appointments)
		         :do (checkable-appointment a)))))))))
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
