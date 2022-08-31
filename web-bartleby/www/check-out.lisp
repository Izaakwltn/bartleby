;;;; web-bartleby/www/check-out.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :web-bartleby)

(defmacro checkout-handler (form-id form-uri)
  `(hunchentoot:define-easy-handler (,form-id :uri ,form-uri) (appointment-id attendance makeup notes)
     (bartleby::check-out
      (bartleby::appointment-id-search (parse-integer appointment-id))
      attendance
      (parse-integer makeup)
      notes)
     (appointments-check-out)))

(defmethod checkable-appointment ((appointment bartleby::appointment))
  (let ((form-id (format nil "appointment-checkout-~a" (mito:object-id appointment)))
	(form-uri (format nil "/appointment-checkout-~a" (mito:object-id appointment))))
    (let ((handler-name (read-from-string form-id)))
      (progn (checkout-handler handler-name form-uri) ;(progn (hunchentoot:define-easy-handler (form-id :uri "/appointment-check-out") (appointment-id atte;ndance makeup notes)
  ;(bartleby::check-out (bartleby::appointment-id-search (parse-integer appointment-id)) attendance (parse-integer makeup) notes)
  ;(appointments-check-out))
    (spinneret:with-html
      (:form :action form-uri :id form-id
		    ;(:select :form "appointment-check-out" :name "appointment" (:option :value appointment))
           (:tr (:td (:select :form form-id :name "appointment-id"
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
			 (:td (:select :name "attendance" :form form-id
				(loop :for i :in bartleby::attendance-values
			              :do (:option :value i (format nil "~a" i)))))
                         (:td (:select :name "makeups" :form form-id
                                (loop :for i :from -60 :to +60 :by 15
                                      :if (zerop i)
                                        :do (:option :value 0 :selected 0)
                                      :else
                                        :do (:option :value i (format nil "~a" i)))))
			 (:td (:input :type "text" :id "notes" :name "notes" :form form-id))
			 (:td (:button :type "submit" :class "btn btn-default" "Check Out")))))))))

(hunchentoot:define-easy-handler (appointments-check-out :uri "/appointments-check-out") ()
  (with-page (:title "Check out Appointments")
    (:h1 "Check out Appointments")
	   (cl-bootstrap:bs-table
	    (:thead
	     (:th "Client")
	     (:th "Employee")
	     (:th "Date/Time")
	     (:th "Attendance")
             (:th "Makeup+/-")
	     (:th "Notes")
	     (:th "")
             (:tbody
              (spinneret:with-html
                (loop :for a :in (sort (bartleby::all-past-appointments)
                                       #'(lambda (x y)
                                           (bartleby:later-timestamp-p
                                            (bartleby::timestamp-from-sql
                                             (write-to-string
                                              (bartleby::appointment-timestamp y)))
                                            (bartleby::timestamp-from-sql
                                             (write-to-string
                                              (bartleby::appointment-timestamp x))))))
		         :do (checkable-appointment a))))))))

(hunchentoot:define-easy-handler (appointment-check-out :uri "/appointment-check-out") (appointment-id attendance makeup notes)
  (bartleby::check-out (bartleby::appointment-id-search (parse-integer appointment-id)) attendance (parse-integer makeup) notes)
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
