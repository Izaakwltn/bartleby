;;;; web-bartleby/new-appointment.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :web-bartleby)

(hunchentoot:define-easy-handler (new-appointment-form :uri "/new-appointment") ()
  (with-page (:title "New Appointment")
    (:h1 "New Appointment")
	(:form :action "/submit-new-appointment" :id "new-appointment"
	       (spinneret:with-html
		 (:select :name "client" :form "new-appointment"
		   (loop :for c :in (bartleby:all-clients)
		         :do (:option :value (mito:object-id c)
				      (format nil "~a ~a" (bartleby::client-first-name c)
					                  (bartleby::client-last-name c))))))
     (:hr)
     (spinneret:with-html (:select :name "employee" :form "new-appointment"
	     (loop :for e :in (bartleby:all-employees)
		   :do (:option :value (mito:object-id e)
				(format nil "~a ~a" (bartleby::employee-first-name e)
						    (bartleby::employee-last-name e))))))
     (:hr)
     (spinneret:with-html (:select :name "room" :form "new-appointment"
	     (loop :for r :in (bartleby:all-rooms)
		   :do (:option :value (mito:object-id r)
				(format nil "~a ~a" (bartleby::room-num r)
					(bartleby::room-name r))))))
     (:hr)
     (spinneret:with-html
       (:select :name "date" :form "new-appointment"
	 (loop :for i :from 0 :to 30
	       :do (:option :value (bartleby:add-days
				    (bartleby::date-o (bartleby::current-timestamp)) i)
			    (format nil "~a"
				    (bartleby:add-days (bartleby::date-o (bartleby::current-timestamp)) i))))))
     (spinneret:with-html
       (:select :name "time" :form "new-appointment"
	 (loop :for i :in time-options
	       :do (:option :value i
			    (format nil "~a" i)))))
     (:hr)
     (:hr)
     (:label :for "notes"
	     "Notes ")
     (:input :type "text" :id "notes" :name "notes")
	          (cl-bootstrap:bs-form-checkbox "Recurring")
	     (:button :type "submit" :class "btn btn-default" "Submit"))))


(setq time-options (loop :for i :from 1 :to 24
			 :collect (loop :for j :from 0 :to 59
				     :if (zerop (mod i 15))
				       :collect (bartleby::set-time i j) :into time-options
				     :finally (return time-options))))
