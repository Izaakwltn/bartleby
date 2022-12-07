;;;; webbartleby/new-appointment.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :webbartleby)

(defvar time-options (loop :with time-incr := (bartleby::set-time 1 00)

                           :for i :from 1 :to 96
                           :collect (list i time-incr)
                           :do (setq time-incr (bartleby::add-time time-incr 15))))

(defvar date-options (loop :with date-incr := (bartleby::date-o (bartleby::current-timestamp))

                           :for i :from 0 :to 30
                           :collect (list i date-incr)
                           :do (setq date-incr (bartleby::add-days date-incr 1))))

(hunchentoot:define-easy-handler (new-appointment-form :uri "/new-appointment") ()
  (with-page (:title "New Appointment")
    (:h1 "New Appointment")
	(:form :action "/submit-new-appointment" :id "new-appointment"
	       (spinneret:with-html
		 (:select :name "client-id" :form "new-appointment"
		   (loop :for c :in (bartleby:all-clients)
		         :do (:option :value (mito:object-id c)
				      (format nil "~a ~a" (bartleby::client-first-name c)
					                  (bartleby::client-last-name c))))))
     (:hr)
     (spinneret:with-html (:select :name "employee-id" :form "new-appointment"
	     (loop :for e :in (bartleby:all-employees)
		   :do (:option :value (mito:object-id e)
				(format nil "~a ~a" (bartleby::employee-first-name e)
						    (bartleby::employee-last-name e))))))
     (:hr)
     (spinneret:with-html (:select :name "room-id" :form "new-appointment"
	     (loop :for r :in (bartleby:all-rooms)
		   :do (:option :value (mito:object-id r)
				(format nil "~a ~a" (bartleby::room-num r)
					(bartleby::room-name r))))))
     (:hr)
     (spinneret:with-html
       (:select :name "date" :form "new-appointment"
	 (loop :for i :in date-options
	       :do (:option :value (first i)
			    (format nil "~a"
				    (bartleby::pretty-print (second i)))))))
     (spinneret:with-html
       (:select :name "time" :form "new-appointment"
	 (loop :for i :in time-options
	       :do (:option :value (first i)
			    (format nil "~a" (bartleby::pretty-print (second i)))))))
     (:hr)
     (spinneret:with-html
       (:select :name "duration" :form "new-appointment"
	 (loop :for i :from 15 :to 60 :by 15
	       :do (:option :value i
			    (format nil "~a" i)))))
     (:hr)
     (:hr)
     (:label :for "notes"
	     "Notes ")
     (:input :type "text" :id "notes" :name "notes")
     (:hr)
                                        ;(cl-bootstrap:bs-form-checkbox "Recurring")
     (:input :type "checkbox" :id "recurring" :name "recurring" :value 1 "Recurring weekly")
	     (:button :type "submit" :class "btn btn-default" "Submit"))))

;(defvar time-options (loop :with time-incr := (bartleby::set-time 1 0)
;
 ;                          :for i :from 1 :to 96
  ;                         :collect (list i time-incr)
   ;                        :do (setq time-incr (bartleby::add-time time-incr 15))))

;(defvar date-options (loop :with date-incr := (bartleby::date-o (bartleby::current-timestamp))
;
 ;                          :for i :from 0 :to 30
  ;                         :collect (list i date-incr)
 ;                          :do (setq date-incr (bartleby::add-days date-incr 1))))
;			   
;
;(setq time-options (loop :for i :from 1 :to 24
;			 :collect (loop :for j :from 0 :to 59
;				     :if (zerop (mod i 15))
;				       :collect (bartleby::set-time i j) :into time-options
;				     :finally (return time-options))))

(hunchentoot:define-easy-handler (submit-new-appointment :uri "/submit-new-appointment")
    (client-id employee-id room-id date time duration notes recurring)
  (let ((a (bartleby::make-appointment
            (parse-integer client-id)
            (parse-integer employee-id)
            (parse-integer room-id)
            (bartleby::timestamp (second (assoc (parse-integer date) date-options))
                                 (second (assoc (parse-integer time) time-options)))
            (parse-integer duration)
            notes)))
    (if recurring
        (bartleby::weekly a)
        (bartleby::add-appointment a))
  (home-page)))
                                        ;(browse-appointments))
