;;;;appointments.lisp

(in-package :schedulizer)

;;;;------------------------------------------------------------------------
;;;;Appointment list
;;;;------------------------------------------------------------------------

(defvar *appointments* nil) ;;;;should this be in virtual memory or saved to hard drive?
;I guess ideally clients and appointments should both be saved as database files
;that can be accessed and searched- maybe sql or postgre?

;;;;------------------------------------------------------------------------
;;;;Appointment Class
;;;;------------------------------------------------------------------------

(defclass appointment ()
  ((client     :initarg :client
	       :accessor client)
   (month      :initarg :month
	       :accessor month)
   (day        :initarg :day
	       :accessor day)
   (year       :initarg :year
	       :accessor year)
   (start-time :initarg :start-time
	       :accessor start-time)
   (duration   :initarg :duration
	       :accessor duration)
   (notes      :initarg :notes
	       :accessor notes)))

(defmethod print-object ((obj appointment) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((client client)
		     (month month)
		     (day day)
		     (year year)
		     (start-time start-time)
		     (duration duration)
		     (notes notes))
	obj
      (format stream
	      "~%Date/Time: ~a/~a/~a at ~a~%Client: ~a~%Duration: ~a~%Notes: ~a~%"
	      month day year start-time client duration notes))))
	      
(defun make-appointment (client-id month day year start-time duration notes)
  (make-instance 'appointment :client (id-search client-id)
		              :month month
			      :day day
			      :year year
			      :start-time start-time
			      :duration duration
			      :notes notes))

;;;;test
(defvar test-appointment (make-appointment 1001 3 26 2022 3 45 "cabbage"))

;(defun reccurring (client-id time first-date start-time duration notes) ;optional last-day, default 1 year
 ; (loop for 

;;;;calendar system (calendar.lisp)
;;;;;function to calculate day of week given m/d/y
;;;;



				     
