;;;;invoices.lisp
;;;;

(in-package :schedulizer)

;;;;------------------------------------------------------------------------

;;;two steps: appointments are stored first as unchecked
;;;then have a repl cycle which goes through each unchecked appointment,
;;;prompts for either No Show, Cancelled Makeup Added, or Arrived on each lesson
;;;maybe also prompt for makeups added or used

;;;;two lists: *unchecked-appointments* and *checked-appointments*

;;;;functions for appointments by month, by year, by week, within a range of dates, by student id, by 

(defclass invoice ()
  ((title        :initarg :title
	         :accessor title)
   (employee     :initarg :employee
		 :accessor employee)
   (hourly-rate  :initarg :hourly-rate
		 :accessor hourly-rate)
   (appointments :initarg :appointments
		 :accessor appointments)))

(defmethod print-object ((obj invoice) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((title title)
		     (hourly-rate hourly-rate)
		     (appointments appointments))
	obj
      (format stream
	      "~%~a:~%~%Hourly Rate"

(defun invoice-total (invoice)
  (* (/ (loop for a in (appointments invoice)
	      sum (duration a))
	60)
     (hourly-rate invoice)))
   
