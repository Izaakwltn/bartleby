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

;;;;------------------------------------------------------------------------
;;;;Employee class
;;;;------------------------------------------------------------------------

(defclass employee ()
  ((employee-id :initarg :employee-id
		:accessor employee-id)
   (first-name  :initarg :first-name
		:accessor first-name)
   (last-name   :initarg :last-name
		:accessor last-name)
   (hourly-rate :initarg :hourly-rate
		:accessor hourly-rate)))

(defmethod print-object ((obj employee) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((employee-id employee-id)
		     (first-name first-name)
		     (last-name last-name)
		     (hourly-rate hourly-rate))
	obj
      (format stream "~%Employee-ID: ~a~%Name: ~a ~a~%Rate: $~a/hr~%"
	      employee-id first-name last-name hourly-rate))))

(defun make-employee (employee-id first-name last-name hourly-rate)
  (make-instance 'employee :employee-id employee-id
		           :first-name first-name
			   :last-name last-name
			   :hourly-rate hourly-rate))

;;;;------------------------------------------------------------------------
;;;;Invoice class
;;;;------------------------------------------------------------------------

(defclass invoice ()
  ((title        :initarg :title
	         :accessor title)
   (employee     :initarg :employee
		 :accessor employee)
   ;(hourly-rate  :initarg :hourly-rate
;		 :accessor hourly-rate)
   (appointments :initarg :appointments
		 :accessor appointments)))

(defmethod print-object ((obj invoice) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((title title)
		     (employee employee)
		     (appointments appointments))
	obj
      (format stream
	      "~%~a:~%~%Employee: ~a~%~{~a~%~}~%"
	      title employee appointments))))

(defun draft-invoice (title employee appointments)
  (make-instance 'invoice :title title
		          :employee employee
			  :appointments appointments))
;;;;------------------------------------------------------------------------
;;;;Invoice calculations
;;;;------------------------------------------------------------------------
(defun invoice-total (invoice)
  (* (/ (loop for a in (appointments invoice)
	      sum (duration a))
	60)
     (hourly-rate invoice)))


