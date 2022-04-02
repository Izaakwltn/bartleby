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
;;;;Receipts
;;;;------------------------------------------------------------------------

(defvar *receipts* nil)

(defclass receipt ()
  ((appointment   :initarg :appointment
		  :accessor appointment)
   (makeup-change :initarg :makeup-change
		  :accessor makeup-change)
   (attendance    :initarg :attendance
		  :accessor attendance)
   (duration      :initarg :duration
		  :accessor duration)))

(defmethod print-object ((obj receipt) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((appointment   appointment)
		     (attendance    attendance)
		     (duration      duration)
		     (makeup-change makeup-change))
	obj
      (format stream"~a~%~a~%~a~%~a" appointment attendance duration makeup-change))))

(defun make-receipt (appointment attendance duration makeup-change)
  (make-instance 'receipt :appointment   appointment
		          :attendance    attendance
			  :duration      duration
			  :makeup-change makeup-change))

(defun check-out-appointment (appointment)
  (format t "~a" appointment)
  (push (make-receipt appointment
		(prompt-read "Attendance: Arrived(a), No Show(n), Cancelled with makeup Added (c), Makeup(m), Used some makeup (u)")
		(prompt-read "Duration: ")
		(prompt-read "Makeup used or earned"))
	*receipts*))

(defun check-out (employee-id)
  (loop :for a :in (ready-appointments employee-id)
	:do (check-out-appointment a))) 

;;;;------------------------------------------------------------------------
;;;;Makeups
;;;;------------------------------------------------------------------------

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
   (receipts     :initarg :receipts
		 :accessor receipts)))

(defmethod print-object ((obj invoice) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((title title)
		     (employee employee)
		     (receipts receipts))
	obj
      (format stream
	      "~%~a:~%~%~a~%~{~a~%~}~%"
	      title employee receipts))))

(defun draft-invoice (title employee-id appointments)
  (make-instance 'invoice :title title
		          :employee (employee-search employee-id)
			  :receipts receipts))

;(defvar test-invoice (draft-invoice "Test Invoice" 2001 (month-appointments 2001 1)))
;;;;------------------------------------------------------------------------
;;;;Invoice calculations
;;;;------------------------------------------------------------------------
(defun invoice-total (invoice)
  "Returns the total money earned for an invoice."
  (* (/ (loop for r in (receipts invoice)
	      sum (duration a))
	60)
     (hourly-rate invoice)))

(defun ready-appointments (employee-id)
  "Returns all past appointments."
    (loop :for a in *appointments*
          :if (and (equal (employee-id (employee a)) employee-id)
		   (equal-date (later-date (app-date a) (today)) (today)))
	    :collect a into apts
	  :finally (return apts)))

(defun month-appointments (employee-id month)
  "Returns all apppointments for an employee in a given month."
  (loop :for a in *appointments*
	:if (and (equal (employee-id (employee a)) employee-id)
		 (equal month (month (app-date a))))
	  :collect a into apts
	:finally (return apts)))

(defun month-receipts (employee-id month)
  "Returns all apppointments for an employee in a given month."
  (loop :for r in *receipts*
	:if (and (equal (employee-id (employee (appointment r))) employee-id)
		 (equal month (month (app-date (appointment r)))))
	  :collect r into rcpts
	:finally (return rcpts)))

;;;;------------------------------------------------------------------------
;;;;Printing Invoices:
;;;;------------------------------------------------------------------------

(defvar test-invoice (draft-invoice "Test Invoice" 2001 (month-receipts 2001 1)))

(defun print-invoice (filename invoice)
  (with-open-file (out (asdf:system-relative-pathname "schedulizer" filename)
		       :direction         :output
		       :if-does-not-exist :create
		       :if-exists         :overwrite)
    (format out "~%~a~%~%~a ~a~%~a~%~%"
	    (title invoice)
	    (first-name (employee invoice))
	    (last-name (employee invoice))
	    (address (employee invoice)))))

