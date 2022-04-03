;;;;receipts.lisp
;;;;
;;;;

(in-package :schedulizer)

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

(defvar attendance-values '((0 "Arrived                ")
			    (1 "No Show                ")
			    (2 "Cancelled, Makeup Added")
			    (3 "Makeup Used            ")
			    (4 "Lesson + Makeup Minutes")))

(defmethod print-object ((obj receipt) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((appointment   appointment)
		     (attendance    attendance)
		     (duration      duration)
		     (makeup-change makeup-change))
	obj
      (format stream"~a~a ~a ~a, ~a ~a minutes, Makeup Change: ~a"
	      (app-date appointment)
	      (start-time appointment)
	      (first-name (client appointment))
	      (last-name (client appointment))
	      (second (assoc attendance attendance-values))
	      duration
	      makeup-change))))

(defun make-receipt (appointment attendance duration makeup-change)
  (make-instance 'receipt :appointment   appointment
		          :attendance    attendance
			  :duration      duration
			  :makeup-change makeup-change))

;;;;------------------------------------------------------------------------
;;;;Checking out appointments, making receipts
;;;;------------------------------------------------------------------------

(defun ready-appointments (employee-id)
  "Returns all past appointments."
    (loop :for a in *appointments*
          :if (and (equal (employee-id (employee a)) employee-id)
		   (past-appointment-p a))
	    :collect a into apts
	  :finally (return apts)))

(defun month-appointments (employee-id month)
  "Returns all apppointments for an employee in a given month."
  (loop :for a in *appointments*
	:if (and (equal (employee-id (employee a)) employee-id)
		 (equal month (month (app-date a))))
	  :collect a into apts
	:finally (return apts)))

(defun check-out-appointment (appointment)
  (format t "~%~a~a~%~a ~a, Scheduled Duration: ~a~%"
	  (app-date appointment)
	  (start-time appointment)
	  (first-name (client appointment))
	  (last-name  (client appointment))
	  (duration appointment))
  (push (make-receipt appointment
		(prompt-read (format nil "Attendance:~%(0) Arrived~%(1) No Show~%(2) Cancelled, Makeup added~%(3) Makeup~%(4) Lesson + Makeup used"))
		(prompt-read "Duration: ")
		(prompt-read "Makeup used or earned"))
	*receipts*))

(defun check-out (employee-id)
  (loop :for a :in (ready-appointments employee-id)
	:do (check-out-appointment a))) 
