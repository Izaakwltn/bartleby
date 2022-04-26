;;;;receipts.lisp
;;;;
;;;;

(in-package :bartleby)

;;;;------------------------------------------------------------------------
;;;;Receipts
;;;;------------------------------------------------------------------------

(defvar *receipts* nil)

(defclass receipt ()
  ((id            :initarg :id
		  :accessor id)
   (appointment   :initarg :appointment
		  :accessor appointment)
   (makeup-change :initarg :makeup-change
		  :accessor makeup-change)
   (attendance    :initarg :attendance
		  :accessor attendance)
   (duration      :initarg :duration
		  :accessor duration)
   (notes         :initarg :notes
		  :accessor notes)))
				      ;in interface, can accept nil value

(defvar attendance-values '((0 "Arrived                ")
			    (1 "No Show                ")
			    (2 "Cancelled, Makeup Added")
			    (3 "Makeup Used            ")
			    (4 "Lesson + Makeup Minutes")))

(defmethod print-object ((obj receipt) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((id            id)
	             (appointment   appointment)
		     (attendance    attendance)
		     (duration      duration)
		     (makeup-change makeup-change)
		     (notes         notes))
	obj
      (format stream"Receipt #~a, ~a ~a ~a, ~a ~a minutes, Makeup Change: ~a Notes: ~a"
	      id
	      (dt appointment)
	      (clients appointment)
	      (employees appointment)
	      (second (assoc attendance attendance-values))
	      duration
	      makeup-change
	      notes))))

(defmethod make-receipt ((appointment appointment) id attendance duration makeup-change notes)
  (make-instance 'receipt :id            id
			  :appointment   appointment
		          :attendance    attendance
			  :duration      duration
			  :makeup-change makeup-change
			  :notes         notes))

;;;;------------------------------------------------------------------------
;;;;Adding and removing receipts
;;;;------------------------------------------------------------------------
(defvar *receipts* nil)

(defmethod add-receipt ((receipt receipt))
  (push receipt *receipts*)
  (refresh-receipt-backup))

(defmethod remove-receipt ((receipt receipt))
  (setq *receipts*
	(remove-if #'(lambda (r)
		       (equal (id r) (id receipt))))))
;;;;------------------------------------------------------------------------
;;;;Backing up receipts                                                          ;;;;;like clients, employees etc
;;;;------------------------------------------------------------------------
(defun refresh-receipt-backup ()
  "Refreshes the receipt backup")

;;;;------------------------------------------------------------------------
;;;;New Receipts
;;;;------------------------------------------------------------------------
(defvar last-receipt-id (if (first *receipts*)
			    (id (first *appointments*))
			    20000))

(defun new-receipt-id ()
  (setq last-receipt-id (+ last-receipt-id 1))
  last-receipt-id)

(defmethod new-receipt ((appointment appointment) attendance duration makeup-change notes)
  (add-receipt (make-receipt (new-receipt-id) appointment attendance duration makeup-change notes)))

;;;;------------------------------------------------------------------------
;;;;Finding appointments ready for check-out
;;;;------------------------------------------------------------------------

(defgeneric ready-appointments (object)
  (:documentation "Finds all past, unchecked appointments for the given object"))

(defmethod ready-appointments ((employee employee))
  "Returns all past appointments."
    (loop :for a in *appointments*
          :if (and (find-if #'(lambda (e)
				(equal (id e) (id employee)))
			    (employees a))
		   (past-appointment-p a))
	    :collect a into apts
	  :finally (return apts)))

(defmethod ready-appointments ((client client))
  "Returns all past appointments for the client."
  (loop :for a in *appointments*
	:if (and (find-if #'(lambda (c)
			      (equal (id c) (id client)))
			  (clients a))
		 (past-appointment-p a))
	  :collect a into apts
	:finally (return apts)))

(defmethod ready-appointments ((meeting-room meeting-room))
  (loop :for a in *appointments*
	:if (and (equal (id meeting-room) (id (meeting-room a)))
		 (past-appointment-p a))
	  :collect a into apts
	:finally (return apts)))

;;;;------------------------------------------------------------------------
;;;;Appointments by object for a given month
;;;;------------------------------------------------------------------------
                                                                    ;maybe this should be moved to the appointments section
(defgeneric month-appointments (object month)
  (:documentation "Returns all appointments for an object given a month number"))

(defmethod month-appointments ((employee employee) month)
  (loop :for a :in *appointments*
	:if (and (find-if #'(lambda (e)
			      (equal (id e) (id employee)))
			  (employees a))
		 (equal (m (date-o (dt a))) month))
	  :collect a :into apts
	:finally (return apts)))

(defmethod month-appointments ((client client) month)
  (loop :for a :in *appointments*
	:if (and (find-if #'(lambda (c)
			      (equal (id c) (id client)))
			  (clients a))
		 (equal (m (date-o (dt a))) month))
	  :collect a :into apts
	:finally (return apts)))

(defmethod month-appointments ((meeting-room meeting-room) month)
  (loop :for a :in *appointments*
	:if (and (equal (id meeting-room) (id (meeting-room a)))
		 (equal (m (date-o (dt a))) month))
	  :collect a :into apts
	:finally (return apts)))

;;;;------------------------------------------------------------------------
;;;;Checking out appointments
;;;;------------------------------------------------------------------------
;;;;move to interface
;(defgeneric check-out (object)
 ; (:documentation "Checks out an appointment or an object's appointments"))

;(defmethod check-out ((appointment appointment) attendance duration makeup-change notes)
 ; (make-receipt appointment attendance duration makeup-change notes)
  ;(remove-appointment appointment))

;(defmethod check-out ((client client))
 ; (loop :for a :in (ready-appointments client)
;	:finally (return client)))
;(defun check-out-appointment (appointment)
 ; (format t "~%~a~a~%~a ~a, Scheduled Duration: ~a~%"
;	  (app-date appointment)
;	  (start-time appointment)
;	  (first-name (client appointment))
;	  (last-name  (client appointment))
;	  (duration appointment))
 ; (push (make-receipt appointment
;		(prompt-read (format nil "Attendance:~%(0) Arrived~%(1) No Show~%(2) Cancelled, Makeup added~%(3) Makeup~%(4) Lesson + Make;up used"))
;		(prompt-read "Duration: ")
;		(prompt-read "Makeup used or earned"))
;	*receipts*))

;(defun check-out (employee-id)
 ; (loop :for a :in (ready-appointments employee-id)
;	:do (check-out-appointment a))) 
