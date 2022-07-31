;;;; receipts.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :bartleby)

;;; Receipts

(mito:deftable receipt ()
  ((appointment-id :col-type (:int))
   (attendance    :col-type (:int))
   (notes         :col-type (:int)))
  (:conc-name "receipt-"))

(mito:ensure-table-exists 'receipt)  

(defvar attendance-values '(arrived no-show cancelled-add-makeup makeup-used lesson-extra-makeup-used))

(deftype attendance-value () (member attendance-values))

(defmethod print-object ((obj receipt) stream)
  (print-unreadable-object (obj stream :type t)
   (with-accessors ((receipt-appointment-id   receipt-appointment-id)
		     (receipt-attendance    receipt-attendance)
      	     (receipt-notes         receipt-notes))
	obj
     (format stream "Receipt # ~a~%~a~%~a~%~a"
	      (mito:object-id obj)
	      (mito:find-dao 'appointment :id receipt-appointment)
	      (second (assoc receipt-attendance attendance-values))
	      receipt-notes))))

(defmethod make-receipt ((appointment appointment) attendance notes)
 (make-instance 'receipt :appointment-id (mito:object-id appointment)
		          :attendance     attendance
			  :notes          notes))

;;; Adding and removing receipts

(defmethod add-receipt ((receipt receipt))
 (mito:insert-dao receipt))

(defmethod remove-receipt ((receipt receipt))
  (mito:delete-dao receipt))

(defmethod replace-receipt ((receipt receipt) new-receipt)
  (remove-receipt receipt)
  (add-receipt new-receipt))

;;; Finding appointments ready for check-out
	 
(defgeneric ready-appointments (object)
  (:documentation "Finds all past, unchecked appointments for the given object"))

(defmethod ready-appointments ((employee employee))
  "Returns all past appointments."
    (past-appointments (appointments employee)))

(defmethod ready-appointments ((client client))
  "Returns all past appointments for the client."
  (past-appointments (appointments client)))

(defmethod ready-appointments ((meeting-room meeting-room))
  (past-appointments (appointments meeting-room)))













;;;Appointments by object for a given month
;maybe this should be moved to the appointments section
;(defgeneric month-appointments (object month)
 ; (:documentation "Returns all appointments for an object given a month number"))

;(defmethod month-appointments ((employee employee) month)
 ; (loop :for a :in *appointments*
;	:if (and (find-if #'(lambda (e)
;			      (equal (id e) (id employee)))
;			  (employees a))
;		 (equal (m (date-o (dt a))) month))
;	  :collect a :into apts
;	:finally (return apts)))

;(defmethod month-appointments ((client client) month)
 ; (loop :for a :in *appointments*
;	:if (and (find-if #'(lambda (c)
;			      (equal (id c) (id client)))
;			  (clients a))
;		 (equal (m (date-o (dt a))) month))
;	  :collect a :into apts
;	:finally (return apts)))

;(defmethod month-appointments ((meeting-room meeting-room) month)
 ; (loop :for a :in *appointments*
;	:if (and (equal (id meeting-room) (id (meeting-room a)))
;		 (equal (m (date-o (dt a))) month))
;	  :collect a :into apts
;	:finally (return apts)))
;

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
