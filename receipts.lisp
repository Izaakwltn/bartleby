;;;; receipts.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :bartleby)

;;; Receipts

(mito:deftable receipt (appointment)
  ((attendance    :col-type (:varchar 32))
   (makeup      :col-type (:int))
   (final-notes :col-type (:varchar 128)))
  (:conc-name "receipt-"))

(mito:ensure-table-exists 'receipt)  

(defvar attendance-values '(arrived no-show cancelled))

(deftype attendance-value () (member attendance-values))

(defmethod print-object ((obj receipt) stream)
  (print-unreadable-object (obj stream :type t)
   (with-accessors ((client-id   appointment-client-id)
		    (employee-id appointment-employee-id)
		    (room-id     appointment-room-id)
		    (timestamp   appointment-timestamp)
		    (duration    appointment-duration)
		    (notes       appointment-notes)
                    (attendance  receipt-attendance)
                    (makeup      receipt-makeup)
      	            (final-notes receipt-final-notes))
	obj
     (format stream "Receipt #~a~%~a~%~a~%~a~%~a~%~a~%~a~%Makeup+/-:~a~%Attendance:~a~%~a"
	     (mito:object-id obj)
	     timestamp
	     (room-id-search room-id)
	     (client-id-search client-id)
             (employee-id-search employee-id)
             duration
	     notes
             makeup
	     attendance
	     final-notes))))

(defmethod make-receipt ((appointment appointment) attendance makeup notes)
  (make-instance 'receipt :client-id (appointment-client-id appointment)
			  :employee-id (appointment-employee-id appointment)
			  :room-id (appointment-room-id appointment)
			  :timestamp (appointment-timestamp appointment)
			  :duration (appointment-duration appointment)
                          :makeup makeup
			  :notes       (appointment-notes appointment)
			  :attendance (write-to-string attendance)
		          :final-notes notes))
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


;;; Referencing Receipts

(defun receipt-count ()
  "Returns the total number of Receipts"
  (mito:count-dao 'receipt))

(defun receipt-id-search (receipt-id)
  (mito:find-dao 'receipt :id receipt-id))

(defun all-receipts ()
  (loop :with receipts := nil
        :with rc := (receipt-count)

	:for i :upfrom 1
        :do (setq receipts (if (null (receipt-id-search i))
                               receipts
                               (cons (receipt-id-search i) receipts)))
        :when (equal (length receipts) rc)
          :do (return receipts)))





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
