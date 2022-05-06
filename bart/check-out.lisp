;;;;check-out.lisp
;;;;

(in-package :bart)

;;;;------------------------------------------------------------------------
;;;;Checking out appointments
;;;;------------------------------------------------------------------------
;;;;move to interface

;(defgeneric check-out (object attendance )
 ; (:documentation "Checks out an appointment or an object's appointments"))

(defun check-out (&optional appointment-id)
  (if appointment-id
      (check-out-prompt (bartleby::appointment-id-search appointment-id))
      (check-out-cycle
       (let ((id-number (parse-integer
			 (prompt-read "Enter a client or employee-id to check out"))))
	(if (bartleby::client-id-search id-number)
	    (bartleby::client-id-search id-number)
	    (bartleby::employee-id-search id-number))))))

(defmethod check-it-out ((appointment bartleby::appointment) attendance duration makeup-change notes)
  (bartleby::new-receipt appointment attendance duration makeup-change notes)
  (bartleby::remove-appointment appointment))

(defmethod check-out-prompt ((appointment bartleby::appointment))
  ;(browse-print appointment)
  (check-it-out appointment
	     (parse-integer
	      (prompt-read
	       (format nil "~a~%Attendance:~%0 Arrived~%1 No Show~%2 Cancelled, Makeup Added~%3 Makeup appointment~%4 Appointment + Makeup" (browse-print appointment))))
	     (parse-integer (prompt-read "Duration in minutes"))
	     (parse-integer (prompt-read "+/- makeup minutes"))
	     (prompt-read "Notes")))

                                                          ;;;;;maybe move to receipts.lisp
(defmethod undo-receipt ((receipt bartleby::receipt))
  (bartleby::add-appointment (bartleby::appointment receipt))
  (bartleby::remove-receipt receipt))

(defgeneric check-out-cycle (object)
  (:documentation "Cycles through all associated appointments, checks them out"))

(defmethod check-out-cycle ((client bartleby::client))
  (loop :for a :in (bartleby::ready-appointments client)
	:do (check-out-prompt a)))

(defmethod check-out-cycle ((employee bartleby::employee))
  (loop :for a :in (bartleby::ready-appointments employee)
	:do (check-out-prompt a)))

