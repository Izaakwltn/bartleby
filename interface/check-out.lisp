;;;;check-out.lisp
;;;;

(in-package :bartleby)

;;;;------------------------------------------------------------------------
;;;;Checking out appointments
;;;;------------------------------------------------------------------------
;;;;move to interface

;(defgeneric check-out (object attendance )
 ; (:documentation "Checks out an appointment or an object's appointments"))

(defun check-out (&optional appointment-id)
  (if appointment-id
      (check-out-prompt (appointment-id-search appointment-id))
      (check-out-cycle
       (let ((id-number (parse-integer
			 (prompt-read "Enter a client or employee-id to check out"))))
	(if (client-id-search id-number)
	    (client-id-search id-number)
	    (employee-id-search id-number))))))

(defmethod check-it-out ((appointment appointment) attendance duration makeup-change notes)
  (new-receipt appointment attendance duration makeup-change notes)
  (remove-appointment appointment))

(defmethod check-out-prompt ((appointment appointment))
  ;(browse-print appointment)
  (check-it-out appointment
	     (parse-integer
	      (prompt-read
	       (format nil "~a~%Attendance:~%0 Arrived~%1 No Show~%2 Cancelled, Makeup Added~%3 Makeup appointment~%4 Appointment + Makeup" (browse-print appointment))))
	     (parse-integer (prompt-read "Duration in minutes"))
	     (parse-integer (prompt-read "+/- makeup minutes"))
	     (prompt-read "Notes")))

;;;;;also
(defmethod undo-receipt ((receipt receipt))
  (add-appointment (appointment receipt))
  (remove-receipt receipt))

(defgeneric check-out-cycle (object)
  (:documentation "Cycles through all associated appointments, checks them out"))

(defmethod check-out-cycle ((client client))
  (loop :for a :in (ready-appointments client)
	:do (check-out-prompt a)))

(defmethod check-out-cycle ((employee employee))
  (loop :for a :in (ready-appointments employee)
	:do (check-out-prompt a)))

