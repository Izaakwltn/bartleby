;;;;check-out.lisp
;;;;

(in-package :bartleby)

;;;;------------------------------------------------------------------------
;;;;Checking out appointments
;;;;------------------------------------------------------------------------
;;;;move to interface

;(defgeneric check-out (object attendance )
 ; (:documentation "Checks out an appointment or an object's appointments"))

(defmethod check-out ((appointment appointment) attendance duration makeup-change notes)
  (new-receipt appointment attendance duration makeup-change notes)
  (remove-appointment appointment))

(defmethod check-out-prompt ((appointment appointment))
  (browse-print appointment)
  (check-out appointment
	     (parse-integer
	      (prompt-read
	       (format nil "Attendance:~%0 Arrived~%1 No Show~%2 Cancelled, Makeup Added~%3 Makeup appointment~%4 Appointment + Makeup")))
	     (prompt-read "Duration in minutes")
	     (prompt-read "+/- makeup minutes")
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

