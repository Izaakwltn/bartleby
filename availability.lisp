;;;;availability.lisp
;;;;
;;;;

(in-package :schedulizer)

;;;;------------------------------------------------------------------------
;;;;Appointment functions
;;;;------------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;Make clients, rooms, and employees lists instead of just one per appointment
;;;;;or just accomodate either option

					;this-day ((meeting-room)) .... employee, client
					;this-week ... meeting room, employee, client
					;this month meeting room, employee, client...

(defgeneric appointments (object)
  (:documentation "Returns a list of appointments associated with an object"))

(defmethod appointments ((employee employee))
  "Will return all appointments relating to the employee."
  (let ((id (employee-id employee)))
    (loop :for a :in *appointments*
	  :if (equal (employee-id (employee a)) id)
	    :collect a :into apts
	  :finally (return apts))))

(defmethod appointments ((meeting-room meeting-room))
  "Will return all appointments relating to the room")

(defmethod appointments ((date date))
  "Will return all appointments occuring on the date.")

;;;also for room, set-time, date, client

;(defmethod appointments ((obj employee))
 ; (with-accessors ((employee-id employee-id))
  ;    (loop :for a :in *appointments*
;	:if (equal employee-id (employee-id (employee a)))
;	  :collect a into apts
;	:finally (return apts))))

;;;;------------------------------------------------------------------------
;;;;Unavailability
;;;;------------------------------------------------------------------------
(defgeneric block-off (object app-date start-time duration)
  (:documentation "Creates a blank appointment block for the given object."))

(defmethod block-off ((employee-employee) app-date start-time duration)
  (add-appointment (make-appointment 0 (employee-id employee) 0 app-date start-time duration)))
;;;repeat for room, maybe client

;(defun employee-block-off (employee-id app-date start-time duration)
 ; "Creates and unavailable block for an employee."
  ;(add-appointment (make-appointment 0 employee-id 0 app-date start-time duration "unavailable")))

;(defun room-block-off (room-num app-date start-time duration)
 ; (add-appointment (make-appointment 0 0 room-num app-date start-time duration)))
;;;room-block-off
				
;;;;------------------------------------------------------------------------				
;;;;Availability calculations:
;;;;------------------------------------------------------------------------
(defun overlap-p (appointment1 appointment2)
  "Determines whether two appointments overlap in time."
  (or (time-conflict-p (start-time appointment1)
		       (start-time appointment2)
		       (end-time appointment2))
      (time-conflict-p (start-time appointment2)
		       (start-time appointment1)
		       (end-time appointment1))))



;(defun available-p (appointment)
 ; "Checks availability of employee and room against the list of appointments"
  ;(and (loop :for a :in (
;
;(defgeneric availability (object)
 ; (:documentation "Returns a list of availability for a given object."))
;
;(defmethod availability ((obj employee)
;			 "available"))
