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
  (let ((id (id employee)))
    (loop :for a :in *appointments*
	  :if (equal (id (employee a)) id)
	    :collect a :into apts
	  :finally (return apts))))

(defmethod appointments ((client client))
  (let ((id (id client)))
    (loop :for a in *appointments*
	  :if (equal (id (client a)) id)
	    :collect a :into apts
	  :finally (return apts))))

(defmethod appointments ((date date))
  (loop :for a :in *appointments*
	:if (equal-date date (app-date a))
	  :collect a :into apts
	:finally (return apts)))

;;;also for room, set-time, date, client

;;;;------------------------------------------------------------------------
;;;;Unavailability
;;;;------------------------------------------------------------------------
(defgeneric block-off (object app-date start-time duration)
  (:documentation "Creates a blank appointment block for the given object."))

;(defmethod block-off ((employee-employee) app-date start-time duration)
 ; (add-appointment (make-appointment 0 (employee-id employee) 0 app-date start-time duration)))
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
;(defun overlap-p (appointment1 appointment2)
 ; "Determines whether two appointments overlap in time."
  ;(or (time-conflict-p (start-time appointment1)
;		       (start-time appointment2)
;		       (end-time appointment2))
 ;     (time-conflict-p (start-time appointment2)
;		       (start-time appointment1)
;		       (end-time appointment1))))



;(defun available-p (appointment)
 ; "Checks availability of employee and room against the list of appointments"
  ;(and (loop :for a :in (
;
;(defgeneric availability (object)
 ; (:documentation "Returns a list of availability for a given object."))
;
;(defmethod availability ((obj employee)
;			 "available"))
