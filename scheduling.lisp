;;;;scheduling.lisp
;;;;
;;;;

(in-package :schedulizer)

;;;;------------------------------------------------------------------------
;;;;Availability
;;;;------------------------------------------------------------------------

(defgeneric appointments (object)
  (:documentation "Returns a list of appointments associated with an object"))

;(defmethod appointments ((obj employee))
 ; (with-accessors ((employee-id employee-id))
  ;    (loop :for a :in *appointments*
;	:if (equal employee-id (employee-id (employee a)))
;	  :collect a into apts
;	:finally (return apts))))

(defun employee-block-off (employee-id app-date start-time duration)
  (add-appointment (make-appointment 0 employee-id app-date start-time duration "unavailable")))

;;;room-block-off

(defun overlap-p (appointment1 appointment2)
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
