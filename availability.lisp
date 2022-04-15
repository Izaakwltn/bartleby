;;;;availability.lisp
;;;;
;;;;

(in-package :bartleby)

;;;;------------------------------------------------------------------------
;;;;Appointment functions
;;;;------------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;Make clients, rooms, and employees lists instead of just one per appointment
;;;;;or just accomodate either option

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

(defmethod appointments ((meeting-room meeting-room))
  (loop :for a :in *appointments*
	:if (equal (id meeting-room) (id meeting-room))
	  :collect a :into apts
	:finally (return apts)))

(defmethod appointments ((date date))
  (loop :for a :in *appointments*
	:if (equal-date date (app-date a))
	  :collect a :into apts
	:finally (return apts)))

(defmethod appointments ((week week))
  (loop :for a :in *appointments*
	:if (member (app-date a) (days week))
	  :collect a :into apts
	:finally (return apts)))

(defmethod appointments ((month month))
  (loop :for a :in *appointments*
	:if (member (app-date a) (days month))
	  :collect a :into apts
	:finally (return apts)))

;;;;------------------------------------------------------------------------
;;;;Unavailability
;;;;------------------------------------------------------------------------

(defgeneric block-off (object app-date start-time duration)
  (:documentation "Creates a blank appointment block for the given object."))

(defmethod block-off ((employee employee) app-date start-time duration)
  (add-appointment
   (make-appointment 0 0 (id employee) 404 app-date start-time duration "Unavailable")))

(defmethod block-off ((meeting-room meeting-room) app-date start-time duration)
  (add-appointment
   (make-appointment 0 0 0 (id meeting-room) app-date start-time duration "Unavailable")))

				
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

(defmethod available-p ((appointment appointment))
  "Determines whether a suggested appointment poses conflicts."
  (let ((e (id (employee appointment)))
	(r (id (meeting-room appointment)))
	(c (id (meeting-room appointment))))
    (loop :for a :in *appointments*
	  :if (and (or (equal e (id (employee a)))
		       (equal r (id (meeting-room a)))
		       (equal c (id (client a))))
		   (overlap-p a appointment))
	    :do (return nil)
	  :else
	    :do (return t))))

(defgeneric availability-cycle (object start-time start-date end-time end-date duration)
  (:documentation "Cycles through the given time-frame, returns all open slots."))

(defmethod availability-cycle ((employee employee) start-time start-date end-time end-date duration)
  (loop :with ct      := start-time
	:with cd      := start-date
	:with opts    := nil

	:if (and (equal-date (later-date cd end-date) cd)
		 (equal-time (later-time ct end-time) ct))
	  :do (return opts)
	:else :if (available-p (make-appointment 0 0 (id employee) 0 cd ct duration ""))
		:do (progn (setq opts
				 (cons (make-appointment
					0 0 (id employee) 0 cd ct duration "")
				       opts))
			   (setq ct (add-time ct duration))
			   (if (and (equal (hour ct) 24)
				    (> (+ (minutes ct) duration) 60))
			       (setq cd (add-days cd 1))))
	:else :do (progn (setq ct (add-time ct duration))
			        (if (and (equal (hour ct) 24)
				         (> (+ (minutes ct) duration) 60))
				    (setq cd (add-days cd 1))))))


(defmethod availability-cycle ((meeting-room meeting-room) start-time start-date end-time end-date duration)
  (loop :with ct      := start-time
	:with cd      := start-date
	:with opts    := nil

	:if (and (equal-date (later-date cd end-date) cd)
		 (equal-time (later-time ct end-time) ct))
	  :do (return opts)
	:else :if (available-p (make-appointment 0 0 0 (id meeting-room) cd ct duration ""))
		:do (progn (setq opts
				 (cons (make-appointment
					0 0 0 (id meeting-room) cd ct duration "")
				       opts))
			   (setq ct (add-time ct duration))
			   (if (and (equal (hour ct) 24)
				    (> (+ (minutes ct) duration) 60))
			       (setq cd (add-days cd 1))))
	:else :do (progn (setq ct (add-time ct duration))
			        (if (and (equal (hour ct) 24)
				         (> (+ (minutes ct) duration) 60))
				    (setq cd (add-days cd 1))))))

;;;;------------------------------------------------------------------------
;;;;Availability class- maybe....
;;;;------------------------------------------------------------------------

;;;;------------------------------------------------------------------------
;;;;Object availability
;;;;------------------------------------------------------------------------

(defgeneric available-slots (object start-date end-date duration)
  (:documentation "Returns all available appointments for a given object."))
;;;;I think I messed up time- new day starts on 23 not 24

(defmethod available-slots ((employee employee) start-date end-date duration)
  (availability-cycle employee (set-time 1 00) start-date (set-time 23 00) end-date duration))

					;(defun available-p (appointment)
 ; "Checks availability of employee and room against the list of appointments"
  ;(and (loop :for a :in (
;
;(defgeneric availability (object)
 ; (:documentation "Returns a list of availability for a given object."))
;
;(defmethod availability ((obj employee)
;			 "available"))

;;;;------------------------------------------------------------------------
;;;;Set Availability functions
;;;;------------------------------------------------------------------------

;;;eventually have a system for setting 
