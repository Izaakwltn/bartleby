;;;;appointments.lisp

(in-package :schedulizer)

;;;;------------------------------------------------------------------------
;;;;Appointment Class
;;;;------------------------------------------------------------------------

(defclass appointment ()
  ((app-number   :initarg :app-number
		 :accessor app-number)
   (client       :initarg :client
	         :accessor client)
   (employee     :initarg :employee
	         :accessor employee)
   (meeting-room :initarg :meeting-room
	         :accessor meeting-room)
   (app-date     :initarg :app-date
	         :accessor app-date)
   (start-time   :initarg :start-time
	         :accessor start-time)
   (end-time     :initarg :end-time
	         :accessor end-time)
   (duration     :initarg :duration
	         :accessor duration)
   (notes        :initarg :notes
	         :accessor notes)))

(defmethod print-object ((obj appointment) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((app-number    app-number)
		     (client        client)
		     (employee      employee)
		     (meeting-room  meeting-room)
		     (app-date      app-date)
		     (start-time    start-time)
		     (end-time      end-time)
		     (duration      duration)
		     (notes notes))
	obj
      (format stream
	      "~%~a~%~a~% ~a---~a~%Room: ~a~%Client: ~a ~a~%Employee: ~a ~a~%Duration: ~a~%Notes: ~a~%"
	      app-number
	      app-date
	      start-time
	      end-time
	      meeting-room
	      (first-name client)
	      (last-name client)
	      (first-name employee)
	      (last-name employee)
	      duration
	      notes))))
	      
(defun make-appointment (app-number client-id employee-id room-num app-date start-time duration notes)
  (make-instance 'appointment :app-number app-number
		              :client (id-search client-id)
		              :employee (employee-search employee-id)
			      :meeting-room (room-search room-num)
		              :app-date app-date
			      :start-time start-time
			      :end-time (add-time start-time duration)
			      :duration duration
			      :notes notes))
					   
(defun equal-appointments-p (app1 app2)
  "Compares two appointments, determines if they are equal."
  (and (equal (app-number app1) (app-number app2))
       (equal (id (client app1)) (id (client app2)))
       (equal (id (employee app1)) (id (employee app2)))
       (equal (id (meeting-room app1)) (id (meeting-room app2)))
       (equal (app-date app1) (app-date app2))
       (equal (start-time app1) (start-time app2))
       (equal (end-time app1) (end-time app2))
       (equal (duration app1) (duration app2))
       (equal (notes app1) (notes app2))))

;;;;------------------------------------------------------------------------
;;;;Adding to, removing from, and editing *appointments*
;;;;------------------------------------------------------------------------

(defvar *appointments* nil)

(defmethod add-appointment ((appointment appointment))
  "Adds an appointment to the *appointments* list."
  (push appointment *appointments*)
  (refresh-appointment-backup))

(defmethod remove-appointment ((appointment appointment))
  "Removes an appointment from the *appointments* list"
  (setq *appointments*
	(remove-if #'(lambda (a)
		       (equal-appointments-p a appointment))
		   *appointments*))
  (refresh-appointment-backup))

(defmethod replace-appointment ((appointment appointment)
				client-id employee-id room-num app-date start-time duration notes)
  "Removes appointment, adds a replacement appointment."
  (remove-appointment appointment)
  (add-appointment (make-appointment (app-number appointment) client-id employee-id room-num app-date start-time duration notes)))

;;;;------------------------------------------------------------------------
;;;;Changing one attribute at a time: 
;;;;------------------------------------------------------------------------

(defmethod change-id ((appointment appointment) app-number)
  "Will figure it out later")
  
(defmethod change-date ((appointment appointment) new-date)
  (replace-appointment appointment
		       (app-number appointment)
		       (employee-id (employee appointment))
		       (room-num (meeting-room appointment))
		       new-date
		       (start-time appointment)
		       (duration appointment)
		       (notes appointment)))

(defmethod change-employee ((appointment appointment) new-employee)
  (replace-appointment appointment
		       (app-number appointment)
		       (employee-id new-employee)
		       (room-num (meeting-room appointment))
		       (app-date appointment)
		       (start-time appointment)
		       (duration appointment)
		       (notes appointment)))
;;change client, change employee, room time, duration, notes

;;;;------------------------------------------------------------------------
;;;;Adding new appointments
;;;;------------------------------------------------------------------------

(defvar last-app-number (if (first *appointments*)
				(app-number (first *appointments*))
				10001))

(defun new-app-number ()
  "Generates a new appointment number."
  (setq last-app-number (+ last-app-number 1))
  last-app-number)

(defun new-appointment (client-id employee-id room-num app-date start-time duration notes)
  (add-appointment (make-instance 'appointment :app-number (new-app-number)
		              :client (id-search client-id)
		              :employee (employee-search employee-id)
			      :meeting-room (room-search room-num)
		              :app-date app-date
			      :start-time start-time
			      :end-time (add-time start-time duration)
			      :duration duration
			      :notes notes)))
					   
;;;;------------------------------------------------------------------------
;;;;Backing up Appointments
;;;;------------------------------------------------------------------------

(defmethod backup-unit ((appointment appointment))
  (format nil "(make-appointment ~a ~a ~a ~a ~a ~a ~a ~a)"
	  (app-number appointment)
	  (id (client appointment))
	  (id (employee appointment))
	  (id (meeting-room appointment))
	  (backup-unit (app-date appointment))
	  (backup-unit (start-time appointment))
	  (duration appointment)
	  (notes appointment)))

(defun refresh-appointment-backup ()
  (make-backup "appointment-backup.lisp" *appointments*))

;;;;------------------------------------------------------------------------
;;;;Recurring Appointments
;;;;------------------------------------------------------------------------

(defmethod recurring ((appointment appointment) number-of-appointments); &optional (recurrence-rate 7))
  (let ((client     (client appointment))
	(employee   (employee appointment))
	(room       (meeting-room appointment))
	(app-date   (app-date appointment))
	(start-time (start-time appointment))
        (duration   (duration appointment))
	(notes      (notes appointment)))
    (loop :with current-week := app-date
	  :for i :from 1 to number-of-appointments
	  :do (add-appointment
	       (make-appointment (new-app-number) (id client) (id employee) (id room) current-week start-time duration notes))
	      (setf current-week (add-days current-week 7)))))

;(recurring (make-appointment 1002 2001 (date 1 5 2022) (set-time 10 30) 30 "") 50)
;(recurring (make-appointment 1003 2001 (date 1 5 2022) (set-time 17 0) 30 "") 50)

;;;;------------------------------------------------------------------------
;;;;
;;;;------------------------------------------------------------------------

;(defun appointment-search (id) 

;;;;------------------------------------------------------------------------
;;;;Checking out Appointments
;;;;------------------------------------------------------------------------

(defmethod past-p ((appointment appointment))
  "Checks whether an appointment has passed."
  (let ((today (today))
	(ct    (current-time)))
    (and (equal-date today (later-date (app-date appointment) today))
	 (equal-time ct    (later-time (start-time appointment) ct)))))
   
;(defun ready-appointments (employee-id)
 ;   (loop :for a in *appointments*
  ;        :if (and (equal (employee-id (employee a)) employee-id)
;		   (past-p a))
;	    :collect a into apts
;	  :finally (return apts)))
