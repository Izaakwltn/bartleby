;;;;appointments.lisp

(in-package :bartleby)

;;;;------------------------------------------------------------------------
;;;;Appointment Class
;;;;------------------------------------------------------------------------

(defclass appointment ()
  ((app-number    :initarg :app-number
		  :accessor app-number)
   (clients       :initarg :clients
	          :accessor clients)
   (employees     :initarg :employees
	          :accessor employees)
   (meeting-room  :initarg :meeting-room
	          :accessor meeting-room)
   (date-time     :initarg :dt
		  :accessor dt)
   (duration      :initarg :duration
	          :accessor duration)
   (notes         :initarg :notes
	          :accessor notes)))

;::::::::::::::::::::::::::::::
;printing appointments

(defun printable-people (people-list)
  (loop :for p :in people-list
	:collect (concatenate 'string
			      (first-name p)
			      " "
			      (last-name p)
			      ", ID: "
			      (write-to-string (id p)))))

(defmethod print-object ((obj appointment) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((app-number    app-number)
		     (clients        clients)
		     (employees      employees)
		     (meeting-room  meeting-room)
		     (dt            dt)
		     (duration      duration)
		     (notes notes))
	obj
      (format stream
	      "~%~a~%~a~%~%Room: ~a~%Client(s):~%~{~a~%~}~%Employee(s):~%~{~a~}~%Duration: ~a~%Notes: ~a~%"
	      app-number
	      dt
	      meeting-room
	      (printable-people clients)
	      (printable-people employees)
	      duration
	      notes))))
;;;;loop through clients, employees, etc, print list of (id, firstname, lastname) for each

(defun client-list (client-ids)
  (loop :for i :in client-ids
	:collect (client-id-search i)))

(defun employee-list (employee-ids)
  (loop :for i :in employee-ids
	:collect (employee-id-search i)))

(defun make-appointment (app-number clients employees room-num date-time duration notes)
  (make-instance 'appointment :app-number app-number
		              :clients clients
		              :employees employees
			      :meeting-room (room-search room-num)
		              :dt date-time
			      :duration duration
			      :notes notes))

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
				client-ids employee-ids room-num date-time duration notes)
  "Removes appointment, adds a replacement appointment."
  (remove-appointment appointment)
  (add-appointment (make-appointment (app-number appointment) client-ids employee-ids room-num date-time duration notes)))

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

(defun new-appointment (client-ids employee-ids room-num date-time duration notes)
  (add-appointment (make-appointment (new-app-number) (client-list client-ids) (employee-list employee-ids) room-num date-time duration notes)))

;example (new-appointment (1002 1003) (2002) 2 (moment 4 20 2022 10 30) 60 "violin")

;;;;------------------------------------------------------------------------
;;;;Backing up Appointments
;;;;------------------------------------------------------------------------

;;;;figure out backup-units for 

(defmethod client-ids ((appointment appointment))
  "returns client ids for all clients in an appointment"
  (loop :for c :in (clients appointment)
	:collect (id c)))

(defmethod employee-ids ((appointment appointment))
  "Returns employee ids for all clients in an appointment."
  (loop :for e :in (employees appointment)
	:collect (id e)))

(defmethod backup-unit ((appointment appointment))
  (format nil "(load-saved-item (make-appointment ~a '(~{~a ~}) '(~{~a~}) ~a ~a ~a ~a))"
	  (app-number appointment)
	  (client-ids appointment)
	  (employee-ids appointment)
	  (id (meeting-room appointment))
	  (backup-unit (dt appointment))
	  (duration appointment)
	  (notes appointment)))

(defun refresh-appointment-backup ()
  (make-backup "appointments" *appointments*))

(defmethod load-saved-item ((appointment appointment))
  (push appointment *appointments*))

;;;;------------------------------------------------------------------------
;;;;Recurring Appointments
;;;;------------------------------------------------------------------------

(defmethod next-day ((appointment appointment))
  (date-time (add-days (dt appointment) 1)))

(defmethod next-week ((appointment appointment))
  (date-time (add-days (dt appointment) 7)))

(defmethod next-month ((appointment appointment))
  "I guess it's convoluted, but do either 30 days or the same day.")

(defmethod recurring ((appointment appointment) number-of-appointments); &optional (recurrence-rate 7))
  (let ((client     (id (client appointment)))
	(employee   (id (employee appointment)))
	(room       (id (meeting-room appointment)))
        (date-time  (dt appointment))
        (duration   (duration appointment))
	(notes      (notes appointment)))
    (loop :with current-date := date-time
	  :for i :from 1 to number-of-appointments
	  :do (progn (new-appointment client employee room current-date duration notes)
	             (setf current-date (next-week current-date ))))))

;(recurring (make-appointment 1002 2001 (date 1 5 2022) (set-time 10 30) 30 "") 50)
;(recurring (make-appointment 1003 2001 (date 1 5 2022) (set-time 17 0) 30 "") 50)

;;;;------------------------------------------------------------------------
;;;;
;;;;------------------------------------------------------------------------

;(defun appointment-search (id) 

;;;;------------------------------------------------------------------------
;;;;Checking out Appointments
;;;;------------------------------------------------------------------------

;(defmethod past-p ((appointment appointment))
 ; "Checks whether an appointment has passed."
  ;(let ((today (today))
;	(ct    (current-time)))
 ;   (and (equal-date today (later-date (app-date appointment) today))
;	 (equal-time ct    (later-time (start-time appointment) ct)))))
   
;(defun ready-appointments (employee-id)
 ;   (loop :for a in *appointments*
  ;        :if (and (equal (employee-id (employee a)) employee-id)
;		   (past-p a))
;	    :collect a into apts
;	  :finally (return apts)))
