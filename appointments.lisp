;;;;appointments.lisp

(in-package :schedulizer)

;;;;------------------------------------------------------------------------
;;;;Room Class
;;;;------------------------------------------------------------------------

(defvar *rooms* nil)

(defclass meeting-room ()
  ((room-num   :initarg :room-num
	       :accessor room-num)
   (room-name  :initarg :room-name
	       :accessor room-name)
   (capacity   :initarg :capacity
	       :accessor capacity)
   (notes      :initarg :notes
	       :accessor notes)))

(defmethod print-object ((obj meeting-room) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((room-num room-num)
		     (room-name room-name)
		     (capacity capacity)
		     (notes notes))
	obj
      (format stream "~%Room ~a, ~a~%Capacity: ~a~%Notes: ~a~%"
	      room-num
	      room-name
	      capacity
	      notes))))

(defun make-room (room-num room-name capacity notes)
  (make-instance 'meeting-room :room-num  room-num
		               :room-name room-name
			       :capacity  capacity
		               :notes     notes))

(defun add-room (room-num room-name capacity notes)
  (push (make-room room-num room-name capacity notes) *rooms*))

(defun room-search (room-num)
  (loop :for r :in *rooms*
	:if (equal room-num (room-num r))
	  :do (return r)))

(add-room 0 "Virtual" 1000 "Default, Virtual lesson space.")

;;;;------------------------------------------------------------------------
;;;;Appointment list
;;;;------------------------------------------------------------------------

(defvar *appointments* nil) ;;;;should this be in virtual memory or saved to hard drive?
;I guess ideally clients and appointments should both be saved as database files
;that can be accessed and searched- maybe sql or postgre? ----Mito project?

;;;;------------------------------------------------------------------------
;;;;Appointment Class
;;;;------------------------------------------------------------------------

(defclass appointment ()
  ((client       :initarg :client
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
    (with-accessors ((client client)
		     (employee employee)
		     (meeting-room meeting-room)
		     (app-date app-date)
		     (start-time start-time)
		     (end-time end-time)
		     (duration duration)
		     (notes notes))
	obj
      (format stream
	      "~%~a~% ~a---~a~%Room: ~a~%Client: ~a ~a~%Employee: ~a ~a~%Duration: ~a~%Notes: ~a~%"
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
	      
(defun make-appointment (client-id employee-id room-num app-date start-time duration notes)
  (make-instance 'appointment :client (id-search client-id)
		              :employee (employee-search employee-id)
			      :meeting-room (room-search room-num)
		              :app-date app-date
			      :start-time start-time
			      :end-time (add-time start-time duration)
			      :duration duration
			      :notes notes))

(defmethod add-appointment ((appointment appointment))
  (push appointment *appointments*))
;;;;test
(setq test-appointment (make-appointment 1001 2001 0 (date 3 26 2022) (set-time 15 30) 45 "cabbage"))

;;;;------------------------------------------------------------------------
;;;;Recurring Appointments
;;;;------------------------------------------------------------------------

(defmethod recurring ((appointment appointment) number-of-appointments); &optional (recurrence-rate 7))
  (let ((client   (client appointment))
	(employee (employee appointment))
	(app-date    (app-date appointment))
	(start-time  (start-time appointment))
        (duration    (duration appointment))
	(notes       (notes appointment)))
    (loop :with current-week := app-date
	  :for i :from 1 to number-of-appointments
	  :do (add-appointment
	       (make-appointment (client-id client) (employee-id employee) current-week start-time duration notes))
	      (setf current-week (add-days current-week 7)))))

;(recurring (make-appointment 1002 2001 (date 1 5 2022) (set-time 10 30) 30 "") 50)
;(recurring (make-appointment 1003 2001 (date 1 5 2022) (set-time 17 0) 30 "") 50)
;;;;------------------------------------------------------------------------
;;;;Unchecked Appointments
;;;;------------------------------------------------------------------------

;(defvar *appointments* nil)

;(defmethod add-appointment ((appointment appointment))
 ; (push appointment
;	*appointments*))

(defmethod backup-appointment ((appointment appointment))
  (with-open-file (out (asdf:system-relative-pathname "schedulizer"
						      "appointment-backup.lisp")
		       :direction :output
		       :if-exists :append)
    (format out
	    "~%(add-appointment (make-appointment ~a (date ~a ~a ~a) (set-time ~a ~a) ~a))"
	    (write-to-string (client-id (client appointment)))
	    (write-to-string (month (app-date appointment)))
	    (write-to-string (day (app-date appointment)))
	    (write-to-string (year (app-date appointment)))
	    (write-to-string (month (app-date appointment)))
	    (write-to-string (hour (start-time appointment)))
	    (write-to-string (minutes (start-time appointment)))
	    (write-to-string (duration appointment))
	    (write-to-string (notes appointment)))))



(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun appointment-input ()
  (let ((new-appointment (make-appointment (prompt-read "Client ID: ")
					   (date (prompt-read "Month: (1, 2, 3 etc)")
						 (prompt-read "Day: ")
						 (prompt-read "Year: "))
					   (set-time (prompt-read "Hour: ")
						     (prompt-read "Minutes: "))
					   (prompt-read "Duration in minutes: ")
					   (prompt-read "Notes"))))
    (add-appointment new-appointment)
    (backup-appointment new-appointment)))

(defun input-appointments ()
  (loop (appointment-input)
	(if (not (y-or-n-p "Another new appointment? [y/n]: ")) (return))))
;;;;------------------------------------------------------------------------
;;;;Checking out Appointments
;;;;------------------------------------------------------------------------

(defmethod past-appointment-p ((appointment appointment))
  "Checks whether an appointment has passed."
  (let ((today (today))
	(ct    (current-time)))
    (and (equal-date today (later-date (app-date appointment) today))
	 (equal-time ct    (later-time (start-time appointment) ct)))))
   
;(defun ready-appointments (employee-id)
 ;   (loop :for a in *appointments*
  ;        :if (and (equal (employee-id (employee a)) employee-id)
;		   (past-appointment-p a))
;	    :collect a into apts
;	  :finally (return apts)))
