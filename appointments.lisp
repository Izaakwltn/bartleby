;;;;appointments.lisp

(in-package :bartleby)

;;;;------------------------------------------------------------------------
;;;;Appointment Class
;;;;------------------------------------------------------------------------

(defclass appointment ()
  ((id            :initarg :id
		  :accessor id)
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
    (with-accessors ((id            id)
		     (clients       clients)
		     (employees     employees)
		     (meeting-room  meeting-room)
		     (dt            dt)
		     (duration      duration)
		     (notes         notes))
	obj
      (format stream
	      "~%~a~%~a~%~%Room: ~a~%Client(s):~%~{~a~%~}~%Employee(s):~%~{~a~}~%Duration: ~a~%Notes: ~a~%"
	      id
	      dt
	      meeting-room
	      (printable-people clients)
	      (printable-people employees)
	      duration
	      notes))))

(defun client-list (client-ids)
  (loop :for i :in client-ids
	:collect (client-id-search i)))

(defun employee-list (employee-ids)
  (loop :for i :in employee-ids
	:collect (employee-id-search i)))

(defun make-appointment (id client-ids employee-ids room-num date-time duration notes)
  (make-instance 'appointment :id           id
		              :clients      (client-list client-ids)
		              :employees    (employee-list employee-ids)
			      :meeting-room (room-search room-num)
		              :dt           date-time
			      :duration     duration
			      :notes        notes))

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
		       (equal (id a) (id appointment)))
		   *appointments*))
  (refresh-appointment-backup))

(defmethod replace-appointment ((appointment appointment)
				client-ids employee-ids meeting-room date-time duration notes)
  "Removes appointment, adds a replacement appointment."
  (remove-appointment appointment)
  (add-appointment (make-appointment (id appointment) client-ids employee-ids meeting-room date-time duration notes)))

(defmethod credit-appointment ((appointment appointment))
  (make-credit (date-o (dt appointment))
	       (client appointment)
	       appointment
	       (duration appointment)
	       nil))

(defun appointment-id-search (app-id)
  (find-if #'(lambda (a)
	       (equal (id a) app-id))
	   *appointments*))

;;;;------------------------------------------------------------------------
;;;;Changing one attribute at a time: 
;;;;------------------------------------------------------------------------

(defmethod client-ids ((appointment appointment))
  "returns client ids for all clients in an appointment"
  (loop :for c :in (clients appointment)
	:collect (id c)))

(defmethod employee-ids ((appointment appointment))
  "Returns employee ids for all clients in an appointment."
  (loop :for e :in (employees appointment)
	:collect (id e)))

(defmethod change-id ((appointment appointment) new-id)
  "Will figure it out later, maybe unecessary but just ticking every box.")

(defmethod change-clients ((appointment appointment) new-client-ids)
  (replace-appointment appointment
		       new-client-ids
		       (employee-ids appointment)
		       (meeting-room appointment)
		       (dt appointment)
		       (duration appointment)
		       (notes appointment)))

(defmethod change-employees ((appointment appointment) new-employee-ids)
  (replace-appointment appointment
		       (client-ids appointment)
		       new-employee-ids
		       (meeting-room appointment)
		       (dt appointment)
		       (duration appointment)
		       (notes appointment)))

(defmethod change-room ((appointment appointment) new-room)
  (replace-appointment appointment
		       (client-ids appointment)
		       (employee-ids appointment)
		       new-room
		       (dt        appointment)
		       (duration  appointment)
		       (notes     appointment)))

(defmethod change-date-time ((appointment appointment) new-date-time) ;;;;run availability check
  (replace-appointment appointment
		       (client-ids appointment)
		       (employee-ids appointment)
		       (room-num (meeting-room appointment))
		       new-date-time
		       (duration appointment)
		       (notes appointment)))

(defmethod change-duration ((appointment appointment) new-duration) ;;;;run availability check
  (replace-appointment appointment
		       (client-ids appointment)
		       (employee-ids appointment)
		       (meeting-room appointment)
		       (dt appointment)
		       new-duration
		       (notes appointment)))

(defmethod change-notes ((appointment appointment) new-notes)
  (replace-appointment appointment
		       (client-ids appointment)
		       (employee-ids appointment)
		       (meeting-room appointment)
		       (dt appointment)
		       (duration appointment)
		       new-notes))

;;;;------------------------------------------------------------------------
;;;;Adding new appointments
;;;;------------------------------------------------------------------------

(defvar last-app-id (if (first *appointments*)
			(id (first *appointments*))
			10000))

(defun new-app-id ()
  "Generates a new appointment number."
  (setq last-app-id (+ last-app-id 1))
  last-app-id)

(defun update-last-app-id ()
  (if (null *appointments*)
      (setq last-app-id 10000)
      (setq last-app-id (id (first *appointments*)))))

(defun new-appointment (client-ids employee-ids room-num date-time duration notes)
  (add-appointment (make-appointment (new-app-id) client-ids employee-ids room-num date-time duration notes)))

;;;;------------------------------------------------------------------------
;;;;Backing up Appointments
;;;;------------------------------------------------------------------------

(defmethod backup-unit ((appointment appointment))
  (format nil "(load-saved-item (make-appointment ~a '(~{~a ~}) '(~{~a~}) ~a ~a ~a ~a))"
	  (id appointment)
	  (client-ids appointment)
	  (employee-ids appointment)
	  (id (meeting-room appointment))
	  (backup-unit (dt appointment))
	  (duration appointment)
	  (write-to-string (notes appointment))))

(defun refresh-appointment-backup ()
  (make-backup "appointments" (sort (copy-list *appointments*) #'(lambda (app1 app2)
								  (< (id app1) (id app2))))))

(defmethod load-saved-item ((appointment appointment))
  (push appointment *appointments*))

;;;;------------------------------------------------------------------------
;;;;Recurring Appointments
;;;;------------------------------------------------------------------------
(defmethod new-date-time ((appointment appointment) new-date-time)
  (make-appointment (new-app-id)
		    (client-ids appointment)
		    (employee-ids appointment)
		    (id (meeting-room appointment))
		    new-date-time
		    (duration appointment)
		    (notes appointment)))

(defmethod add-days ((appointment appointment) days)
  (new-date-time appointment (add-days (dt appoitnment) days)))

(defmethod next-day ((appointment appointment))
  (new-date-time appointment (next-day (dt appointment))))

(defmethod next-week ((appointment appointment))
  (new-date-time appointment (add-days (dt appointment) 7)))

(defmethod next-month ((appointment appointment))
  (new-date-time appointment (next-month (dt appointment))))

(defmethod next-year ((appointment appointment))
  (new-date-time appointment (next-year (dt appointment))))

(defmethod recurring ((appointment appointment) number-of-appointments day-gap)
    (loop :with a := appointment
	  :for i :from 1 :to number-of-appointments
	  :do (progn (add-appointment (add-days a day-gap))
		     (setf a (add-days a day-gap)))))

(defmethod weekly ((appointment appointment) &optional (cut-off 52))
  (loop :with a := appointment
	  :for i :from 1 :to cut-off
	  :do (progn (add-appointment (next-week a))
		     (setq a (next-week a)))))

(defmethod monthly ((appointment appointment) &optional (cut-off 12))
  (loop :with a := appointment
	:for i :from 1 :to cut-off
	:do (progn (add-appointment (next-month appointment))
		   (setf a (next-month appointment)))))

(defmethod yearly ((appointment appointment) &optional (cut-off 10))
  "Generates yearly appointments from a given appointment"
  (loop :with a := appointment
	:for i :from 1 :to cut-off
	:do (progn (add-appointment (next-year appointment))
		   (setf a (next-year appointment)))))


;;;;------------------------------------------------------------------------
;;;;
;;;;------------------------------------------------------------------------

