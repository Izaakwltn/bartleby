;;;; appointments.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :bartleby)

;;; Appointment Class/SQL object

(mito:deftable appointment () ; maybe separate class for group-classes, or appointment class
  ((client       :col-type client)
   (employee     :col-type employee)
   (meeting-room :col-type meeting-room)
   (timestamp    :col-type (:timestamp))
   (duration     :col-type (:int))
   (notes        :col-type (or (:varchar 128) :null)))
  (:conc-name appointment-))

(mito:ensure-table-exists 'appointment)

(defmethod print-object ((obj appointment) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((client-id     appointment-client-id)
		     (employee-id   appointment-employee-id)
		     (meeting-room  appointment-meeting-room-id)
		     (timestamp     appointment-timestamp)
		     (duration      appointment-duration)
		     (notes         appointment-notes))
	obj
      (format stream
	      "~%~%~a~%~%Room: ~a~%Client:~%~{~a~%~}~%Employee:~%~{~a~}~%Duration: ~a~%Notes: ~a~%"
	      timestamp
	      meeting-room
	      client-id
              employee-id
              duration
	      notes))))

(defun make-appointment (client-id employee-id meeting-room timestamp duration notes)
  (make-instance 'appointment :client       client-id
		              :employee     employee-id
			      :meeting-room meeting-room
		              :timestamp    timestamp
			      :duration     duration
			      :notes        notes))

;;; Adding and removing Appointments

(defvar *appointments* nil)

(defmethod add-appointment ((appointment appointment))
  "Adds an appointment to the appointment SQL table."
  (mito:insert-dao appointment))

(defmethod remove-appointment ((appointment appointment))
  "Removes an appointment from the *appointments* list"
  (mito:delete-dao appointment))

(defmethod replace-appointment ((appointment appointment) new-appointment)
  "Removes appointment, adds a replacement appointment."
  (remove-appointment appointment)
  (add-appointment new-appointment))

;(defmethod credit-appointment ((appointment appointment))
 ; (make-credit (date-o (dt appointment))
;	       (client appointment)
;	       appointment
;	       (duration appointment)
;	       nil))

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



;;;;;;;;;;;;;;;;;;;;;;
;(add-appointment (make-appointment (mito:find-dao 'client :id 1) (mito:find-dao 'employee :id 1) (mito:find-dao 'meeting-room :id 1) (sql-print (timestamp (date 7 13 2022) (set-time 10 30))) 45 "+15 makeup"))
