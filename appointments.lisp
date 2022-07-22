;;;; appointments.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :bartleby)

;;; Appointment Class/SQL object

(mito:deftable appointment () ; maybe separate class for group-classes, or appointment class
  ((client-id    :col-type (:int))
   (employee-id  :col-type (:int))
   (room-id      :col-type (:int))
   (timestamp    :col-type (:timestamp))
   (duration     :col-type (:int))
   (notes        :col-type (or (:varchar 128) :null)))
  (:conc-name appointment-))

(mito:ensure-table-exists 'appointment)

(defmethod print-object ((obj appointment) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((client-id     appointment-client-id)
		     (employee-id   appointment-employee-id)
		     (room-id       appointment-room-id)
		     (timestamp     appointment-timestamp)
		     (duration      appointment-duration)
		     (notes         appointment-notes))
	obj
      (format stream
	      "~%~%~a~%~%Room: ~a~%Client:~%~a~%Employee:~%~a~%Duration: ~a~%Notes: ~a~%"
	      timestamp
	      (room-id-search room-id)
	      (client-id-search client-id)
              (employee-id-search employee-id)
              duration
	      notes))))

(defun make-appointment (client-id employee-id room-id timestamp duration notes)
  (make-instance 'appointment :client-id   client-id
		              :employee-id employee-id
			      :room-id     room-id
		              :timestamp   timestamp
			      :duration    duration
			      :notes       notes))

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

(defun appointment-id-search (app-id)
  (find-if #'(lambda (a)
	       (equal (id a) app-id))
	   *appointments*))

(defun appointment-count ()
  (mito:count-dao 'appointment))

;;; Changing one attribute at a time: 

(defmethod change-client ((appointment appointment) client-id)
  (setf (slot-value appointment 'client-id) client-id)
  (mito:save-dao appointment))

(defmethod change-employee ((appointment appointment) employee-id)
  (setf (slot-value appointment 'employee-id) employee-id)
  (mito:save-dao appointment))

(defmethod change-room ((appointment appointment) room-id)
  (setf (slot-value appointment 'room-id) room-id)
  (mito:save-dao appointment))

(defmethod change-timestamp ((appointment appointment) timestamp)
  (setf (slot-value appointment 'timestamp) timestamp)
  (mito:save-dao appointment))

(defmethod change-duration ((appointment appointment) duration)
  (setf (slot-value appointment 'duration) duration)
  (mito:save-dao appointment))

(defmethod change-room ((appointment appointment) notes)
  (setf (slot-value appointment 'notes) notes)
  (mito:save-dao appointment))

;;;Recurring Appointments

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

;;; Searching Appointments

(defun appointment-count ()
  (mito:count-dao 'appointment))

(defun appointment-id-search (appointment-id)
  (mito:find-dao 'appointment :id appointment-id))

(defgeneric appointments (object)
  (:documentation "Returns all appointments associated with an object."))

(defmethod appointments ((client client))
  (let ((c (client-id client)))
    (loop :for i :from 1 :to (appointment-count)
	  :if (equal c (client-id (mito:find-dao 'appointment :id i)))
	    :collect (mito:find-dao 'appointment :id i) :into matches
	  :finally (return matches))))
		     
