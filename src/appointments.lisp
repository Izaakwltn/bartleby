;;;; appointments.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :bartleby)

;;; Appointment Class/SQL object

(mito:deftable appointment () ; maybe separate class for group-classes, or appointment class
  ((client-id    :col-type (:int))
   (employee-id  :col-type (:int))
   (room-id      :col-type (:int))
   (timestamp    :col-type (:timestamptz))
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
	      (timestamp-from-sql (write-to-string timestamp))
	      (room-id-search room-id)
	      (client-id-search client-id)
              (employee-id-search employee-id)
              duration
	      notes))))

(defun make-appointment (client-id employee-id room-id timestamp duration notes)
  (make-instance 'appointment :client-id   client-id
		              :employee-id employee-id
			      :room-id     room-id
		              :timestamp   (sql-print timestamp)
			      :duration    duration
			      :notes       notes))

;;; Adding and removing Appointments

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

;;; Changing one attribute at a time: 

(defmethod change-client ((appointment appointment) id)
  (setf (slot-value appointment 'client-id) id)
  (mito:save-dao appointment))

(defmethod change-employee ((appointment appointment) id)
  (setf (slot-value appointment 'employee-id) id)
  (mito:save-dao appointment))

(defmethod change-room ((appointment appointment) id)
  (setf (slot-value appointment 'room-id) id)
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

;;; Recurring Appointments

(defmethod copy-new-timestamp ((appointment appointment) new-timestamp)
  (make-appointment (appointment-client-id appointment)
		    (appointment-employee-id appointment)
		    (appointment-room-id appointment)
		    new-timestamp
		    (appointment-duration appointment)
		    (appointment-notes appointment)))

(defmethod add-days ((appointment appointment) days)
  (copy-new-timestamp appointment (add-days (timestamp-from-sql (appointment-timestamp appointment)) days)))

(defmethod add-time ((appointment appointment) minutes)
  (copy-new-timestamp appointment (add-time (timestamp-from-sql (appointment-timestamp appointment)) minutes)))

(defmethod next-day ((appointment appointment))
  (copy-new-timestamp appointment (next-day (appointment-timestamp appointment))))

;(defmethod next-week ((appointment appointment))
 ; (copy-new-timestamp appointment (add-days (appointment-timestamp appointment) 7)))

(defmethod next-week ((appointment appointment))
  (add-days appointment 7))

(defmethod next-month ((appointment appointment))
  (copy-new-timestamp appointment (next-month (let ((at (appointment-timestamp appointment)))
					       (if (typep at 'simple-base-string)
							    (recurring-timestamp-format at)
							    at)))))

(defmethod next-year ((appointment appointment))
  (copy-new-timestamp appointment (next-year (let ((at (appointment-timestamp appointment)))
					       (if (typep at 'simple-base-string)
							    (recurring-timestamp-format at)
							    at)))))

(defun recurring-timestamp-format (t-string)
  (timestamp (date (parse-integer (subseq t-string 5 7))
                   (parse-integer (subseq t-string 8 10))
                   (parse-integer (subseq t-string 0 4)))
             (set-time (parse-integer (subseq t-string 11 13))
                       (parse-integer (subseq t-string 14 16)))))
  
(defmethod recurring ((appointment appointment) number-of-appointments day-gap)
    (loop :with a := appointment
	  :for i :from 1 :to number-of-appointments
	  :do (progn (add-appointment (add-days a day-gap))
		     (setf a (add-days a day-gap)))))

(defmethod weekly ((appointment appointment) &optional (cut-off 52))
  (loop :with a := appointment
	  :for i :from 1 :to cut-off
	  :do (add-appointment a)
	  :do (setq a (next-week a))))

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
  "Returns the total number of Appointments"
  (mito:count-dao 'appointment))

(defun appointment-id-search (appointment-id)
  "Searches for an appointment by id integer"
  (mito:find-dao 'appointment :id appointment-id))

(defun all-appointments ()
  "Returns a list of all appointments"
  (loop :with appointments := nil
	:with ac := (appointment-count)

	:for i :upfrom 1
	:do (setq appointments (if (null (appointment-id-search i))
				   appointments
				   (cons (appointment-id-search i) appointments)))
	:when (equal (length appointments) ac)
	  :do (return appointments)))
        ;:else :do (setq appointments (cons (appointment-id-search i) appointments))))

(defun future-appointments (appointment-list)
  "Returns all future appointments"
  (loop :for a :in (sort appointment-list #'(lambda (x y)
                                              (later-timestamp-p
                                               (timestamp-from-sql
                                                (write-to-string (appointment-timestamp y)))
                                               (timestamp-from-sql
                                                (write-to-string (appointment-timestamp x))))))
        :if (future-p (timestamp-from-sql (write-to-string (appointment-timestamp a))))
          :collect a))

(defun chronological-appointments (appointment-list)
  (sort appointment-list #'(lambda (x y)
			     (later-timestamp-p
			      (timestamp-from-sql
			       (write-to-string (appointment-timestamp y)))
                              (timestamp-from-sql
                               (write-to-string (appointment-timestamp x)))))))

  
(defun all-future-appointments ()
  (future-appointments (all-appointments)))

;(defgeneric future-appointments (object)
 ; (:documentation "Finds all future appointments for an object"))

(defun past-appointments (appointment-list)
  (loop :for i :in appointment-list
	:if (not (future-p (timestamp-from-sql (write-to-string (appointment-timestamp i)))))
	  :collect i))

(defun all-past-appointments ()
  "Returns all past appointments"
  (past-appointments (all-appointments)))

