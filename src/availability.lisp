;;;; availability.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :bartleby)

;;; Object availability is determined with three overlapped systems:
;;; First, an availability chart that shows general, long-term scheduling
;;; Second, a blocking system for marking short term availabilities
;;; Third, availability-checking which cycles through appointments to find open spots


;;; Availability class and formatting system

(defun write-availability (start-time end-time)
  "Converts two times into a condensed 'HH:MM-HH:MM' string for sql storage."
  (concatenate 'string
	       (two-digits (write-to-string (hour start-time)))
	       ":"
	       (two-digits (write-to-string (minutes start-time)))
	       "-"
	       (two-digits (write-to-string (hour end-time)))
	       ":"
	       (two-digits (write-to-string (minutes end-time)))))

(defun read-availability (avail-string)
  "Converts 'HH:MM-HH:MM' string into two set-time instances."
  (list (set-time (parse-integer (subseq avail-string 0 2))
		  (parse-integer (subseq avail-string 3 5)))
	(set-time (parse-integer (subseq avail-string 6 8))
		  (parse-integer (subseq avail-string 9 11)))))   
  
(mito:deftable availability () 
  ((object-id :col-type (:int))
   (monday    :col-type (or (:char 11) :null))
   (tuesday   :col-type (or (:char 11) :null))
   (wednesday :col-type (or (:char 11) :null))
   (thursday  :col-type (or (:char 11) :null))
   (friday    :col-type (or (:char 11) :null))
   (saturday  :col-type (or (:char 11) :null))
   (sunday    :col-type (or (:char 11) :null))))

(mito:ensure-table-exists 'availability)

(defmethod print-object ((obj availability) stream)
  (print-unreadable-object (obj stream :type t)
    obj
    (format stream
            "Monday ~a~%Tuesday ~a~%Wednesday ~a~%Thursday ~a~%Friday ~a~%Saturday ~a~%Sunday ~a~%"
            (availability-monday obj)
            (availability-tuesday obj)
            (availability-wednesday obj)
            (availability-thursday obj)
            (availability-friday obj)
            (availability-saturday obj)
            (availability-sunday obj))))

;;; Making availability charts

(defgeneric make-availability (object monday tuesday wednesday thursday friday saturday sunday)
  (:documentation "Makes a complete availability chart for a given object"))

(defmethod make-availability ((employee employee) mon tue wed thu fri sat sun)
  (make-instance 'availability :object-id (mito:object-id employee)
                               :monday mon
                               :tuesday tue
                               :wednesday wed
                               :thursday thu
                               :friday fri
                               :saturday sat
                               :sunday sun))

(defmethod make-availability ((meeting-room meeting-room) mon tue wed thu fri sat sun)
  (make-instance 'availability :object-id (mito:object-id meeting-room)
                               :monday mon
                               :tuesday tue
                               :wednesday wed
                               :thursday thu
                               :friday fri
                               :saturday sat
                               :sunday sun))

(defmethod add-availability ((availability availability))
  "Adds an availability chart to the sql db"
  (mito:insert-dao availability))

;;; Blank availability charts for temporary use

(defgeneric blank-availability (object)
  (:documentation "Adds a blank availability chart for a given object"))

(defmethod blank-availability ((employee employee))
  (mito:insert-dao (make-instance 'availability
				  :object-id (mito:object-id employee))))

(defmethod blank-availability ((meeting-room meeting-room))
  (mito:insert-dao (make-instance 'availability
				  :object-id (mito:object-id meeting-room))))

;;; Finding an object's availability

(defgeneric find-availability (object)
  (:documentation "Finds the availability list for a given object"))

(defmethod find-availability ((employee employee))
  (mito:find-dao 'availability :object-id (mito:object-id employee)))

(defmethod find-availability ((meeting-room meeting-room))
  (mito:find-dao 'availability :object-id (mito:object-id meeting-room)))

;;; Change availability for an object, one day at a time

(defgeneric change-availability (object day avail-string)
 (:documentation "Changes the monday availability for an object"))

(defmethod change-availability ((employee employee) day avail-string)
  (let ((avail (find-availability employee)))
    (setf (slot-value avail day) avail-string)
    (mito:save-dao avail)))

(defmethod change-availability ((meeting-room meeting-room) day avail-string)
  (let ((avail (find-availability meeting-room)))
    (setf (slot-value avail day) avail-string)
    (mito:save-dao avail)))

;;; Day-of-the-week availability processing

(defvar availability-days '((1 #'availability-monday)
                            (2 #'availability-tuesday)
                            (3 #'availability-wednesday)
                            (4 #'availability-thursday)
                            (5 #'availability-friday)
                            (6 #'availabaility-saturday)
                            (7 #'availability-sunday)))

(defmethod day-of-week-avail ((availability availability) day-of-week)
  (funcall (eval (second (assoc day-of-week availability-days)))
           availability))


;;; Appointment Conflict Calculations

(defgeneric appointments (object)
  (:documentation "Returns a list of appointments associated with an object"))

(defmethod appointments ((client client))
  (let ((c (mito:object-id client)))
    (loop :for i :in (all-appointments)
	  :if (equal c (appointment-client-id i))
	    :collect i :into matches
	  :finally (return matches))))

(defmethod appointments ((employee employee))
  "Will return all appointments relating to the employee."
  (let ((e (mito:object-id employee)))
    (loop :for i :from 1 :to (appointment-count)
	  :if (equal e (appointment-employee-id (mito:find-dao 'appointment :id i)))
	    :collect (mito:find-dao 'appointment :id i) :into matches
	  :finally (return apts))))

(defmethod appointments ((meeting-room meeting-room))
  "Returns all appointments relating to the meeting-room."
  (loop :for i :from 1 :to (appointment-count)
	:if (equal e (appointment-room-id (mito:find-dao 'appointment :id i)))
	  :collect (mito:find-dao 'appointment :id i) :into matches
	:finally (return matches)))

(defmethod appointments ((date date))
  (loop :with matches := nil

        :for i :from 1 to (appointment-count)
	:if (mito:find-dao 'appointment :id i)
            :do (if (equal-date date
                        (date-o
                         (timestamp-from-sql
                          (write-to-string
                           (appointment-timestamp (mito:find-dao 'appointment :id i))))))
                    (setq matches (cons (appointment-id-search i) matches)))
	  ;:collect (mito:find-dao 'appointment :id i) :into matches
	:finally (return matches)))

(defmethod appointments ((week week))
  (loop :for i :from 1 :to (appointment-count)
        :if (member (date-o (appointment-timestamp (mito:find-dao 'appointment :id i)))
                    (days week))
	  :collect (mito:find-dao 'appointment :id i) :into matches
	:finally (return matches)))

(defmethod appointments ((month month))
  (loop :for i :from 1 :to (appointment-count)
	:if (member (date-o (appointment-timestamp (mito:find-dao 'appointment :id i)))
                    (days month))
	  :collect (mito:find-dao 'appointment :id i) :into matches
	:finally (return matches)))

;;; Blocks for irregular unavailabilities

(defgeneric block-off (object timestamp duration)
  (:documentation "Creates a blank appointment block for the given object."))

(defmethod block-off ((employee employee) timestamp duration)
  (add-appointment
   (make-appointment 0 (mito:object-id employee) 404 timestamp duration "Unavailable")))

(defmethod block-off ((meeting-room meeting-room) timestamp duration)
  (add-appointment
   (make-appointment 0 0 (mito:object-id meeting-room) timestamp duration "Unavailable")))

;;; Appointment conflict calculations:

(defun overlap-p (appointment1 appointment2)
  "Determines whether two appointments overlap in time."
  (or (time-conflict-p (start-time appointment1)
		       (start-time appointment2)
		       (end-time appointment2))
     (time-conflict-p (start-time appointment2)
		       (start-time appointment1)
		       (end-time appointment1))))


;;; Available Time Slots

(defgeneric available-times (object start-timestamp end-timestamp)
  (:documentation "Finds all available 30 min timeslots for the given time period."))

;(defmethod available-times ((employee employee) start-timestamp end-timestamp)
 ; (cond ((later-timestamp start-timestamp
  ;                        (first (read-availability
   ;                               (day-of-week-avail
     ;                              (find-availability employee)
    ;                               (day-of-week (date-o start-timestamp))))))

                                        ; steps for available times
                                        ; first, start with either the start-timestamp
                                        ; or the beginning of the availability for that day

                                        ; then cycle through 30 minute time slots until
                                        ; there is a conflict with either the end of shift
                                        ; or end-timestamp
                                        ; then cycle through the appointments for that employee
                                        ; and remove any "available" slots that conflict



;;; Appointment conflict calculations:

(defun overlap-p (appointment1 appointment2)
  "Determines whether two appointments overlap in time."
  (or (time-conflict-p (start-time appointment1)
		       (start-time appointment2)
		       (end-time appointment2))
     (time-conflict-p (start-time appointment2)
		       (start-time appointment1)
		       (end-time appointment1))))

;(defmethod available-p ((appointment appointment))
 ; "Determines whether a suggested appointment poses conflicts."
  ;(let ((e (id (employee appointment)))
;	(r (id (meeting-room appointment)))
;	(c (id (meeting-room appointment))))
 ;   (loop :for a :in *appointments*
;	  :if (and (or (equal e (id (employee a)))
;		       (equal r (id (meeting-room a)))
;		       (equal c (id (client a))))
;		   (overlap-p a appointment))
;	    :do (return nil)
;	  :else
;	    :do (return t))))

(defgeneric availability-cycle (object start-time start-date end-time end-date duration)
  (:documentation "Cycles through the given time-frame, returns all open slots."))

;(defmethod availability-cycle ((employee employee) start-time start-date end-time end-date duration);
  ;(loop :with ct      := start-time
;	:with cd      := start-date
;	:with opts    := nil
;
;	:if (and (equal-date (later-date cd end-date) cd)
;		 (equal-time (later-time ct end-time) ct))
;	  :do (return opts)
;	:else :if (available-p (make-appointment 0 0 (id employee) 0 cd ct duration ""))
;		:do (progn (setq opts
;				 (cons (make-appointment
;					0 0 (id employee) 0 cd ct duration "")
;				       opts))
;			   (setq ct (add-time ct duration))
;;			   (if (and (equal (hour ct) 24)
;				    (> (+ (minutes ct) duration) 60))
;			       (setq cd (add-days cd 1))))
;	:else :do (progn (setq ct (add-time ct duration))
;			        (if (and (equal (hour ct) 24)
;;				         (> (+ (minutes ct) duration) 60))
;				    (setq cd (add-days cd 1))))))


;(defmethod availability-cycle ((meeting-room meeting-room) start-time start-date end-time end-date duration)
 ; (loop :with ct      := start-time
;	:with cd      := start-date
;	:with opts    := nil
;
;	:if (and (equal-date (later-date cd end-date) cd)
;		 (equal-time (later-time ct end-time) ct))
;	  :do (return opts)
;	:else :if (available-p (make-appointment 0 0 0 (id meeting-room) cd ct duration ""))
;		:do (progn (setq opts
;				 (cons (make-appointment
;					0 0 0 (id meeting-room) cd ct duration "")
;				       opts))
;			   (setq ct (add-time ct duration))
;			   (if (and (equal (hour ct) 24)
;				    (> (+ (minutes ct) duration) 60))
;			       (setq cd (add-days cd 1))))
;	:else :do (progn (setq ct (add-time ct duration))
;			        (if (and (equal (hour ct) 24)
;				         (> (+ (minutes ct) duration) 60))
;				    (setq cd (add-days cd 1))))))



;;;;------------------------------------------------------------------------
;;;;Set Availability functions
;;;;------------------------------------------------------------------------

;;;eventually have a system for setting 
