;;;; time.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :bartleby)

;;; Define hour/minute types

(defun clock-hour-p (n)
  "Checks whether a provided integer is between 1 and 24 inclusive."
  (and (> n 0) (<= n 24)))

(deftype clock-hour ()
  `(satisfies clock-hour-p))

(defun clock-minutes-p (n)
  (and (>= n 0) (< n 60)))

(deftype clock-minutes ()
  `(satisfies clock-minutes-p))

;;; Set-time class

(defclass set-time ()
  ((hour    :initarg :hour
	    :accessor hour) ;;;stored in 24 hour system
   (minutes :initarg :minutes
	    :accessor minutes)))

(defmethod print-object ((obj set-time) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((hour hour)
		     (minutes minutes))
	obj
      (format stream "~a:~a ~a"
	      (if (> hour 12) (- hour 12) hour)
	      (if (equal 1 (length (write-to-string minutes)))
		  (concatenate 'string "0" (write-to-string minutes))
		  minutes)
	      (if (and (< hour 24) (>= hour 12)) "pm" "am")))))

(defmethod pretty-print ((set-time set-time))
  "Prints the time in '00:00 am/pm' format"
  (let ((h (hour set-time))
        (m (minutes set-time)))
    (format nil "~a:~a ~a"
            (if (> h 12) (- h 12) h)
	    (if (equal 1 (length (write-to-string m)))
                (format nil "0~a" m)
		m)
	      (if (and (< h 24) (>= h 12)) "pm" "am"))))

(declaim (ftype (function (clock-hour clock-minutes) set-time) make-time))
(defun make-time (hour minutes)
  "Makes an instance of set-time"
  (check-type hour clock-hour)
  (check-type minutes clock-minutes)
  (make-instance 'set-time :hour hour
		           :minutes minutes))

(defgeneric add-time (object minutes)
  (:documentation "Adds a specified number of minutes to a given object"))
  
(defmethod add-time ((time set-time) minutes)
  "Adds a specified number of minutes to a given time."
  (check-type minutes integer)
  (cond ((zerop minutes) time)
	((and (equal (minutes time) 59)
	      (equal (hour time) 24))
	 (add-time (make-time 1 0) (- minutes 1)))
	((equal (minutes time) 59)
	 (add-time (make-time (+ 1 (hour time)) 0) (- minutes 1)))
	(t (add-time (make-time (hour time) (+ 1 (minutes time))) (- minutes 1)))))

(defun current-time ()
  "Returns the current time (Hours/Minutes)."
  (make-time (local-time:timestamp-hour (local-time:now))
	    (local-time:timestamp-minute (local-time:now))))

(declaim (ftype (function (set-time set-time) (or t null)) later-time-p))
(defun later-time-p (time1 time2)
  "Returns t if time1 is later than time2"
  (check-type time1 set-time)
  (check-type time2 set-time)
  (cond ((> (hour time1) (hour time2)) t)
	((> (hour time2) (hour time1)) nil)
	((> (minutes time1) (minutes time2)) t)
	((> (minutes time2) (minutes time1)) nil)
	(t t)))

(declaim (ftype (function (set-time set-time) set-time) later-time))
(defun later-time (time1 time2)                
  "Returns the later of two times."
  (check-type time1 set-time)
  (check-type time2 set-time)
  (cond ((> (hour time1) (hour time2)) time1)
	((> (hour time2) (hour time1)) time2)
	((> (minutes time1) (minutes time2)) time1)
	((> (minutes time2) (minutes time1)) time2)
	(t time1)))

(declaim (ftype (function (set-time set-time) (or t null)) equal-time))
(defun equal-time (time1 time2)
  "Checks whether two times are equal."
  (check-type time1 set-time)
  (check-type time2 set-time)
  (and (equal (hour time1) (hour time2))
       (equal (minutes time1) (minutes time2))))

(declaim (ftype (function (set-time set-time set-time) (or t null)) time-conflict-p))
(defun time-conflict-p (newtime earlytime latetime)
  "Checks whether a new time falls between two other times."
  (check-type newtime set-time)
  (check-type earlytime set-time)
  (check-type latetime set-time)
  (cond ((equal-time newtime earlytime) t)
	((later-time-p newtime latetime) nil)
	((later-time-p earlytime newtime) nil)
	((and (later-time-p newtime earlytime)
	      (later-time-p latetime newtime))
	 t)))

;;; Timezones (headache)

;;; the more functional approach would be to just have (current-timezone)

(defun current-timezone ()
  local-time:*default-timezone*)

(defun current-timezone-offset ()
  (/ (nth 9
                         (multiple-value-list
                          (local-time:decode-timestamp (local-time:now))))
                       3600))

(defun current-timezone-offset-minutes ()
  (* (current-timezone-offset) 60))

;;; probably don't need this but leaving it for now

(defvar tz local-time:*default-timezone*)

(defvar *tz-offset* (/ (nth 9
                         (multiple-value-list
                          (local-time:decode-timestamp (local-time:now))))
                       3600))

(defvar *tz-offset-minutes* (* *tz-offset* 60))

(defun refresh-timezone ()
  "Resets timezone information."
  (setq tz local-time:*default-timezone*)
  (setq *tz-offset* (/ (nth 9
                         (multiple-value-list
                          (local-time:decode-timestamp (local-time:now))))
                       3600))
  (setq *tz-offset-minutes* (* *tz-offset* 60)))
