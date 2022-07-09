;;;; time.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :bartleby)

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
	      (if (> hour 12) "pm" "am")))))


(defun set-time (hour minutes)
  (make-instance 'set-time :hour hour
		           :minutes minutes))

(defgeneric add-time (object minutes) ;;also used for date-time
  (:documentation "Adds a specified number of minutes to a given object"))
  
(defmethod add-time ((time set-time) minutes)
  "Adds a specified number of minutes to a given time."
  (cond ((zerop minutes) time)
	((and (equal (minutes time) 59)
	      (equal (hour time) 24))
	 (add-time (set-time 1 0) (- minutes 1)))
	((equal (minutes time) 59)
	 (add-time (set-time (+ 1 (hour time)) 0) (- minutes 1)))
	(t (add-time (set-time (hour time) (+ 1 (minutes time))) (- minutes 1)))))

(defun current-time ()
  "Returns the current time (Hours/Minutes)."
  (set-time (local-time:timestamp-hour (local-time:now))
	    (local-time:timestamp-minute (local-time:now))))

(defun later-time-p (time1 time2)
  "Returns t if time1 is later than time2"
  (cond ((> (hour time1) (hour time2)) t)
	((> (hour time2) (hour time1)) nil)
	((> (minutes time1) (minutes time2)) t)
	((> (minutes time2) (minutes time1)) nil)
	(t t)))

(defun later-time (time1 time2)                
  "Returns the later of two times."
  (cond ((> (hour time1) (hour time2)) time1)
	((> (hour time2) (hour time1)) time2)
	((> (minutes time1) (minutes time2)) time1)
	((> (minutes time2) (minutes time1)) time2)
	(t time1)))

(defun equal-time (time1 time2)
  "Checks whether two times are equal."
  (and (equal (hour time1) (hour time2))
       (equal (minutes time1) (minutes time2))))

(defun time-conflict-p (newtime earlytime latetime)
  "Checks whether a new time falls between two other times."
  (cond ((equal-time newtime earlytime) t)
	((later-time-p newtime latetime) nil)
	((later-time-p earlytime newtime) nil)
	((and (later-time-p newtime earlytime)
	      (later-time-p latetime newtime))
	 t)))
