;;;; timestamps.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :bartleby)

;;; Timestamps

(defclass timestamp ()
  ((date-o :initarg :date-o
	   :accessor date-o)
   (time-o :initarg :time-o
	   :accessor time-o)))

(defmethod print-object ((obj timestamp) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((date-o date-o)
		     (time-o time-o))
	obj
      (format stream "~a ~a" date-o time-o))))

(defun two-digits (numstring)
  (cond ((equal (length numstring) 2)
	 numstring)
	((equal (length numstring) 1)
	 (concatenate 'string "0" numstring))
	(t nil)))

(defmethod sql-print ((obj timestamp))
  (let ((string-year (write-to-string
		      (y (date-o obj))))
	(string-month (write-to-string
		       (m (date-o obj))))
        (string-day (write-to-string
		     (d (date-o obj))))
        (string-hour (write-to-string
		      (hour (time-o obj))))
        (string-minute (write-to-string
			(minutes (time-o obj)))))
    (format nil "~a-~a-~a ~a:~a:00"
	    string-year
	    (two-digits string-month)
	    (two-digits string-day)
	    (two-digits string-hour)
	    (two-digits string-minute))))

(defun timestamp (date time)
  (make-instance 'timestamp :date-o date
		            :time-o time))

(defun current-timestamp ()
  (timestamp (today) (current-time)))

(defun moment (m d yyyy hour minutes)
  "Simple input for a date and time."
  (timestamp (date m d yyyy) (set-time hour minutes)))

(defmethod add-days ((timestamp timestamp) days)
  "Adds a specified number of dates to the given date-time"
  (timestamp (add-days (timestamp timestamp) days) (time-o timestamp)))

(defmethod add-time ((timestamp timestamp) minutes)
  "Adds minutes to a date-time"
  (let ((ct (time-o timestamp))
	(cd (date-o timestamp)))
    (cond ((zerop minutes) timestamp)
 	  ((and (equal (minutes ct) 59)
	        (equal (hour ct) 23))
	   (add-time (timestamp (add-days cd 1) (add-time ct 1))
		     (- minutes 1)))
	  ((equal (minutes ct) 59)
	   (add-time (timestamp cd (add-time ct 1)) (- minutes 1)))
	  (t (add-time (timestamp cd (add-time ct 1)) (- minutes 1))))))

(defmethod next-day ((timestamp timestamp))
  (timestamp (add-days timestamp1)))

(defmethod change-date ((timestamp timestamp) new-date)
  (timestamp new-date
	     (time-o timestamp)))

(defmethod change-time ((timestamp timestamp) new-time)
  (timestamp (date-o timestamp)
	     new-time))

(defvar *midnight* (set-time 24 0))

(defmethod next-year ((timestamp timestamp))
  (timestamp (next-year (date-o timestamp)) (time-o timestamp)))

(defun later-timestamp-p (timestamp1 timestamp2)
  "Compares two date-times, returns t if the first is later"
  (if (and (later-date-p (date-o timestamp1)
		         (date-o timestamp2))
	   (later-time-p (time-o timestamp1)
			 (time-o timestamp2)))
      t
      nil))
