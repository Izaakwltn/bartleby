;;;; timestamps.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :bartleby)

;;; Timestamps

(defclass timestamp ()
  ((date-o :initarg :date-o ; if I just make #'make-date, date-o can be less stupid
	   :accessor date-o)
   (time-o :initarg :time-o
	   :accessor time-o))) ; maybe add timezone

(defmethod print-object ((obj timestamp) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((date-o date-o)
		     (time-o time-o))
	obj
      (format stream "~a ~a" date-o time-o))))

(declaim (ftype (function (string) (or t null)) two-digits))
(defun two-digits (numstring)
  "Ensures that a number string has 2 digits"
  (cond ((equal (length numstring) 2)
	 numstring)
	((equal (length numstring) 1)
	 (concatenate 'string "0" numstring))
	(t nil)))

(defmethod sql-print ((obj timestamp))
  (let (;(ts (add-time obj (- *tz-offset-minutes*)))
         (string-year (write-to-string
	               (y (date-o obj))))
	 (string-month (write-to-string
                        (m (date-o obj))))
         (string-day (write-to-string
		      (d (date-o obj))))
         (string-hour (write-to-string
 		       (hour (time-o obj))))
         (string-minute (write-to-string
			 (minutes (time-o obj)))))
    (format nil "~a-~a-~a ~a:~a:00~a~a" ; +_ 02
	    string-year
	    (two-digits string-month)
	    (two-digits string-day)
	    (two-digits string-hour)
	    (two-digits string-minute)
            (if (>= *tz-offset* 0)
               "+"
                "-")
            (two-digits (write-to-string (abs *tz-offset*))))))

;;;; 
(alexa:define-string-lexer timestamp-parser
  ((:noise "[^0-9]")
   (:num "[0-9][0-9]*"))
   ;(:noise "^[0-9]"))
  ("{{NOISE}}" nil)
  ("{{NUM}}" (return (princ-to-string $@))))

(defun parse-timestamp (t-string)
  "Breaks a timestamp into number values."
  (loop :with lexer := (timestamp-parser t-string)
        :for tok := (funcall lexer)
        :while tok
        :collect tok))

(defun timestamp-from-sql (t-string)
  "Takes a sql timestamp, returns a bartleby timestamp."
  (let ((parsed (mapcar #'parse-integer
                        (parse-timestamp t-string))))
    (make-timestamp (date (second parsed)
                     (third parsed)
                     (first parsed))
               (make-time (nth 3 parsed)
                         (nth 4 parsed)))))

(defun timestamp-from-sql-with-offset (t-string)
  (add-time (timestamp-from-sql t-string) (current-timezone-offset-minutes)))

(declaim (ftype (function (date set-time) timestamp) make-timestamp)) 
(defun make-timestamp (date time) ;should timestamp include timezone?
  (make-instance 'timestamp :date-o date
		            :time-o time))

(defun current-timestamp ()
  "Shows the current Date/time"
  (make-timestamp (today) (current-time)))

(declaim (ftype (function (calendar-month integer calendar-year clock-hour clock-minutes) timestamp)))
(defun moment (m d yyyy hour minutes)
  "Simple input for a date and time."
  (make-timestamp (date m d yyyy) (make-time hour minutes)))

(defmethod add-days ((timestamp timestamp) days)
  "Adds a specified number of dates to the given date-time"
  (make-timestamp (add-days (date-o timestamp) days) (time-o timestamp)))

(defmethod add-time ((timestamp timestamp) minutes)
  "Adds minutes to a date-time"
  (let ((ct (time-o timestamp))
	(cd (date-o timestamp)))
    (cond ((zerop minutes) timestamp)
 	  ((and (equal (minutes ct) 59)
	        (equal (hour ct) 23))
	   (add-time (make-timestamp (add-days cd 1) (add-time ct 1))
		     (- minutes 1)))
	  ((equal (minutes ct) 59)
	   (add-time (make-timestamp cd (add-time ct 1)) (- minutes 1)))
	  (t (add-time (make-timestamp cd (add-time ct 1)) (- minutes 1))))))

(defmethod day-of-week ((timestamp timestamp))
  (day-of-week (date-o timestamp)))

(defmethod day-of-week-name ((timestamp timestamp))
  (day-of-week (date-o timestamp)))

;;; Altering timestamps

(defmethod change-date ((timestamp timestamp) new-date)
  (make-timestamp new-date
	     (time-o timestamp)))

(defmethod change-time ((timestamp timestamp) new-time)
  (make-timestamp (date-o timestamp)
	     new-time))

(defvar *midnight* (make-time 24 0))

;;; Checking status relative to current timestamp

(defun later-timestamp-p (timestamp1 timestamp2)
  "Compares two date-times, returns t if the first is later"
  (cond ((equal-date (date-o timestamp1)
                     (date-o timestamp2))
         (if (later-time-p (time-o timestamp1)
                           (time-o timestamp2))
             t nil))
        ((later-date-p (date-o timestamp1)
                       (date-o timestamp2))
         t)
        (t nil)))

(defun later-timestamp (timestamp1 timestamp2)
  "Compares two date-times, returns the later of the two"
  (cond ((equal-date (date-o timestamp1)
                     (date-o timestamp2))
         (if (later-time-p (time-o timestamp1)
                           (time-o timestamp2))
             timestamp1 timestamp2))
        ((later-date-p (date-o timestamp1)
                       (date-o timestamp2))
         timestamp1)
        (t timestamp2)))

(defun future-p (timestamp)
  "Determines whether a timestamp is in the future"
  (later-timestamp-p timestamp (current-timestamp)))
;;; Relative timestamps

(defmethod next-day ((timestamp timestamp))
  (make-timestamp (add-days timestamp 1)
             (time-o timestamp)))

(defmethod previous-day ((timestamp timestamp))
  (make-timestamp (sub-days timestamp 1)
             (time-o timestamp)))

(defmethod last-week ((timestamp timestamp))
  (week (sub-days (date-o timestamp) 7)))

(defmethod next-week ((timestamp timestamp))
  (week (add-days (date-o timestamp) 7)))

(defmethod next-year ((timestamp timestamp))
  (make-timestamp (next-year (date-o timestamp)) (time-o timestamp)))
