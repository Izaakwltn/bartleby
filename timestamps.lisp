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
  "Ensures that a number string has 2 digits"
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

;;;;parse with a lexer instead of
(alexa:define-string-lexer timestamp-parser
  ((:noise "[^0-9]")
   (:num "[0-9][0-9]*"))
   ;(:noise "^[0-9]"))
  ("{{NOISE}}" nil)
  ("{{NUM}}" (return (princ-to-string $@))))

(defun parse-timestamp (t-string)
  (loop :with lexer := (timestamp-parser t-string)
        :for tok := (funcall lexer)
        :while tok
        :collect tok))

(defun timestamp-from-sql (t-string)
  (let ((parsed (mapcar #'parse-integer
                        (parse-timestamp t-string))))
    (timestamp (date (second parsed)
                     (third parsed)
                     (first parsed))
               (set-time (nth 3 parsed)
                         (nth 4 parsed)))))
(defun timestamp (date time)
  (make-instance 'timestamp :date-o date
		            :time-o time))

(defun current-timestamp ()
  "Shows the current Date/time"
  (timestamp (today) (current-time)))

(defun moment (m d yyyy hour minutes)
  "Simple input for a date and time."
  (timestamp (date m d yyyy) (set-time hour minutes)))

(defmethod add-days ((timestamp timestamp) days)
  "Adds a specified number of dates to the given date-time"
  (timestamp (add-days (date-o timestamp) days) (time-o timestamp)))

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

(defmethod day-of-week ((timestamp timestamp))
  (day-of-week (date-o timestamp)))

(defmethod day-of-week-name ((timestamp timestamp))
  (day-of-week (date-o timestamp)))

;;; Altering timestamps

(defmethod change-date ((timestamp timestamp) new-date)
  (timestamp new-date
	     (time-o timestamp)))

(defmethod change-time ((timestamp timestamp) new-time)
  (timestamp (date-o timestamp)
	     new-time))

(defvar *midnight* (set-time 24 0))

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
  (if (later-timestamp-p timestamp (current-timestamp))
      t
      nil))

;;; Relative timestamps

(defmethod next-day ((timestamp timestamp))
  (timestamp (add-days timestamp 1)
             (time-o timestamp)))

(defmethod previous-day ((timestamp timestamp))
  (timestamp (sub-days timestamp 1)
             (time-o timestamp)))

(defmethod last-week ((timestamp timestamp))
  (week (sub-days (date-o timestamp) 7)))

(defmethod next-week ((timestamp timestamp))
  (week (add-days (date-o timestamp) 7)))

(defmethod next-year ((timestamp timestamp))
  (timestamp (next-year (date-o timestamp)) (time-o timestamp)))
