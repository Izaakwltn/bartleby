;;;; dates.lisp
;;;;
;;;; Copyright (c) Izaak Walton

(in-package :bartleby)

;;; Date Class

(defclass date ()
  ((month :initarg :m
          :accessor m)
   (day   :initarg :d
          :accessor d)
   (year  :initarg :y
          :accessor y)))

(defmethod print-object ((obj date) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((m m)
		     (d d)
		     (y y))
	obj
      (format stream "~a, ~a ~a~a, ~a"
	      (second (assoc (day-of-week (date m d y)) days-of-week))
	      (month-name m)
	      d
	      (number-suffix d)
	      y))))

(defmethod sql-print ((obj date))
  (let ((parsed-year  (write-to-string (y obj)))
        (parsed-month (write-to-string (m obj)))
        (parsed-day   (write-to-string (d obj))))
    (format nil
	    "~a-~a-~a"
	    parsed-year
	    (if (eq 1 (length parsed-month))
		(concatenate 'string "0" parsed-month)
		parsed-month)
	    (if (eq 1 (length parsed-month))
		(concatenate 'string "0" parsed-day)
		parsed-day))))

(defun date (m d y)
  (make-instance 'date :m m
		       :d d
		       :y y))

(defun date-from-sql (sql-date)
  "Creates a bartleby:date object from an sql-date string."
  (loop :with yyyy := ""
	:with mm := ""
	:with dd := ""
	:with selector := 0

        :for i :from 1 :to (length sql-date)
        :if (equal (subseq sql-date (- i 1) i) "-")
	  :do (setq selector (+ selector 1))
	:else :if (equal selector 0)
		:do (setq yyyy
			  (concatenate 'string
				       yyyy
				       (subseq sql-date (- i 1) i)))
	:else :if (equal selector 1)
		:do (setq mm
			  (concatenate 'string
				       mm
				       (subseq sql-date (- i 1) i)))
	:else
	  :do (setq dd
		    (concatenate 'string
				 dd
				 (subseq sql-date (- i 1) i)))
	:finally (return (date (parse-integer mm)
			       (parse-integer dd)
			       (parse-integer yyyy)))))
	     
;;; Date Functions

(defun equal-date (date1 date2)
  "Determines whether two dates are equal."
  (if (and (equal (m date1) (m date2))
	   (equal (d date1) (d date2))
	   (equal (y date1) (y date2)))
	 t
	 nil))

(defun later-date-p (date1 date2)
  "Returns t if date1 is later than date2."
  (cond ((equal-date date1 date2) nil)
        ((> (y date1) (y date2)) t)
	((> (y date2) (y date1)) nil)
	((> (m date1) (m date2)) t)
	((> (m date2) (m date1)) nil)
	((> (d date1) (d date2)) t)
	((> (d date2) (d date1)) nil)
	(t t)))

(defun later-date (date1 date2)
  "Compares two dates, returns the later date."
  (cond ((> (y date1) (y date2)) date1)
	((> (y date2) (y date1)) date2)
	((> (m date1) (m date2)) date1)
	((> (m date2) (m date1)) date2)
	((> (d date1) (d date2)) date1)
	((> (d date2) (d date1)) date2)
	(t date1)))

;(defun equal-date (date1 date2)
 ; "Determines whether two dates are equal."
  ;(if (and (equal (m date1) (m date2))
;	   (equal (d date1) (d date2))
;	   (equal (y date1) (y date2)))
;	 t
;	 nil))

(defun month-days (month year)
  "Given a month and a year, returns the number of days in that month."
  (second (assoc month (if (leap-year-p year)
			   leap-year-numbers
			   common-year-numbers))))

(defgeneric add-days (object days)
  (:documentation "Adds days to date, date-time, appointment, etc."))

(defmethod add-days ((date date) days)
  "Returns a date a specified number of days after a particular date."
  (cond ((zerop days) date)
	((and (equal (m date) 12)
	      (equal (d date) 31))
	 (add-days (date 1 1 (+ (y date) 1)) (- days 1)))
	((equal (d date) (month-days (m date) (y date)))
	 (add-days (date (+ (m date) 1) 1 (y date)) (- days 1)))
	(t (add-days (date (m date) (+ 1 (d date)) (y date))
		     (- days 1)))))

(defmethod sub-days ((date date) days)
  "Subtracts a specified number of days from a date."
  (cond ((zerop days) date)
	((and (equal (m date) 1)
	      (equal (d date) 1))
	 (sub-days (date 12 31 (- (y date) 1))
		   (- days 1)))
	((equal (d date) 1)
	 (sub-days (date (- (m date) 1)
			 (month-days (- (m date) 1) (y date))
			 (y date))
		   (- days 1)))
	(t (sub-days (date (m date) (- (d date) 1) (y date))
		     (- days 1)))))

(defgeneric next-year (object)
  (:documentation "Returns a date one year later"))

(defmethod next-year ((date date))
  (if (and (equal (m date) 2)
	   (equal (d date) 29))
      (date 3 1 (+ (y date) 1))
      (date (m date) (d date) (+ (y date) 1))))

;;; Date Calculations

(defvar common-year-numbers '((1  31)
			      (2  28)
			      (3  31)
			      (4  30)
			      (5  31)
			      (6  30)
			      (7  31)
			      (8  30)
			      (9  30)
			      (10 31)
			      (11 30)
			      (12 31)))

(defvar leap-year-numbers '((1  31)
			    (2  29)
			    (3  31)
			    (4  30)
			    (5  31)
			    (6  30)
			    (7  31)
			    (8  30)
			    (9  30)
			    (10 31)
			    (11 30)
			    (12 31)))

(defvar month-names '((1  "January")
		      (2  "February")
		      (3  "March")
		      (4  "April")
		      (5  "May")
		      (6  "June")
		      (7  "July")
		      (8  "August")
		      (9  "September")
		      (10 "October")
		      (11 "November")
		      (12 "December")))

(defun month-name (n-month)
  "Returns the name for a given month number"
  (second (assoc n-month month-names)))

(defun day-cycle (day-value change)
  "Cycles through days of the week as designated."
  (cond ((zerop change) day-value)
	((equal day-value 6) (day-cycle 0 (- change 1)))
	(t (day-cycle (+ day-value 1) (- change 1)))))

(defun leap-year-p (year)
  "Determines whether a given year is a leap year"
  (cond ((not (zerop (mod year 4))) nil)
	((not (zerop (mod year 100))) t)
	((not (zerop (mod year 400))) nil)
	(t t)))

(defmethod day-nth ((date date))
  "Returns how many days into the year the given date is"
  (let ((month-list (if (leap-year-p (y date))
			leap-year-numbers
			common-year-numbers)))
    (+ (cond ((equal (m date) 1) 0)
	     ((equal (m date) 2) 31)
	     (t (loop for i from 0 to (- (m date) 2)
		 sum (second (nth i month-list)))))
       (d date))))

(defun each-first-of-january (start-year start-day-of-week end-year)
  "Start from an arbitrary monday january 1st, go up to a specified year, store a list of (year day-of-week)"
  (loop :with day-of-week := start-day-of-week
	:for year :from start-year :to end-year
	:collect (list year day-of-week) :into firsts
	:do (if (leap-year-p year)
		(setf day-of-week (day-cycle day-of-week 2))
		(setf day-of-week (day-cycle day-of-week 1)))
	:finally (return firsts)))

(defvar firsts-of-january (each-first-of-january 1900 1 2100))

(defvar days-of-week '((0 "Sunday")
		       (1 "Monday")
		       (2 "Tuesday")
		       (3 "Wednesday")
		       (4 "Thursday")
		       (5 "Friday")
		       (6 "Saturday")))

(defgeneric day-of-week (object)
  (:documentation "Determines the day of the week"))

(defmethod day-of-week ((date date)) ;;;returns day number
  "Determines the day of the week for a given date."
  (let ((jan1 (second (assoc (y date) firsts-of-january))))
    (day-cycle jan1 (mod (- (day-nth date) 1) 7))))

(defgeneric day-of-week-name (object)
  (:documentation "Returns the name of the day of the week."))

(defmethod day-of-week-name ((date date))
  "Returns the name of the day of the week of a date."
  (second (assoc (day-of-week date) days-of-week)))

(defun number-suffix (n)
  "Given a number, returns the English suffix (st, nd, th)."
  (let ((s (write-to-string n)))
    (cond ((> (length s) 2)
	   (number-suffix
	    (parse-integer
	     (subseq s (- (length s) 2) (length s)))))
	  ((member n '(11 12 13 14 15 16 17 18 19)) "th")
	  ((> (length s) 1)
	   (number-suffix
	    (parse-integer
	     (subseq s (- (length s) 1) (length s)))))
	  ((equal n 1) "st")
	  ((equal n 2) "nd")
	  ((equal n 3) "rd")
	  (t "th"))))

;;; This day and that

(defun today ()
  "Returns today's date."
  (date (local-time:timestamp-month (local-time:now))
        (local-time:timestamp-day   (local-time:now))
	(local-time:timestamp-year  (local-time:now))))

(defgeneric next-day (object)
  (:documentation "Returns the following day from an object."))

(defmethod next-day ((date date))
  "Returns the day after a given date."
  (add-days date 1))

(defun tomorrow ()
  "Returns tomorrow's date."
  (next-day (today)))

;;; Week Class

(defclass week ()
  ((days :initarg :days
	 :accessor days)))
				    

(defmethod print-object ((obj week) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((days days))
	obj
      (format stream "~{~a~%~}~%" days))))

(defmethod most-recent-sunday ((date date))
  "Returns the most recent sunday."
  (if (equal (day-of-week date) 0)
      date
      (most-recent-sunday (sub-days date 1))))

(defmethod week ((date date))
  "Generates the specific week for a given day"
  (make-instance 'week :days (let ((sunday (most-recent-sunday date)))
			       (loop :for i :from 0 :to 6
	                             :collect (add-days sunday i)))))

(defun this-week ()
  (week (today)))

(defgeneric last-week (object)
  (:documentation "Returns the previous week"))

(defmethod last-week ((date date))
  (week (sub-days date 7)))

;(defmethod last-week ((timestamp timestamp))
 ; (week (sub-days (date-o timestamp) 7)))

(defmethod last-week ((week week))
  (last-week (first (days week))))

(defgeneric next-week (object)
  (:documentation "Returns the following week"))

(defmethod next-week ((date date))
  (week (add-days date 7)))

;(defmethod next-week ((timestamp timestamp))
 ; (week (add-days (date-o timestamp) 7)))

(defmethod next-week ((week week))
  (next-week (first (days week))))

;;; Month Class

(defclass month ()
  ((num  :initarg :num
	       :accessor num)
   (name :initarg :name
	       :accessor name)
   (year       :initarg :year
	       :accessor year)
   (days       :initarg :days
	       :accessor days)))

(defmethod print-object ((obj month) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((month-name month-name)
		     (year year)
		     (days days))
	obj
      (format stream "~a ~a:~%~{~a~%~}~%" month-name year days))))

(defmethod month ((date date))
  (let ((m (m date))
        (y (y date)))
    (let ((m-days (month-days m y)))
      (make-instance 'month :num m
                            :name (month-name m)
                            :year y
                            :days (loop :for i :from 1 :to m-days
	                                :collect (date m i y))))))

(defun week-gatherer (date last-day-of-month)
  (cond ((later-date-p (first (days (week date))) last-day-of-month) nil)
        (t (cons (week date) (week-gatherer
                              (add-days date 7) last-day-of-month)))))

(defmethod gather-weeks ((month month))
  (week-gatherer (first (days month))
                 (first (last (days month)))))

(defun this-month ()
  (month (today)))

(defgeneric last-month (object)
  (:documentation "Generates the previous month of a given date, month, or week."))

(defmethod last-month ((date date))
  (let ((m (m date))
	(y (y date)))
    (month (date (if (equal m 1)
		     12
		     (- m 1))
		 1
		 (if (equal m 1)
		     (- y 1)
		     y)))))

(defmethod last-month ((month month))
  (last-month (first (days month))))

(defgeneric next-month (object)
  (:documentation "Returns the subsequent month."))

(defmethod next-month ((date date))
  (let ((m (m date))
	(y (y date)))
    (month (date (if (equal m 12)
		     1
		     (+ m 1))
		 1
		 (if (equal m 12)
		     (+ y 1)
		     y)))))

(defmethod next-month ((month month))
  (next-month (first (days month))))
