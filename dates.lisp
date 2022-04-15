;;;;dates.lisp
;;;;

(in-package :schedulizer)

;;;;------------------------------------------------------------------------
;;;;Date Class
;;;;------------------------------------------------------------------------

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
	      (second (assoc m month-names))
	      d
	      (number-suffix d)
	      y))))

(defun date (m d y)
  (make-instance 'date :m m
		       :d d
		       :y y))

(defmethod backup-unit ((date date))
  (format nil "(date ~a ~a ~a)"
	  (m date)
	  (d date)
	  (y date)))

;;;;------------------------------------------------------------------------
;;;;Date Functions
;;;;------------------------------------------------------------------------

(defun later-date (date1 date2)
  "Compares two dates, returns the later date."
  (cond ((> (y date1) (y date2)) date1)
	((> (y date2) (y date1)) date2)
	((> (m date1) (m date2)) date1)
	((> (m date2) (m date1)) date2)
	((> (d date1) (d date2)) date1)
	((> (d date2) (d date1)) date2)
	(t date1)))

(defun equal-date (date1 date2)
  "Determines whether two dates are equal."
  (if (and (equal (m date1) (m date2))
	   (equal (d date1) (d date2))
	   (equal (y date1) (y date2)))
	 t
	 nil))

(defun month-days (month year)
  "Given a month and a year, returns the number of days in that month."
  (second (assoc month (if (leap-year-p year)
			   leap-year-numbers
			   common-year-numbers))))

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
			       

;;;;------------------------------------------------------------------------
;;;;Date Calculations
;;;;------------------------------------------------------------------------

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

(defvar days-of-week '((0 "Sunday")
		       (1 "Monday")
		       (2 "Tuesday")
		       (3 "Wednesday")
		       (4 "Thursday")
		       (5 "Friday")
		       (6 "Saturday")))

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

(defmethod day-of-week ((date date))
  "Determines the day of the week for a given date."
  (let ((jan1 (second (assoc (y date) firsts-of-january))))
    (day-cycle jan1 (mod (- (day-nth date) 1) 7))))

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

;;;;------------------------------------------------------------------------
;;;;This day and that
;;;;------------------------------------------------------------------------

(defun today ()
  "Returns today's date."
  (date (local-time:timestamp-month (local-time:now))
        (local-time:timestamp-day   (local-time:now))
	(local-time:timestamp-year  (local-time:now))))

(defmethod next-day ((date date))
  (add-days date 1))

(defun tomorrow ()
  "Returns tomorrow's date."
  (next-day (today)))

;;;;------------------------------------------------------------------------
;;;;Date Time
;;;;------------------------------------------------------------------------

(defclass date-time ()
  ((date-o   :initarg :date-o
	   :accessor date-o)
   (time-o :initarg :time-o
	   :accessor time-o)))

(defmethod print-object ((obj date-time) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((date-o date-o)
		     (time-o time-o))
	obj
      (format stream "~a ~a" date-o time-o))))

(defun date-time (date time)
  (make-instance 'date-time :date-o date
		            :time-o time))

(defmethod backup-unit ((date-time date-time))
  (let ((cd (date-o date-time))
	(ct (time-o date-time)))
    (format nil "(date-time (date ~a ~a ~a) (set-time ~a ~a))"
	    (m cd)
	    (d cd)
	    (y cd)
	    (hour ct)
	    (minutes ct))))

(defmethod add-days ((date-time date-time) days)
  (date-time (add-days (date-o date-time) days) (time-o date-time)))

(defmethod add-time ((date-time date-time) minutes) 
  (let ((ct (time-o date-time))
	(cd (date-o date-time)))
    (cond ((zerop minutes) date-time)
 	  ((and (equal (minutes ct) 59)
	        (equal (hour ct) 23))
	   (add-time (date-time (add-days cd 1) (add-time ct 1))
		     (- minutes 1)))
	  ((equal (minutes ct) 59)
	   (add-time (date-time cd (add-time ct 1)) (- minutes 1)))
	  (t (add-time (date-time cd (add-time ct 1)) (- minutes 1))))))


;;;;------------------------------------------------------------------------
;;;;Week Class
;;;;------------------------------------------------------------------------

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

(defmethod last-week ((week week))
  (last-week (first (days week))))

(defgeneric next-week (object)
  (:documentation "Returns the following week"))

(defmethod next-week ((date date))
  (week (add-days date 7)))

(defmethod next-week ((week week))
  (next-week (first (days week))))

;;;;------------------------------------------------------------------------
;;;;Month Class
;;;;------------------------------------------------------------------------

(defclass month ()
  ((month-num  :initarg :month-num
	       :accessor month-num)
   (month-name :initarg :month-name
	       :accessor month-name)
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
  (make-instance 'month :month-num (m date)
		        :month-name (second (assoc (m date) month-names))
			:year (y date)
			:days (let ((m (m date))
				    (y (y date))
	                            (m-days (month-days (m date) (y date))))
				(loop :for i :from 1 :to m-days
	                              :collect (date m i y)))))

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

;;;;------------------------------------------------------------------------
;;;;
;;;;------------------------------------------------------------------------
