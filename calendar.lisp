;;;;calendar.lisp

(in-package :bartleby)

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
(defun string-date-error (string)
  "Checks that the string has either mm/dd/yyyy format or m/d/yyyy";;;;fix this- (string-to-date "03/26/1993") made March 36th....
(defun string-to-date (string) ; input mm/dd/yyyy, maybe can handle 
  "Parse mm/dd/yyyy into address instance."
  (loop :with mm   := ""
	:with dd   := ""
	:with yyyy := ""
        :with slot := 0
	
	:for i :from 1 :to (length string)
	:if (string-equal (subseq string (- i 1) i) "/")
	  :do (setq slot (+ slot 1))
	:else
	  :if (equal slot 0)
	    :do (setq mm (concatenate 'string mm (subseq string (- i 1) i)))
	:else
	  :if (equal slot 1)
	    :do (setq dd (concatenate 'string mm (subseq string (- i 1) i)))
	:else
	  :do (setq yyyy (concatenate 'string yyyy (subseq string (- i 1) i)))
	:finally (return (date (parse-integer mm) (parse-integer dd) (parse-integer yyyy)))))
	  

(defmethod date-to-string ((date date))
  (let ((m (write-to-string (m date)))
	(d (write-to-string (d date)))
	(y (write-to-string (y date))))
    (concatenate 'string
		 (if (equal (length m) 1)
		     (concatenate 'string "0" m)
		     m)
		 "/"
		 (if (equal (length d) 1)
		     (concatenate 'string "0" d)
		     d)
		 "/"
		 y)))
		 

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
				   
(defun today ()
  "Returns today's date."
  (date (local-time:timestamp-month (local-time:now))
        (local-time:timestamp-day   (local-time:now))
	(local-time:timestamp-year  (local-time:now))))


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
;;;;Time Calculations
;;;;------------------------------------------------------------------------

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

(defmethod backup-unit ((set-time set-time))
  (format nil "(set-time ~a ~a)"
	  (hour set-time)
	  (minutes set-time)))

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
	((equal-time newtime (later-time newtime latetime)) nil)
	((equal-time earlytime (later-time newtime earlytime)) nil)
	((and (equal-time newtime (later-time newtime earlytime))
	      (equal-time latetime (later-time newtime latetime)))
	 t)))

;;;;------------------------------------------------------------------------
;;;;
;;;;------------------------------------------------------------------------
