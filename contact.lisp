;;;;contact.lisp
;;;;
;;;;

(in-package :schedulizer)

;;;;------------------------------------------------------------------------
;;;;Address Class
;;;;------------------------------------------------------------------------

(defclass address ()
  ((street-number :initarg :street-number
		  :accessor street-number)
   (street-name   :initarg :street-name
		  :accessor street-name)
   (city          :initarg :city
		  :accessor city)
   (state         :initarg :state
		  :accessor state)
   (zip-code      :initarg :zip-code
		  :accessor zip-code)))

(defmethod print-object ((obj address) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((street-number street-number)
		     (street-name   street-name)
		     (city          city)
		     (state         state)
		     (zip-code      zip-code))
	obj
      (format stream "~a ~a~%~a, ~a ~a" street-number street-name city state zip-code))))

(defun make-address (street-number street-name city state zip-code)
  (make-instance 'address :street-number street-number
		          :street-name   street-name
			  :city          city
			  :state         state
			  :zip-code      zip-code))

(defmethod address-backup ((address address))
  (format nil "(make-address ~a ~a ~a ~a ~a)"
	  (street-number address)
	  (street-name address)
	  (city address)
	  (state address)
	  (zip-code address)))

(defun random-address ()
 (make-address "2022" "Johnson Street" " Denver" "Kansas" "90000"))

;;;;------------------------------------------------------------------------
;;;;Phone Class
;;;;------------------------------------------------------------------------

(defclass phone-number ()
  ((country :initarg :country
	    :accessor country)
   (area    :initarg :area
	    :accessor area)
   (middle  :initarg :middle
	    :accessor middle)
   (end     :initarg :end
	    :accessor end)))

(defmethod print-object ((obj phone-number) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((country country)
		     (area area)
		     (middle middle)
		     (end end))
	obj
      (format stream "+~a(~a)~a-~a" country area middle end))))

(defun make-phone-number (number-string)
  "Stores a string phone number as an object including country and area codes"
  (let ((l (length number-string)))
    (make-instance 'phone-number :country (if (> l 10)
				            (subseq number-string 0 (- l 10))
				            1)
		                 :area (subseq number-string (- l 10) (- l 7))
				 :middle (subseq number-string (- l 7) (- l 4))
				 :end (subseq number-string (- l 4) l))))

(defun random-phone ()
  "Generates a random, probably not functioning US phone number."
  (make-phone-number
   (loop :with number := nil
        :for i :from 1 :to 10
	 :do (setf number
		   (concatenate 'string number (write-to-string (random 9))))
	 :finally (return number))))

(defmethod phone-backup ((phone-number phone-number))
  (format nil "(make-instance 'phone-number :country ~a :area ~a :middle ~a :end ~a)"
	  (country phone-number)
	  (area phone-number)
	  (middle phone-number)
	  (end phone-number)))

;;;;------------------------------------------------------------------------
;;;;Random entries for simulated contact information
;;;;------------------------------------------------------------------------

(defclass email-address ()
  ((username :initarg :username
	     :accessor username)
   (domain   :initarg :domain
	     :accessor domain)))

(defmethod print-object ((obj email-address) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((username username)
		     (domain domain))
	obj
      (format stream "~a@~a" username domain))))

(defun parse-email (email-string)
  "Parses an email address and splits username from domain."
  (loop :with switch := nil ;username, then domain
	:with u := ""
	:with d := ""

	:for ch :from 0 :to (- (length email-string) 1)
	:if (equal (subseq email-string ch (+ ch 1)) "@")
	  :do (setq switch t)
	:else :if (null switch)
	  :do (setq u (concatenate 'string u (subseq email-string ch (+ ch 1))))
	:else
	  :do (setq d (concatenate 'string d (subseq email-string ch (+ ch 1))))
	:finally (return (list u d))))


	  
(defun make-email (email-string)
  "Generates an email object from its username and domain."
  (let ((parsed-email (parse-email email-string)))
	(make-instance 'email-address :username (first parsed-email)
				      :domain (second parsed-email))))

(defmethod email-backup ((email-address email-address))
  (format nil "(make-instance 'email-address :username ~a :domain ~a)"
	  (username email-address)
	  (domain email-address)))

(defvar email-domains '("gmail.com" "yahoo.com" "hotmail.com" "aol.com" "msn.com"))

(defun auto-email (first-name last-name)
  "Automatically generates an email using someone's name"
  (make-email (concatenate 'string last-name "." first-name "@" (nth (random 4) email-domains))))
  
