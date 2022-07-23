;;;; contact.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :bartleby)

;;; Phone Class

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

(defmethod sql-print ((phone-number phone-number))
  (concatenate 'string
	       (area phone-number)
	       (middle phone-number)
	       (end phone-number)))

(defun make-phone-number (number-string)
  "Stores a string phone number as an object including country and area codes"
  (let ((l (length number-string)))
    (make-instance 'phone-number :country (if (> l 10)
				            (subseq number-string 0 (- l 10))
				            1)
		                 :area (subseq number-string (- l 10) (- l 7))
				 :middle (subseq number-string (- l 7) (- l 4))
				 :end (subseq number-string (- l 4) l))))

;;; Email Addresses

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

(defmethod sql-print ((email-address email-address))
  (concatenate 'string
	       (username email-address)
	       "@"
	       (domain email-address)))

;;; Address Class

;(defclass address ()
 ; ((street-number :initarg :street-number
;		  :accessor street-number)
 ;  (street-name   :initarg :street-name
;		  :accessor street-name)
 ;  (city          :initarg :city
;		  :accessor city)
 ;  (state         :initarg :state
;		  :accessor state)
 ;  (zip-code      :initarg :zip-code
;		  :accessor zip-code)))
;
;(;;defmethod print-object ((obj address) stream)
 ; (print-unreadable-object (obj stream :type t)
  ;  (with-accessors ((street-number street-number)
;		     (street-name   street-name)
;		     (city          city)
;		     (state         state)
;		     (zip-code      zip-code))
;	obj
 ;     (format stream "~a ~a~%~a, ~a ~a" street-number street-name city state zip-;code))))

;(defun make-address (street-number street-name city state zip-code)
 ;; (make-instance 'address :street-number street-number
;		          :street-name   street-name
;			  :city          city
;			  :state         state
;			  :zip-code      zip-code))    ; this will only be effective with a proper string-reading function

