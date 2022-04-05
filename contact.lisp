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
  (make-intance 'address :street-number street-number
		         :street-name   street-name
			 :city          city
			 :state         state
			 :zip-code      zip-code))

(defun random-address ()
  "2022 Johnson scuba, Denver, Kansas 90000")

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

;(defun make-phone-number (number-string)
 ; (let ((l (length number-string)))
  ;  (make-instance 'phone-number :country (if (> l 10)
;				            (subseq number-string 0 (- l 10))
;				            1)
;		                 :area (if (> l 10)
;;					   (subseq number-string (- l 10) (- l 8))
;					   (subseq number-string 0 2))
;;				 :middle (if (> l 10)
;					     (subseq number-string (- l 8) (- l 5))
;					     (subseq number-string 2 4))
;				 :
		 
;;;;------------------------------------------------------------------------
;;;;Random entries for simulated contact information
;;;;------------------------------------------------------------------------

(defvar email-domains '("gmail.com" "yahoo.com" "hotmail.com" "aol.com" "msn.com"))

(defun random-email (first-name last-name)
  (concatenate 'string last-name "." first-name "@" (nth (random 4) email-domains)))
  
(defun random-phone ()
  (loop :with number := nil
        :for i :from 1 :to 10
	:do (if (or (equal i 4)
	            (equal i 7))
		(setf number
		      (concatenate 'string number "-" (write-to-string (random 9))))
		(setf number
	              (concatenate 'string number (write-to-string (random 9)))))
	:finally (return number)))

