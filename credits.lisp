;;;;credits.lisp

(in-package :bartleby)

(mito:deftable credit ()
  ((date :col-type (:date))))

;(defclass credit ()
 ; ((date-added       :initarg :date-added
;	             :accessor date-added)
 ;  (expiration-date  :initarg :expiration-date
;		     :accessor expiration-date)
 ;  (client           :initarg :client
;		     :accessor client)
 ;  (orig-appointment :initarg :orig-appointment
;		     :accessor orig-appointment)
 ;  (minutes          :initarg :minutes
;		     :accessor minutes)))

(defmethod print-object ((obj credit) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((date-added       date-added)
		     (expiration-date  expiration-date)
		     (client           client)
		     (orig-appointment orig-appointment)
		     (minutes          minutes))
	obj
      (format stream
	      "~a~%~a~%~a~%~a~%~a~%"
	      date-added
	      expiration-date
	      client
	      orig-appointment
	      minutes))))

(defun make-credit (date-added client orig-appointment minutes &optional expiration-days)
  (make-instance 'credit :date-added       date-added
		         :expiration-date  (if expiration-days
					       (add-days date-added expiration-days) (add-days *standard-expiration-days*)) 
			 :client           client
			 :orig-appointment orig-appointment
			 :minutes          minutes))


;(defmethod backup-unit ((credit credit))
 ; (format nil "(make-credit ~a ~a (client-id-search ~a) (appointment-id-search ~a) ~a ~a)"
;	  (backup-unit (date-added credit))
;	  (backup-unit (expiration-date credit))
;	  (id (client credit))
;	  (id (orig-appointment credit))
;	  (minutes credit)))
  
;(defmethod total-credit-minutes ((client client))
 ; (loop :for c :in (credits client)
;	:sum (minutes c)))

(defvar *standard-expiration-days* 180)

;(defmethod add-credit ((client client) date-added minutes &optional; expiration-days)
  ;(make-credit date-added
;	       client
 ;              nil
;	       minutes
;	       (if expiration-days
;		   expiration-days
;		   nil)))

;(defmethod credit-appointment ((appointment appointment))
 ; (make-credit (date-o (dt appointment))
;	       (client appointment)
;	       appointment
;	       (duration appointment)
;	       nil))


		 
			  
(defvar *all-credits* nil)
			  
