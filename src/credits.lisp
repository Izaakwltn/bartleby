;;;;credits.lisp

(in-package :bartleby)

(mito:deftable credit ()
  ((date-added  :col-type (:date))
   (expiration  :col-type (:date))
   (client      :col-type client)
   (appointment :col-type appointment)
   (minutes     :col-type (:int)))
   (:conc-name credit-))

(mito:ensure-table-exists 'credit)

(defmethod print-object ((obj credit) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((date-added  credit-date-added)
		     (expiration  credit-expiration)
		     (client      credit-client)
		     (appointment credit-appointment)
		     (minutes     credit-minutes))
	obj
      (format stream
	      "~a~%~a~%~a~%~a~%~a~%"
	      date-added
	      expiration
	      client-id
	      appointment
	      minutes))))

(defvar *standard-expiration-days* 180)

(defmethod make-credit ((appointment appointment) &optional expiration-days)
  (make-instance 'credit :date-added (appointment-date-time appointment)
		         :expiration (if expiration-days
                                         (add-days  expiration-days)
                                         (add-days *standard-expiration-days*)) 
			 :client-id   (appointment-client-id appointment)
			 :appointment (appointment-id appointment)
			 :minutes     (appointment-duration appointment)))

  
;(defmethod total-credit-minutes ((client client))
 ; (loop :for c :in (credits client)
;	:sum (minutes c)))

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
