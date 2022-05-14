;;;;services.lisp

(in-package :bartleby)

;;;services to be attached to either an employee or a room

(defclass service ()
  ((name        :initarg    :name
		:accessor name)
  ; (description :initarg :description
;		:accessor description)
   (client-rate :initarg :client-rate
		:accessor client-rate)))

(defmethod print-object ((obj service) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((name name)
;		     (description description)
		     (client-rate client-rate))
	obj
      (format stream "~a~%~a~%Rate: ~a" name client-rate))))

(defun make-service (name client-rate)
  (make-instance 'service :name name
			  :client-rate client-rate))

(make-service "violin lesson" 60)

(defmethod total-price (appointment)
  ; (hourly-rate :initarg :hourly-rate
;		:accessor hourly-rate)))

;;;add service attribute to appointment, services attribute to rooms and employees

(defclass product ()
  ((name  :initarg :name
	  :accessor name)
   (price :initarg :price
	  :accessor price)))

  ;;;;;transactions are not receipts, they're like pre-receipts. Most likely will export as invoices
  ;;;;to be paid in a different form, maybe separate python-stripe web system
(defclass transaction () ;;;to be used for either services or products
  ((id          :initarg :id
                :accessor id)
   (dt          :initarg :dt
                :accessor dt) ;;date and time purchased
   (item        :initarg :item
	        :accessor item)
   (price :initarg :price
		:accessor price)))

  
(defmethod print-object ((obj transaction) stream)
  (with-unreadable-object (obj stream :type t)
    (with-accessors ((id id)
		     (dt dt)
		     (item item))
	(format stream "~a~%~a~%~a~%" id dt item))))

(defun make-transaction (id dt item)
  (make-instance 'transaction :id id
		              :dt dt
			      :item item))

(defun new-transaction-id ()
  1234321)
  
(defgeneric check-out (object)
  (:documentation "Checks out an appointment, service, or product"))

(defmethod check-out (appointment)
  (make-transaction (new-transaction-id)
		    (dt appointment)
		    appointment))

(defmethod check-out (product)
  (make-transaction (new-transaction-id)
		    (current-date-time)
		    product))
;;;checked out items become transactions, then become receipts when the sale is 
;;;;products: gift cards/prepaid subscriptions (4-pack, 8-pack, etc)




  
;;;mito deftable for clients, employees, rooms, products, services, appointments, and transactions
  
