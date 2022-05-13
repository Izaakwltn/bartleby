;;;;services.lisp

(in-package :bartleby)

;;;services to be attached to either an employee or a room

(defclass service ()
  ((name        :initarg    :name
		:accessor name)
   (description :initarg :description
		:accessor description)
   (client-rate :initarg :client-rate
		:accessor client-rate)
   (hourly-rate :initarg :hourly-rate
		:accessor hourly-rate)))

(defclass product ()
  ((name :initarg :name
	 :accessor name)
   (description :initarg :description
		:Accessor description)
   (price       :initarg :price
		:accessor price)))

(defclass transaction () ;;;to be used for either services or products
  ((id :initarg :id
       :accessor id)
   (dt :initarg :dt
       :accessor dt) ;;date and time purchased
   (item :initarg :item
	 :accessor item)))

(defgeneric check-out (object)
  (:documentation "Checks out an appointment, service, or product"))

;;;checked out items become transactions, then become receipts when the sale is 
;;;;products: gift cards/prepaid subscriptions (4-pack, 8-pack, etc)
   
