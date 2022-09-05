;;;;services.lisp

(in-package :bartleby)

;;;services to be attached to either an employee or a room

(mito:deftable service ()
  ((name :col-type (:varchar 32))) ; maybe eventually add hourly-rate
  (:conc-name service-))

(mito:ensure-table-exists 'service)

(defmethod print-object ((obj service) stream)
  (print-unreadable-object (obj stream :type t)
    obj
    (format stream "~a" (service-name obj))))

(defun make-service (name)
  (make-instance 'service :name name))

(defmethod add-service ((service service))
  (mito:insert-dao service))

(defmethod remove-service ((service service))
  (mito:delete-dao service))

(defmethod replace-service ((service service) new-service)
  (remove-service service)
  (add-service new-service))

(defun new-service (name)
  (add-service (make-service name)))

(defmethod change-name ((service service) new-name)
  (setf (slot-value service 'name) new-name)
  (mito:save-dao service))

(defun service-count ()
  (mito:count-dao 'service))

(defun service-id-search (service-id)
  (mito:find-dao 'service :id service-id))

(defun all-services ()
  (loop :with services := nil
        :with sc       := (service-count)

        :for i :upfrom 1
        :do (setq services (if (null (service-id-search i))
                               services
                               (cons (service-id-search i)
                                     services)))
        :when (equal (length services) sc)
          :do (return services)))

(defun service-name-search (name)
  (find-if #'(lambda (s)
               (string-equal name (service-name s)))
           (all-services)))

;;; Reading and writing services

(alexa:define-string-lexer service-lexer
  "A lexer for service lists"
  ((:service "[A-Za-z][A-Za-z]*")
   (:all-else " "))
  ("{{SERVICE}}" (return (princ-to-string $@)))
  ("{{ALL-ELSE}}" nil))

(defun parse-services (service-list-string)
  (loop :with lexer := (service-lexer service-list-string)
        :for tok := (funcall lexer)
        :while tok
        :collect tok))

(defun absorb-services (service-list-string)
  (loop :with service-chunk := ""
        :for s :in (parse-services service-list-string)
        :if (service-name-search s)
          :do (setq service-chunk
                    (concatenate 'string
                                 service-chunk
                                 (write-to-string (mito:object-id (service-name-search s)))
                                 ","))
        :else
          :do (progn (new-service s)
                     (setq service-chunk
                           (concatenate 'string
                                 service-chunk
                                 (write-to-string (mito:object-id (service-name-search s)))
                                 ",")))
        :finally (return service-chunk)))

(alexa:define-string-lexer service-chunk-lexer
  "A lexer for numerical service CSVs"
  ((:service "[0-9][0-9]*")
   (:all-else ","))
  ("{{SERVICE}}" (return (princ-to-string $@)))
  ("{{ALL-ELSE}}" nil))

(defun parse-service-chunk (service-chunk)
          (loop :with lexer := (service-chunk-lexer service-chunk)
                :for tok := (funcall lexer)
                :while tok
                :collect tok))

(defgeneric return-services (object)
  (:documentation "Reads a service-chunk from a room or employee object, returns list of services"))

(defmethod return-services ((employee employee))
  (mapcar #'(lambda (s)
              (service-name (service-id-search (parse-integer s))))
          (parse-service-chunk (employee-services employee))))

(defmethod return-services ((meeting-room meeting-room))
  (mapcar #'(lambda (s)
              (service-name (service-id-search (parse-integer s))))
          (parse-service-chunk (room-services meeting-room))))
;;;

(defgeneric add-services (object new-services)
  (:documentation "Adds services to an Employee or Room object"))

(defmethod add-services ((employee employee) new-services)
  (setf (slot-value employee 'services) (concatenate 'string (employee-services employee)
                                                     (absorb-services new-services)))
  (mito:save-dao employee))

(defmethod add-services ((meeting-room meeting-room) new-services)
  (setf (slot-value meeting-room 'services) (concatenate 'string (room-services meeting-room)
                                                         (absorb-services new-services)))
  (mito:save-dao meeting-room))

(defun service-list (service-chunk)
  (loop :for id :in (parse-service-chunk service-chunk)
        :collect (service-name (service-id-search id))))

(defun employee-service-search (service-name)
  (remove-if-not #'(lambda (e)
               (find-if #'(lambda (s)
                            (string-equal s service-name))
                        (service-list (employee-services e))))
           (all-employees)))

(defun room-service-search (service-name)
  (remove-if-not #'(lambda (r)
               (find-if #'(lambda (s)
                            (string-equal s service-name))
                        (service-list (room-services r))))
           (all-rooms)))


;;;add service attribute to appointment, services attribute to rooms and employees
;;; separate
;(defclass product ()
 ; ((name  :initarg :name
;	  :accessor name)
 ;;  (price :initarg :price
;	  :accessor price))) ; maybe add number of items in stock

  ;;;;;transactions are not receipts, they're like pre-receipts. Most likely will export as invoices
  ;;;;to be paid in a different form, maybe separate python-stripe web system
;;(defclass transaction () ;;;to be used for either services or products
;  ((id          :initarg :id
 ;               :accessor id)
  ; (dt          :initarg :dt
   ;             :accessor dt) ;;date and time purchased
 ;  (;item        :initarg :item
;	        :accessor item)
 ;  (price :initarg :price
;		:accessor price)))

  
;(defmethod print-object ((obj transaction) stream)
 ; (with-unreadable-object (obj stream :type t)
  ;  (with-accessors ((id id)
;		     (dt dt)
;		     (item item))
;	(format stream "~a~%~a~%~a~%" id dt item))))

;(defun make-transaction (id dt item)
 ; (make-instance 'transaction :id id
;		              :dt dt
;			      :item item))

;'(defun new-transaction-id ()
;  1234321)
  
;(defgeneric check-out (object)
 ; (:documentation "Checks out an appointment, service, or product"))

;(defmethod check-out (appointment)
;  (make-transaction (new-transaction-id)
;		    (dt appointment)
;;		    appointment))

;(defmethod check-out (product)
 ; (make-transaction (new-transaction-id)
;		    (current-date-time)
;		    product))
  
;;; checked out items become transactions, then become receipts when the sale is 
;;; products: gift cards/prepaid subscriptions (4-pack, 8-pack, etc)




  
;;; mito deftable for clients, employees, rooms, products, services, appointments, and transactions
  
