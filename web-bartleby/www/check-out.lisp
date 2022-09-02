;;;; web-bartleby/www/check-out.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :web-bartleby)

;;;;;;;;scratch work for checkout-handler
;(defmacro ddt (func-name) `(defun ,(intern func-name) () (+ 2 4)))

;(ddt "turtle")

;(defun ddt-test ()
 ; (let ((s (concatenate 'string "S")))
  ;  (ddt s)))

;(defun make-function (func-name) (defvar (intern func-name) ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(eval-when (:compile-toplevel :load-toplevel :execute)
 ; (defun handler-name (handler-string)
  ;  (intern handler-string)))

;(defmacro checkout-handler (lambda-list)
 ; (destructuring-bind (name &key uri
  ;                                      ;(let ((handler form-id))
 ; (destructuring-bind
  ;  `(hunchentoot:define-easy-handler (,(intern form-id) :uri ,form-uri) (appointment-id attendance makeup notes)
   ;  (bartleby::check-out
    ;  (bartleby::appointment-id-search (parse-integer appointment-id))
     ; attendance
  ;    (parse-integer makeup)
   ;   notes)
    ; (appointments-check-out))))))

;(mito:deftable checkable-appointment (bartleby::appointment)
 ; ((form-id :col-type (:varchar 32)) ;type symbol
 ;  (form-uri :col-type (:varchar 32)))
 ; (:conc-name "checkable-"))

;(mito:ensure-table-exists 'checkable-appointment)

;(defmethod make-checkable-appointment ((appointment bartleby:appointment))
 ; (make-instance 'checkable-appointment
  ;               :client-id   (bartleby::appointment-client-id appointment)
   ;              :employee-id (bartleby::appointment-employee-id appointment)
    ;             :room-id     (bartleby::appointment-room-id appointment)
     ;            :timestamp   (bartleby::appointment-timestamp appointment)
      ;           :duration    (bartleby::appointment-duration appointment)
       ;          :notes       (bartleby::appointment-notes appointment)
        ;         :form-id     (make-symbol
         ;                      (concatenate 'string
          ;                                  "appointment-checkout-"
           ;                                 (write-to-string
            ;                                 (mito:object-id appointment))))
             ;    :form-uri    (concatenate 'string
              ;                             "/appointment-checkout-"
               ;                            (write-to-string
                ;                            (mito:object-id appointment)))))

;(defmethod add-checkable-appointment ((checkable-appointment checkable-appointment))
 ;(mito:insert-dao checkable-appointment))

;(defmethod remove-checkable-appointment ((checkable-appointment checkable-appointment))
 ; (mito:delete-dao checkable-appointment))

;(defmethod replace-checkable-appointment ((checkable-appointment checkable-appointment) new-checkable-appointment)
 ; (remove-checkable-appointment checkable-appointment)
  ;(add-checkable-appointment checkable-appointment))

(defmethod checkout-item ((appointment bartleby::appointment))
  (let ((form-id (concatenate 'string
                              "appointment-checkout-"
                              (write-to-string
                               (mito:object-id appointment)))))
;                 (form-uri (checkable-form-uri ca)))
;             (checkout-handler form-id form-uri))
;           (let ((form-id (write-to-string (checkable-form-id ca)))
 ;                (form-uri (checkable-form-uri ca)))
           (spinneret:with-html
             (:form :action "/appointment-check-out" :id form-id
                    ;(:select :form form-id :name form-id
                     ; (:option :value appointment))
                    (:tr (:td (:select :form form-id :name "appointment-id"
				(:option :value (mito:object-id appointment) (format nil "~a ~a"
				      (bartleby::client-first-name
				       (mito:find-dao 'bartleby::client
						      :id (bartleby::appointment-client-id
                                                           appointment)))
				      (bartleby::client-last-name
				       (mito:find-dao 'bartleby::client
						      :id (bartleby::appointment-client-id
                                                           appointment)))))))
			 (:td (format nil "~a ~a"
		                      (bartleby::employee-first-name
		                       (mito:find-dao 'bartleby::employee
						      :id (bartleby::appointment-employee-id appointment)))
				      (bartleby::employee-last-name
				       (mito:find-dao 'bartleby::employee
						      :id (bartleby::appointment-employee-id appointment)))))
			 (:td (let ((ts (bartleby::timestamp-from-sql
                                         (write-to-string
                                          (bartleby::appointment-timestamp appointment)))))
				(let ((dos (bartleby::pretty-print (bartleby::date-o ts)))
				      (tos (bartleby::pretty-print (bartleby::time-o ts))))
				  (format nil "~a at ~a" dos tos))))
			 (:td (:select :name "attendance" :form form-id
				(loop :for i :in bartleby::attendance-values
			              :do (:option :value i (format nil "~a" i)))))
                         (:td (:select :name "makeups" :form form-id
                                (loop :for i :from -60 :to +60 :by 15
                                      :if (zerop i)
                                        :do (:option :selected 0 :value 0)
                                     :else
                                      :do (:option :value i
                                                  (format nil "~a" i)))))
			 (:td (:input :type "text" :id "notes" :name "notes" :form form-id))
			 (:td (:button :type "submit" :class "btn btn-default" "Check Out")))))))

(hunchentoot:define-easy-handler (appointments-check-out :uri "/appointments-check-out") ()
  (with-page (:title "Check out Appointments")
    (:h1 "Check out Appointments")
	   (cl-bootstrap:bs-table
	    (:thead
	     (:th "Client")
	     (:th "Employee")
	     (:th "Date/Time")
	     (:th "Attendance")
             (:th "Makeup+/-")
	     (:th "Notes")
	     (:th "")
             (:tbody
              (spinneret:with-html
                (loop :for a :in (sort (bartleby::all-past-appointments)
                                       #'(lambda (x y)
                                           (bartleby:later-timestamp-p
                                            (bartleby::timestamp-from-sql
                                             (write-to-string
                                              (bartleby::appointment-timestamp y)))
                                            (bartleby::timestamp-from-sql
                                             (write-to-string
                                              (bartleby::appointment-timestamp x))))))
                           :do (checkout-item a))))))))

(hunchentoot:define-easy-handler (appointment-check-out :uri "/appointment-check-out") (appointment-id attendance makeups notes)
  ;(write (list appointment-id attendance makeups notes)))
  (bartleby::check-out (bartleby::appointment-id-search (parse-integer appointment-id)) attendance (parse-integer makeups) notes)
  (appointments-check-out))
     ;(:hr)
;     (spinneret:with-html (:select :name "employee" :form "new-appointment"
;	     (loop :for e :in (bartleby:all-employees)
;		   :do (:option :value (mito:object-id e)
;				(format nil "~a ~a" (bartleby::employee-first-name e)
;						    (bartleby::employee-last-name e))))))
 ;    (:hr)
  ;   (spinneret:with-html (:select :name "room" :form "new-appointment"
;	     (loop :for r :in (bartleby:all-rooms)
;		   :do (:option :value (mito:object-id r)
;				(format nil "~a ~a" (bartleby::room-num r)
;					(bartleby::room-name r))))))
 ;    (:hr)
  ;   (spinneret:with-html
   ;    (:select :name "date" :form "new-appointment"
;	 (loop :for i :from 0 :to 30
;	       :do (:option :value (bartleby:add-days
;				    (bartleby::date-o (bartleby::current-timestamp)) i)
;			    (format nil "~a"
;				    (bartleby:add-days (bartleby::date-o (bartleby::current-timestamp)) i))))))
 ;    (spinneret:with-html
  ;     (:select :name "time" :form "new-appointment"
;	 (loop :for i :in time-options
;	       :do (:option :value i
;			    (format nil "~a" i)))))
 ;    (:hr)
  ;   (:hr)
   ;  (:label :for "notes"
;	     "Notes ")
 ;    (:input :type "text" :id "notes" :name "notes")
;	          (cl-bootstrap:bs-form-checkbox "Check me out")
;	     (:button :type "submit" :class "btn btn-default" "Submit"))))
