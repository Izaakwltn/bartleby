;;;;invoices.lisp
;;;;

(in-package :bartleby)

;;; Invoice class

                                        ; maybe do html generation for now
;;; new approach- more filled out invoice class, defmethods for different objects
   ;;; to make invoice, just give an object and a month and it will complete the rest
;;; attributes:
   ;;; object, object contact (a block of address, phone number, and email
   ;;; month
   ;;; list of appointments (printed differently based on object)
   ;;; 

(mito:deftable invoice ()
  ((obj-type :col-type (:varchar 32))
   (obj-id   :col-type (:varchar 32))
   (month    :col-type (:int))
   (filename :col-type (:varchar 32)))
  (:conc-name "invoice-"))

(mito:ensure-table-exists 'invoice)

(defvar invoice-object-types '(client employee room))

(defun find-invoice-object (obj-type obj-id)
  "Finds the object at the center of the invoice"
  (cond ((string-equal obj-type "client")
         (client-id-search obj-id))
        ((string-equal obj-type "employee")
         (employee-id-search obj-id))
        ((string-equal obj-type "meeting-room")
         (room-id-search obj-id))))

(defmethod print-object ((obj invoice) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((obj-type invoice-obj-type)
                     (obj-id   invoice-obj-id)
                     (month    invoice-month)
                     (filename invoice-filename))
	obj
      (format stream
	      "~a-~a~%~a~%~a~%"
	      obj-type obj-id month filename (month-receipts (find-invoice-object obj-type obj-id))))))

(defgeneric make-invoice (object month)
  (:documentation "Puts together information for an invoice"))

(defmethod make-invoice ((client client) month)
  (make-instance 'invoice :obj-type "client"
                          :obj-id (client-id client)
                          :month month
                          :filename (default-invoice-filename)))
    
(defun default-invoice-filename (invoice)
  "Returns a default filename for a given invoice."
  (concatenate 'string "invoice"
               (invoice-obj-type invoice)
               (invoice-obj-id invoice)
               (month-name (invoice-month invoice))))
               

(defvar *invoices* nil) ;populate with invoice
;;;Invoice calculations
;;;;------------------------------------------------------------------------

(defmethod invoice-total ((invoice invoice))
  "Returns the total money earned for an invoice."
  (loop :for r :in (receipts invoice)
	:sum (* (/ (appointment-duration r) 60)
		(employee-hourly-rate (employee-id-search (appointment-employee-id r))))))

(defgeneric month-receipts (object month year)
  (:documentation "Returns all receipts for an object in the month"))

(defmethod month-receipts ((employee employee) month year)
  "Returns all apppointments for an employee in a given month."
  (loop :for r :in (all-receipts)
	:if (and (equal (appointment-employee-id r) (employee-id employee))
		 (equal month (m (date-o (dt (appointment r)))))
		 (equal year (y (date-o (dt (appointment r))))))
	  :collect r :into rcpts
	:finally (return rcpts)))

(defmethod month-receipts ((client client) month year)
  "Returns all appointments for a client in a given month"
  (loop :for r :in *receipts*
	:if (and (find-if #'(lambda (c)
			      (equal (id c) (id client)))
			  (clients (appointment r)))
		 (equal month (m (date-o (dt (appointment r)))))
		 (equal year (y (date-o (dt (appointment r))))))
	  :collect r :into rcpts
	:finally (return rcpts)))                            ;;;;;;later add room method

(defun chronological-receipts (receipts)
  "Sorts a list of receipts by oldest to newest."
  (sort (copy-list receipts)
	#'(lambda (receipt1 receipt2)
	    (not (later-date (app-date (appointment receipt1))
			(app-date (appointment receipt2)))))))

;;;;------------------------------------------------------------------------
;;;;Figure out Makeup Table
;;;;------------------------------------------------------------------------

;;;;------------------------------------------------------------------------
;;;;Printing Invoices:
;;;;------------------------------------------------------------------------

(defun printable-invoice-clients (client-list)
  (cond ((null client-list) "")
	(t (concatenate 'string
			(printable-invoice-clients (rest client-list))
			(concatenate 'string
				     (first-name (first client-list))
				     " "
				     (last-name (first client-list))
				     ", ")))))

(defun printable-invoice-employees (employee-list)
  (cond ((null employee-list) "")
	(t (concatenate 'string
			(printable-invoice-employees (rest employee-list))
			(concatenate 'string
				     (first-name (first employee-list))
				     " "
				     (last-name (first employee-list))
				     ", ")))))

(defmethod print-invoice ((invoice invoice) filename)
  (with-open-file (out (asdf:system-relative-pathname "bartleby" filename)
		       :direction         :output
		       :if-does-not-exist :create
		       :if-exists         :overwrite)
    (format out "~%~a~%~%~a~%~%"
	    (title invoice)
	    ;(first-name (subject invoice))
	    ;(last-name (subject invoice)))
	    ;(address (owner invoice)))
     (loop :for r :in (receipts invoice)
	   :do (let ((d  (date-o (timestamp-from-sql (write-to-string (appointment-timestamp r)))))
		     (st (time-o (timestamp-from-sql (write-to-string (appointment-timestamp r)))))
		     (c (client-id-search (appointment-client-id r)))
		     (e (employee-id-search (appointment-employee-id r))))
		 (format out
			 "~a, ~a ~a~a, ~a-~a:~a ~a~%Client: ~a| Employee: ~a~%~a ~amin|~%~%"
			 (second (assoc (day-of-week d) days-of-week))
		         (second (assoc (m d) month-names))
		         (if (equal (length (write-to-string (d d))) 1)
			     (concatenate 'string " " (write-to-string (d d)))
			     (d d))
		         (number-suffix (d d))
		         (y d)
			 (if (equal 1 (length (write-to-string (hour st))))
			     (concatenate 'string " "
					  (write-to-string
					   (if (> (hour st) 12)
					       (- (hour st) 12)
			                       (hour st))))
			     (if (> (hour st) 12)
			         (- (hour st) 12)
			         (hour st)))
			 (if (equal 1 (length (write-to-string (minutes st))))
			     (concatenate 'string "0" (write-to-string (minutes st)))
			     (minutes st))
			 (if (> (hour st) 12) "pm" "am")
			 (concatenate 'string
				      (client-first-name c)
				      " "
				      (client-last-name c))
			 (concatenate 'string
			              (employee-first-name e)
				      " "
				      (employee-last-name e))
			 (write-to-string (receipt-attendance r))
			 (write-to-string (appointment-duration r))))))))
			 
(defgeneric invoice-it (object month year &optional filename title)
  (:documentation "Prints an invoice for an object"))

(defmethod invoice-it ((client client) month year &optional filename title)
  (print-invoice (if filename
		     filename
		     (concatenate 'string
				  (first-name client)
				  "-"
				  (last-name client)
				  "-"
				  (write-to-string month)
				  "-"
				  (write-to-string year)
				  ".txt"))
		 (draft-invoice (if title
				    title
				    (concatenate 'string
						 (second (assoc month month-names))
						 " Appointments"))
				 (id client)
				 (month-receipts client month year))))

(defmethod invoice-it ((employee employee) month year &optional filename title)
  (print-invoice (if filename
		     filename
		     (concatenate 'string
				  (first-name employee)
				  "-"
				  (last-name employee)
				  "-"
				  (write-to-string month)
				  "-"
				  (write-to-string year)
				  ".txt"))
		 (draft-invoice (if title
				    title
				    (concatenate 'string
						 (second (assoc month month-names))
						 " Appointments"))
				(id employee)
				(month-receipts employee month year))))

;;;;------------------------------------------------------------------------
;;;;
;;;;------------------------------------------------------------------------
