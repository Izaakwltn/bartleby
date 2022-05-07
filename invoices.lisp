;;;;invoices.lisp
;;;;

(in-package :bartleby)

;;;;------------------------------------------------------------------------

;;;two steps: appointments are stored first as unchecked
;;;then have a repl cycle which goes through each unchecked appointment,
;;;prompts for either No Show, Cancelled Makeup Added, or Arrived on each lesson
;;;maybe also prompt for makeups added or used

;;;;------------------------------------------------------------------------
;;;;Makeups
;;;;------------------------------------------------------------------------

;;;;------------------------------------------------------------------------
;;;;Invoice class
;;;;------------------------------------------------------------------------

(defclass invoice ()
  ((title        :initarg :title
	         :accessor title)
   (subject      :initarg :subject
		 :accessor subject) ;;;;client or employee, maybe room or eventually office
   (receipts     :initarg :receipts
		 :accessor receipts)))

(defmethod print-object ((obj invoice) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((title title)
		     (subject subject)
		     (receipts receipts))
	obj
      (format stream
	      "~%~a:~%~%~a~%~{~a~%~}~%"
	      title subject receipts))))

(defun draft-invoice (title owner-id receipts)
  (make-instance 'invoice :title title
		          :subject (if (employee-id-search owner-id) ;;;make its own function 
				       (employee-id-search owner-id)
				       (client-id-search owner-id))
			  :receipts receipts))

;;;;------------------------------------------------------------------------
;;;;Invoice calculations
;;;;------------------------------------------------------------------------

(defmethod invoice-total ((invoice invoice))
  "Returns the total money earned for an invoice."
  (loop :for r :in (receipts invoice)
	:sum (* (/ (duration (appointment r)) 60)
		(hourly-rate (employee (appointment r))))))


(defgeneric month-receipts (object month year)
  (:documentation "Returns all receipts for an object in the month"))

(defmethod month-receipts ((employee employee) month year)
  "Returns all apppointments for an employee in a given month."
  (loop :for r :in *receipts*
	:if (and (find-if #'(lambda (e)
			      (equal (id e) (id employee)))
			  (employees (appointment r)))
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

(defmethod print-invoice (filename (invoice invoice))
  (with-open-file (out (asdf:system-relative-pathname "bartleby" filename)
		       :direction         :output
		       :if-does-not-exist :create
		       :if-exists         :overwrite)
    (format out "~%~a~%~%~a ~a~%~%~%"
	    (title invoice)
	    (first-name (subject invoice))
	    (last-name (subject invoice)))
	    ;(address (owner invoice)))
     (loop :for r :in (receipts invoice)
	   :do (let ((d  (date-o (dt (appointment r))))
	             (st (time-o (dt (appointment r))))
	             (cl (clients (appointment r)))
		     (em (employees (appointment r))))
		 (format out
			 "~a, ~a ~a~a, ~a - ~a:~a ~a~%Client(s): ~a| Employee(s): ~a~%~a ~amin| Makeup Change: ~a~%~%"
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
			 (if (equal (length cl) 1)
			     (concatenate 'string (first-name (first cl))
					  " "
					  (last-name (first cl)))
			     (printable-invoice-clients cl))
			 (if (equal (length em) 1)
			     (concatenate 'string (first-name (first em))
					  " "
					  (last-name (first em)))
			     (printable-invoice-clients em))
			 (second (assoc (attendance r) attendance-values))
			 (write-to-string (duration r))
			 (write-to-string (makeup-change r))))))
  (format t "Printing Invoice to: ~a" filename))
			 
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
