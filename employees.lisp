;;;;employees.lisp
;;;;

(in-package :schedulizer)


;;;;------------------------------------------------------------------------
;;;;Employee class
;;;;------------------------------------------------------------------------

(defclass employee ()
  ((employee-id :initarg :employee-id
		:accessor employee-id)
   (first-name  :initarg :first-name
		:accessor first-name)
   (last-name   :initarg :last-name
		:accessor last-name)
   (phone       :initarg :phone
		:accessor phone)
   (email       :initarg :email
		:accessor email)
   (address     :initarg :address
		:accessor address)
   (hourly-rate :initarg :hourly-rate
		:accessor hourly-rate)))
					;maybe assigned room, can be nil

(defmethod print-object ((obj employee) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((employee-id employee-id)
		     (first-name first-name)
		     (last-name last-name)
		     (phone phone)
		     (email email)
		     (address   address)
		     (hourly-rate hourly-rate))
	obj
      (format stream "~%Employee-ID: ~a~%Name: ~a ~a~%~a~%~a~%~a~%Rate: $~a/hr~%"
	      employee-id first-name last-name phone email address hourly-rate))))

(defun make-employee (employee-id first-name last-name phone email address hourly-rate)
  (make-instance 'employee :employee-id employee-id
		           :first-name  first-name
         		   :last-name   last-name
			   :phone       phone
			   :email       email
			   :address     address
			   :hourly-rate hourly-rate))


;add equal-employees

;;;;------------------------------------------------------------------------
;;;;Adding to, removing from, and editing *clients*
;;;;------------------------------------------------------------------------

(defvar *employees* nil)

(defvar *employee-backup* nil)

(defmethod add-employee ((employee employee))
  "Adds an employee to *employees*"
  (push employee *employees*))

(defmethod remove-employee ((employee employee))
  "Removes an employee from *employees*"
  (remove-if #'(lambda (e)
		 (equal (employee-id employee) (employee-id e)))
	     *employees*))

(defmethod replace-employee ((employee employee) new-employee)
  "Replaces an existing employee with a new-employee in its place."
  (remove-employee employee)
  (add-employee new-employee))

;;editing one attribute at a time
;change first-name last-name phone email address hourly....
;;;;------------------------------------------------------------------------
;;;;Adding New Employees
;;;;------------------------------------------------------------------------

(defvar izaak
  (make-employee 2001 "Izaak" "Walton" (random-phone) (random-email "Izaak" "Walton")
		 "Insert Address Here" 37))

(add-employee izaak)

(defvar last-employee-id (if (employee-id (first *employees*))
			     (+ (employee-id (first *employees*)) 1)
			     2001))

(defun new-employee-id ()
  "Generates a new employee id, updates last-employee-id."
  (setq last-employee-id (+ last-employee-id 1))
  last-client-id)

(defun new-employee (first-name last-name phone email address hourly-rate)
  "Generates a a new employee with a new employee id."
  (add-employee (make-employee (new-employee-id) first-name last-name phone email address hourly-rate)))

;;;;------------------------------------------------------------------------
;;;;Searching for employees:
;;;;------------------------------------------------------------------------

(defun employee-search (employee-id)
  "Searches for an employee by employee-id."
  (loop for employee in *employees*
	if (equal (write-to-string employee-id)
		  (write-to-string (employee-id employee)))
	  do (return employee)))

