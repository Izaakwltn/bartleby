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
;add notes

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

(defmethod change-first-name ((employee employee) first-name)
  (replace-employee employee (make-employee first-name
					    (last-name employee)
					    (employee-id employee)
					    (phone employee)
					    (email employee)
					    (address employee)
					    (hourly-rate employee))))

(defmethod change-last-name ((employee employee) last-name)
  (replace-employee employee (make-employee (first-name employee)
					    last-name
					    (employee-id employee)
					    (phone employee)
					    (email employee)
					    (address employee)
					    (hourly-rate employee))))

(defmethod change-id ((employee employee) employee-id)
  (replace-employee employee (make-employee (first-name employee)
					    (last-name employee)
					    employee-id
					    (phone employee)
					    (email employee)
					    (address employee)
					    (hourly-rate employee))))

(defmethod change-phone ((employee employee) phone)
  (replace-employee employee (make-employee (first-name employee)
					    (last-name employee)
					    (employee-id employee)
					    phone
					    (email employee)
					    (address employee)
					    (hourly-rate employee))))

(defmethod change-email ((employee employee) email)
  (replace-employee employee (make-employee (first-name employee)
					    (last-name employee)
					    (employee-id employee)
					    (phone employee)
					    email
					    (address employee)
					    (hourly-rate employee))))

(defmethod change-address ((employee employee) address)
  (replace-employee employee (make-employee (first-name employee)
					    (last-name employee)
					    (employee-id employee)
					    (phone employee)
					    (email employee)
					    address
					    (hourly-rate employee))))

(defmethod change-hourly ((employee employee) hourly-rate)
  (replace-employee employee (make-employee (first-name employee)
					    (last-name employee)
					    (employee-id employee)
					    (phone employee)
					    (email employee)
					    (address employee)
					    hourly-rate)))
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
  last-employee-id)

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

