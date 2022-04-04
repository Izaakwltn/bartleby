;;;;employees.lisp
;;;;

(in-package :schedulizer)

;;;;------------------------------------------------------------------------
;;;;Employee list
;;;;------------------------------------------------------------------------
(defvar *employees* nil)

(defun add-employee (employee)
  (push employee *employees*))

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
   (address     :initarg :address
		:accessor address)
   (hourly-rate :initarg :hourly-rate
		:accessor hourly-rate)))

(defmethod print-object ((obj employee) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((employee-id employee-id)
		     (first-name first-name)
		     (last-name last-name)
		     (address   address)
		     (hourly-rate hourly-rate))
	obj
      (format stream "~%Employee-ID: ~a~%Name: ~a ~a~%~a~%Rate: $~a/hr~%"
	      employee-id first-name last-name address hourly-rate))))

(defun make-employee (employee-id first-name last-name address hourly-rate)
  (make-instance 'employee :employee-id employee-id
		           :first-name  first-name
         		   :last-name   last-name
			   :address     address
			   :hourly-rate hourly-rate))


(defvar izaak (make-employee 2001 "Izaak" "Walton" "Insert Address Here" 37))

(add-employee izaak)

(defvar last-employee-id (employee-id (first *employees*)))

(defun new-employee-id ()
  "Generates a new employee id, updates last-employee-id."
  (setq last-employee-id (+ last-employee-id 1))
  last-client-id)

(defun new-employee (first-name last-name address hourly-rate)
  "Generates a a new employee with a new employee id."
  (make-employee (new-employee-id) first-name last-name address hourly-rate))

(defun employee-search (employee-id)
  "Searches for an employee by employee-id."
  (loop for employee in *employees*
	if (equal (write-to-string employee-id)
		  (write-to-string (employee-id employee)))
	  do (return employee)))
