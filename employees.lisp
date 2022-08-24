;;;; employees.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :bartleby)

;;; Employee class/sql object

(mito:deftable employee ()
  ((first-name  :col-type (:varchar 64))
   (last-name   :col-type (:varchar 64))
   (phone       :col-type (or (:char 10) :null))
   (email       :col-type (or (:varchar 64) :null))
   (address     :col-type (or (:varchar 100) :null))
   (hourly-rate :col-type (:int))
   (notes       :col-type (or (:varchar 128) :null)))
  (:conc-name employee-))

(mito:ensure-table-exists 'employee)

(defmethod print-object ((obj employee) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((first-name employee-first-name)
		     (last-name employee-last-name)
		     (phone employee-phone)
		     (email employee-email)
		     (address employee-address)
		     (hourly-rate employee-hourly-rate)
		     (notes employee-notes))
	obj
      (format stream "~%Name: ~a ~a~%Phone: ~a~%Email: ~a~%Address: ~a~%Rate: $~a/hr~%Notes: ~a"
	      first-name last-name phone email address hourly-rate notes))))

(defun make-employee (first-name last-name phone email address hourly-rate notes)
  (make-instance 'employee :first-name  first-name
         		   :last-name   last-name
			   :phone       phone
			   :email       email
			   :address     address
			   :hourly-rate hourly-rate
			   :notes       notes))

;;; Adding and removing employees

(defmethod add-employee ((employee employee))
  "Adds an employee to the Employtee sql table"
  (mito:insert-dao employee))

(defvar *standard-hourly* 37)

(defun new-employee (first-name last-name phone email notes &optional hourly-rate)
  "Makes a new employee with optional hourly-rate."
  (mito:create-dao (make-employee first-name
				  last-name
		                  phone
		                  email
		                  (if hourly-rate
		                      hourly-rate
		                      *standard-hourly*))))

(defmethod remove-employee ((employee employee))
  "Removes the employee from the Employee sql table"
  (mito:delete-dao employee))

(defmethod replace-employee ((employee employee) new-employee)
  "Replaces an existing employee with a new-employee in its place."
  (remove-employee employee)
  (add-employee new-employee))

;;; Editing one attribute at a time

(defmethod change-first-name ((employee employee) first-name)
  (setf (slot-value employee 'first-name) first-name)
  (mito:save-dao employee))

(defmethod change-last-name ((employee employee) last-name)
  (setf (slot-value employee 'last-name) last-name)
  (mito:save-dao employee))

(defmethod change-phone ((employee employee) phone)
  (setf (slot-value employee 'phone) phone)
  (mito:save-dao employee))

(defmethod change-email ((employee employee) email)
  (setf (slot-value employee 'email) email)
  (mito:save-dao employee))

(defmethod change-address ((employee employee) address)
  (setf (slot-value employee 'address) address)
  (mito:save-dao employee))

(defmethod change-hourly ((employee employee) hourly-rate)
  (setf (slot-value employee 'hourly-rate) hourly-rate)
  (mito:save-dao employee))

(defmethod change-notes ((employee employee) notes)
  (setf (slot-value employee 'notes) notes)
  (mito:save-dao employee))

;;; Searching/analyzing employees

(defun employee-count ()
  "Returns the total number of stored employees"
  (mito:count-dao 'employee))

(defun employee-id-search (employee-id)
  (mito:find-dao 'employee :id employee-id))

(defun all-employees ()
  (loop :with employees := nil
	:with cc      := (employee-count)

	:for i :upfrom 1
	:do (setq employees (if (null (employee-id-search i))
			      employees
			      (cons (employee-id-search i) employees)))
	:when (equal (length employees) cc)
	  :do (return employees)))

(defun employee-first-name-search (first-name)
  (find-if #'(lambda (e)
	       (string-equal first-name (employee-first-name c)))
	   (all-employees)))

(defun employee-last-name-search (last-name)
  (find-if #'(lambda (e)
	       (string-equal last-name (employee-last-name e)))
	   (all-employees)))
			      
