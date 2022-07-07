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
   (notes       :col-type (or (:varchar 128) :null))))

(mito:ensure-table-exists 'employee)

(defmethod print-object ((obj employee) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((first-name first-name)
		     (last-name last-name)
		     (phone phone)
		     (email email)
		     (address address)
		     (hourly-rate hourly-rate)
		     (notes notes))
	obj
      (format stream "~%Employee-ID: ~a~%Name: ~a ~a~%~a~%~a~%~a~%Rate: $~a/hr~%"
	      first-name last-name phone email address hourly-rate notes))))

(defun make-employee (first-name last-name phone email hourly-rate notes)
  (make-instance 'employee :first-name  first-name
         		   :last-name   last-name
			   :phone       phone
			   :email       email
			   :address     address
			   :hourly-rate hourly-rate
			   :notes       notes))

;;; Adding and removing employees

(defmethod add-employee ((employee employee))
  (mito:insert-dao employee))

;(defmethod new-employee (first-name last-name phone email address hourly-rate notes)
 ; (mito:create-dao (make-employee first-name last-name phone email address hourly-rate notes)))

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
  (replace-employee employee (make-employee (id employee)
					    (first-name employee)
					    last-name
					    (phone employee)
					    (email employee)
					    (hourly-rate employee)
					    (notes       employee))))

(defmethod change-id ((employee employee) id)
  (replace-employee employee (make-employee id
					    (first-name employee)
					    (last-name employee)
					    (phone employee)
					    (email employee)
					    (hourly-rate employee)
					    (notes       employee))))

(defmethod change-phone ((employee employee) phone)
  (replace-employee employee (make-employee (id employee)
					    (first-name employee)
					    (last-name employee)
					    phone
					    (email employee)
					    (hourly-rate employee)
					    (notes employee))))

(defmethod change-email ((employee employee) email)
  (replace-employee employee (make-employee (id employee)
					    (first-name employee)
					    (last-name employee)
					    (phone employee)
					    email
					    (hourly-rate employee)
					    (notes       employee))))

;(defmethod change-address ((employee employee) address)
 ; (replace-employee employee (make-employee (first-name employee)
;					    (last-name employee)
;					    (id employee)
;					    (phone employee)
;					    (email employee)
;					    address
;					    (hourly-rate employee))))

(defmethod change-hourly ((employee employee) hourly-rate)
  (replace-employee employee (make-employee (id employee)
					    (first-name employee)
					    (last-name employee)
					    (phone employee)
					    (email employee)
					    hourly-rate
					    (notes employee))))

(defmethod change-notes ((employee employee) notes)
  (replace-employee employee (make-employee (id employee)
					    (first-name employee)
					    (last-name employee)
					    (phone employee)
					    (email employee)
					    (hourly-rate employee)
					    notes)))

;;;;------------------------------------------------------------------------
;;;;Adding New Employees
;;;;------------------------------------------------------------------------

(defvar izaak
  (make-employee 2001 "Izaak" "Walton"
		 (make-phone-number "4043872185")
		 (make-email "izaakviolin@gmail.com")
		 37
		 "A Teacher"))

;(add-employee izaak)

(defvar last-employee-id (if (first *employees*)
			     (+ (id (first *employees*)) 1)
			     2000))

(defun new-employee-id ()
  "Generates a new employee id, updates last-employee-id."
  (setq last-employee-id (+ last-employee-id 1))
  last-employee-id)

(defun new-employee (first-name last-name string-phone string-email hourly-rate notes)
  "Generates a a new employee with a new employee id."
  (add-employee (make-employee (new-employee-id) first-name last-name (make-phone-number string-phone) (make-email string-email) hourly-rate notes)))

;;;;------------------------------------------------------------------------
;;;;Searching for employees:
;;;;------------------------------------------------------------------------

(defun employee-id-search (employee-id)
  "Searches for an employee by employee-id."
  (loop for employee in *employees*
	if (equal (write-to-string employee-id)
		  (write-to-string (id employee)))
	  do (return employee)))

