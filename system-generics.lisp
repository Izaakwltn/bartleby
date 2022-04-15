;;;;system-generics.lisp
;;;;
;;;;

(in-package :schedulizer)

;;;;------------------------------------------------------------------------
;;;;Sorting
;;;;------------------------------------------------------------------------

(defun sort-by-first-name (object-list)
  (sort object-list #'(lambda (o1 o2)
			(string< (first-name o1) (first-name o2)))))

(defun sort-by-last-name (object-list)
  (sort object-list #'(lambda (o1 o2)
			(string< (last-name o1) (last-name o2)))))

(defun sort-by-id (object-list)
  (sort object-list #'(lambda (o1 o2)
			(< (id o1) (id o2)))))

;sort-by-area-code

;;;;------------------------------------------------------------------------
;;;;Changing attributes
;;;;------------------------------------------------------------------------

(defgeneric change-first-name (object first-name)
  (:documentation "Change the first name of the object"))

(defgeneric change-last-name (object last-name)
  (:documentation "Change the last name of the object"))

(defgeneric change-id (object id)
  (:documentation "Changes the id number for an object."))

(defgeneric change-credits (object credits)
  (:documentation "Changes the credits for an object."))

(defgeneric change-phone (object phone)
  (:documentation "Changes the phone number for an object"))

(defgeneric change-email (object email)
  (:documentation "Changes the email address for an object."))

(defgeneric change-address (object address)
  (:documentation "Changes the address for an object."))

(defgeneric change-notes (object notes)
  (:documentation "Changes the notes for an object"))

(defgeneric change-hourly (object hourly-rate)
  (:documentation "Changes the hourly rate for an object")) ;employees, maybe rooms and clients

(defgeneric change-capacity (object capacity)
  (:documentation "Changes the capacity for an object")) ;use with rooms and appointments

(defgeneric change-name (object name)
  (:documentation "Changes the name of an object")) ;rooms

(defgeneric change-date (object new-date)
  (:documentation "Changes the date of an object"))

(defgeneric change-time (object new-time)
  (:documentation "Changes the time of an object"))


;;;;------------------------------------------------------------------------
;;;;Backing Up
;;;;------------------------------------------------------------------------

(defgeneric backup-unit (object)
  (:documentation "Prepares an item to be printed to backup."))
