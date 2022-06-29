;;;; backup.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :bartleby)

;;; Functions for backing up

(defvar *use-mito* nil) ;options for (if *use-mito* (mito-process...))

(defun connect-mysql (database-name username password)
  (mito:connect-toplevel :mysql :database-name database-name :username username :password password))

(defgeneric backup-unit (object)
  (:documentation "Prepares an object for backup."))
					;date, time, client, employee, room, appointment ...

(defun make-backup (backup-name object-list)
  "Writes a backup of a given object-list to the specified filename."
  (with-open-file (out (asdf:system-relative-pathname "bartleby"
						      (concatenate 'string
								   "backup-of-" backup-name ".lisp"))
		       :direction         :output
		       :if-does-not-exist :create
		       :if-exists         :supersede)
    (format out ";;;;~a~%(in-package :bartleby)~%" backup-name)
    (loop :for o :in object-list
	  :do (format out "~a~%" (backup-unit o)))))

(defun blank-backup (backup-name)
  "Generates a blank backup to make sure it's there for the .asd"
  (with-open-file (out (asdf:system-relative-pathname "bartleby"
						      (concatenate 'string
								   "backup-of-" backup-name ".lisp"))
		       :direction :output
		       :if-does-not-exist :create
		       :if-exists nil)
    (format out ";;;;~a~%(in-package :bartleby)~%" 
	    (concatenate 'string "backup-of-" backup-name ".lisp"))))

(defun and-then-there-were-backups (backup-item-names)
  (loop :for b :in backup-item-names
	:do (blank-backup b)))

(and-then-there-were-backups '("clients" "employees" "rooms" "appointments"))

(defgeneric load-saved-item (object)
  (:documentation "Adding item to its list without updating backup"))

;;;;Eventually SQL- maybe use MITO

