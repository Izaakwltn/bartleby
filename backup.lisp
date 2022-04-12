;;;;backup.lisp

(in-package :schedulizer)

(defgeneric backup-unit (object)
  (:documentation "Prepares an object for backup."))

;make backup-unit for appointments, clients, employees, rooms, maybe more categories

(defun make-backup (filename object-list)
  "Writes a backup of a given object-list to the specified filename."
  (with-open-file (out (asdf:system-relative-pathname "schedulizer" filename)
		       :direction         :output
		       :if-does-not-exist :create
		       :if-exists         :supersede)
    (format out ";;;;~a~%(in-package :schedulizer)~%" filename)
    (loop :for o :in object-list
	  :do (format out "~a~%" (backup-unit o)))))
    ;(format out "))"))) ;;;;;;;so backup-unit will work for any object

;(defun load-backup (backup-list)
 ; "Iterates through backup list, runs each command")

;;;;make backup then make functions for loading the backup when the program is loaded
