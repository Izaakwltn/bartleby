;;;;backup.lisp

(in-package :schedulizer)

;;;;function for generating a backup given a filename, list to backup, function for formatting them
(defgeneric backup-unit (object)
  (:documentation "Prepares an object for backup."))

(defmethod backup-unit ((client client))
  (format nil "~a ~a" (write-to-string (first-name client))
	  (last-name client)))

;make backup-unit for appointments, clients, employees, rooms, maybe more categories

(defun make-backup (backup-name file-name object-list)
  (with-open-file (out (asdf:system-relative-pathname "schedulizer" filename)
		       :direction         :output
		       :if-does-not-exist :create
		       :if-exists         :overwrite)
    (format out ";;;;~a~%(in-package :schedulizer)~%~%(defvar ~a" filename backup-name)
  ;;;;;write to file code....
  (format nil "~a" filename)
  (loop :for o :in object-list
	:do (backup-unit o)))) ;;;;;;;so backup-unit will work for any object

