;;;;backup.lisp

(in-package :schedulizer)

;;;;------------------------------------------------------------------------
;;;;Functions for backing up
;;;;------------------------------------------------------------------------

(defgeneric backup-unit (object)
  (:documentation "Prepares an object for backup."))
					;date, time, client, employee, room, appointment ...

(defun make-backup (filename object-list)
  "Writes a backup of a given object-list to the specified filename."
  (with-open-file (out (asdf:system-relative-pathname "schedulizer" filename)
		       :direction         :output
		       :if-does-not-exist :create
		       :if-exists         :supersede)
    (format out ";;;;~a~%(in-package :schedulizer)~%" filename)
    (loop :for o :in object-list
	  :do (format out "~a~%" (backup-unit o)))))

;;;;Eventually SQL- maybe use MITO
