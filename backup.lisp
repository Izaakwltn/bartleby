;;;;backup.lisp

(in-package :bartleby)

;;;;------------------------------------------------------------------------
;;;;Functions for backing up
;;;;------------------------------------------------------------------------

(defgeneric backup-unit (object)
  (:documentation "Prepares an object for backup."))
					;date, time, client, employee, room, appointment ...

(defun make-backup (backup-name object-list)
  "Writes a backup of a given object-list to the specified filename."
  (with-open-file (out (asdf:system-relative-pathname "schedulizer"
						      (concatenate 'string
								   "backup-of-" backup-name ".lisp"))
		       :direction         :output
		       :if-does-not-exist :create
		       :if-exists         :supersede)
    (format out ";;;;~a~%(in-package :schedulizer)~%" backup-name)
    (loop :for o :in object-list
	  :do (format out "~a~%" (backup-unit o)))))

(defun blank-backup (backup-name)
  (with-open-file (out (asdf:system-relative-pathname "schedulizer"
						      (concatenate 'string
								   "backup-of-" backup-name ".lisp"))
		       :direction :output
		       :if-does-not-exist :create
		       :if-exists nil)
    (format out ";;;;~a~%(in-package :schedulizer)~%" 
	    (concatenate 'string "backup-of-" backup-name ".lisp"))))

(defun and-then-there-were-backups (backup-item-names)
  (loop :for b :in backup-item-names
	:do (blank-backup b)))

(and-then-there-were-backups '("clients" "employees" "rooms" "appointments"))

(defgeneric load-saved-item (object)
  (:documentation "Adding item to its list without updating backup"))

;;;;Eventually SQL- maybe use MITO

;;;;because they'll be git-ignored
					;check whether the backups exist, write them on load if not
