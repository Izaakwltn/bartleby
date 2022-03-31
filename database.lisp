;;;;database.lisp

(in-package :schedulizer)

(defun update-backup (client)
  (with-open-file (out (asdf:system-relative-pathname "schedulizer" "backup.lisp")
		       :direction :output
		       :if-exists :append)
    (format out
	    "~%(add-client (make-client ~a ~a ~a ~a ~a ~a ~a))"
	    (write-to-string (first-name client))
	    (write-to-string (last-name client))
	    (write-to-string (id client))
	    (write-to-string (phone client))
	    (write-to-string (email client))
	    (write-to-string (makeups client))
	    (write-to-string (notes client)))))

;;;;------------------------------------------------------------------------
;;;;entry interface
;;;;------------------------------------------------------------------------

(defun add-client (client)
  (add-client-to-list client)
  (update-backup client))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun new-client-input ()
  (add-client (new-client (prompt-read "First Name")
			  (prompt-read "Last Name")
			  (prompt-read "Phone (or nil)")
			  (prompt-read "Email (or nil)")
			  (prompt-read "Notes: "))))
(defun new-clients ()
  (loop (new-client-input)
	(if (not (y-or-n-p "Another new client? [y/n]: ")) (return))))
  
