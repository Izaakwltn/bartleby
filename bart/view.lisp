;;;;bart/view.lisp
;;;;

(in-package :bart)

(defgeneric view-print (object)
  (:documentation "Prints an object for viewing"))

(defmethod view-print ((client bartleby:client))
  (format nil "~a ~a, ID: ~a~%~a~%~a~%Credits: ~a~%Notes: ~a~%"
	  (bartleby::first-name client)
	  (bartleby::last-name client)
	  (bartleby::id client)
	  (bartleby::phone client)
	  (bartleby::email client)
	  (bartleby::credit-minutes client)
	  (bartleby::notes client)))

(defmethod view-print ((employee bartleby:employee))
  (format nil "~a ~a, ID: ~a~%~a~%~a~%Hourly Rate: ~a~%"
	  (bartleby::first-name  employee)
	  (bartleby::last-name   employee)
	  (bartleby::id          employee)
	  (bartleby::phone       employee)
	  (bartleby::email       employee)
	  (bartleby::hourly-rate employee)))

(defmethod view-print ((appointment bartleby:appointment))
  (format nil "~a"
	  appointment))

(defun view-prompt ()
  (write-line "Please enter a client, employee, or appointment ID.")
  (view (parse-integer (prompt-read "ID"))))

(defun view (&optional id)
  (if id
      (format t "~a" (view-print (bartleby::id-search (if (numberp id)
							  id
							  (parse-integer id)))))
      (view-prompt)))
