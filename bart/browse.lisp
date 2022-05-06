;;;;browse.lisp
;;;;
;;;;

(in-package :bart)

;;;;browse functions

(defgeneric browse-print (object)
  (:documentation "prints an object for browsing"))

(defmethod browse-print ((client bartleby::client))
  (format nil "~a ~a: ID ~a"
	  (bartleby::first-name client)
	  (bartleby::last-name client)
	  (write-to-string (bartleby::id client))))

(defmethod browse-print ((employee bartleby::employee))
  (format nil "~a ~a: ID ~a"
	  (bartleby::first-name employee)
	  (bartleby::last-name employee)
	  (write-to-string (bartleby::id employee))))

(defmethod browse-print ((meeting-room bartleby::meeting-room))
  (format nil "Room ~a, ~a~%Capacity: ~a~%Notes: ~a"
	  (bartleby::id meeting-room)
	  (bartleby::room-name meeting-room)
	  (bartleby::capacity meeting-room)
	  (bartleby::notes meeting-room)))

(defmethod browse-print ((appointment bartleby::appointment))
  (let ((app-date (bartleby::date-o (bartleby::dt appointment)))
	(app-time (bartleby::time-o (bartleby::dt appointment)))
	(clients   (bartleby::clients appointment))
	(employees (bartleby::employees appointment)))
  (format nil "Appointment #~a~%~a/~a/~a | ~a:~a | ~a minutes ~%~%Clients:~%~a~%Employees:~%~a"
	  (bartleby::id appointment)
	  (bartleby::m app-date)
	  (bartleby::d app-date)
	  (bartleby::y app-date)
	  (bartleby::hour app-time)
	  (bartleby::minutes app-time)
	  (bartleby::duration appointment)
	  (bartleby::printable-people clients)
	  (bartleby::printable-people employees))))

(defun browse-prompt ()
  (let ((input (prompt-read (format nil "What would you like to browse?~%clients~%employees~%appointments~%or rooms"))))
    (if (or (string-equal input "quit")
	    (string-equal input "exit")
	    (string-equal input "back"))
	(bart-prompt)
	(progn (browse (eval (token (lex-token input))))
	 (bart-prompt)))))
  
(defun browse (&optional ls)
  (if ls (loop :for o :in ls
		      :do (format t "-------------------~%~a~%" (browse-print o)))
	     (browse-prompt))
      (bart-prompt))

;;;;;maybe sort lists before browsing them
