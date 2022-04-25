;;;;browse.lisp
;;;;
;;;;

(in-package :bartleby)

;;;;browse functions

(defgeneric browse-print (object)
  (:documentation "prints an object for browsing"))

(defmethod browse-print ((client client))
  (format nil "~a ~a: ID ~a"
	  (first-name client)
	  (last-name client)
	  (write-to-string (id client))))

(defmethod browse-print ((employee employee))
  (format nil "~a ~a: ID ~a"
	  (first-name employee)
	  (last-name employee)
	  (write-to-string (id employee))))

(defmethod browse-print ((meeting-room meeting-room))
  (format nil "Room ~a, ~a~%Capacity: ~a~%Notes: ~a"
	  (id meeting-room)
	  (room-name meeting-room)
	  (capacity meeting-room)
	  (notes meeting-room)))

(defmethod browse-print ((appointment appointment))
  (let ((app-date (date-o (dt appointment)))
	(app-time (time-o (dt appointment)))
	(clients   (clients appointment))
	(employees (employees appointment)))
  (format nil "Appointment #~a~%~a/~a/~a | ~a:~a | ~a minutes ~%~%Clients:~%~a~%Employees:~%~a"
	  (id appointment)
	  (m app-date)
	  (d app-date)
	  (y app-date)
	  (hour app-time)
	  (minutes app-time)
	  (duration appointment)
	  (printable-people clients)
	  (printable-people employees))))

(defun browse-prompt ()
  (let ((input (prompt-read (format nil "What would you like to browse?~%clients~%employees~%appointments~%or rooms"))))
    (if (or (string-equal input "quit")
	    (string-equal input "exit")
	    (string-equal input "back"))
	(bartleby-prompt)
	(progn (browse (eval (token (lex-token input))))
	 (bartleby-prompt)))))
  
(defun browse (&optional ls)
  (if ls (loop :for o :in ls
		      :do (format t "-------------------~%~a~%" (browse-print o)))
	     (browse-prompt))
      (bartleby-prompt))

;;;;;maybe sort lists before browsing them
