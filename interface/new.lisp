;;;;new.lisp
;;;;
;;;;

(in-package :bartleby)

(defvar *new-items* '(("client" #'new-client)
		      ("employee" #'new-employee)
		      ("room"     #'new-room)
		      ("appointment" #'new-appointment)))

(defvar *new-prompts* '(("client"      #'new-client-prompt)
			("employee"    #'new-employee-prompt)
			("room"        #'new-room-prompt)
			("appointment" #'new-appointment-prompt)))
(defun new-prompt ()
  (write-line (format nil "~%What would you like to add?~%"))
  (let ((input (prompt-read (format nil "Client, employee, room, or appointment?~%> "))))
  (funcall
   (eval
    (second
     (find-if #'(lambda (i)
		  (string-equal (first i) input))
	      *new-prompts*))))))

(defun new-client-prompt ()
  (new-client (prompt-read "First Name")
	      (prompt-read "Last Name")
	      (prompt-read "Phone number (xxxxxxxxxx)")
	      (prompt-read "Email")
	      (random-address)
	      (prompt-read "Notes")))

(defun new-employee-prompt ()
  (new-employee (prompt-read "First Name")
	        (prompt-read "Last Name")
	        (prompt-read "Phone number (xxxxxxxxxx)")
	        (prompt-read "Email")
	        (random-address)
	        (prompt-read "Hourly Rate")))

(defun new-room-prompt ()
  (new-room (prompt-read "Room Name")
	    (prompt-read "Capacity")
	    (prompt-read "Notes")))

(defun parse-date (date-string) ;dd/mm/yyyy
  "parse date given as dd/mm/yyyy")
;(defun new-appointment-prompt ()
 ; (new-appointment (mapcar #'parse-integer
;			   (parse-input (prompt-read "Client IDs, separated by spaces")))
;		   (mapcar #'parse-integer
;			   (parse-input (prompt-read "Employee ID's separated by spaces")))
;		   (parse-integer (prompt-read "Room Number"))
;;;;;add functions parse date, parse time		   

(defun new (&optional object)
  (if object
      (funcall (eval (second (find-if #'(lambda (i)
					  (string-equal (first i) object))
				      *new-prompts*))))
      (new-prompt)))
