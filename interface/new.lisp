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

(defun parse-time (time-string) ;hh:mm
  (loop :with hh := ""
	:with mm := ""

	:for i :from 1 :to 5
	:if (< i 3)
	  :do (setq hh (concatenate 'string hh (subseq time-string (- i 1) i)))
	:if (> i 3)
	  :do (setq mm (concatenate 'string mm (subseq time-string (- i 1) i)))
	:finally (return (set-time (parse-integer hh)
				   (parse-integer mm)))))
       
(defun parse-date (date-string) ;mm/dd/yyyy
  "parse date given as mm/dd/yyyy"
  (loop :with mm := ""
	:with dd := ""
	:with yyyy := ""

	:for i :from 1 :to 10
	:if (< i 3)
	  :do (setq mm (concatenate 'string mm (subseq date-string (- i 1) i)))
	:else
	  :if (and (> i 3) (< i 6))
	    :do (setq dd (concatenate 'string dd (subseq date-string (- i 1) i)))
	:else
	  :if (> i 6)
	    :do (setq yyyy (concatenate 'string yyyy (subseq date-string (- i 1) i)))
	:finally (return (date (parse-integer mm)
			       (parse-integer dd)
			       (parse-integer yyyy)))))

;;;;new appointment- recurring option
(defun new-appointment-prompt ()
  (new-appointment (mapcar #'parse-integer (parse-input (prompt-read "Enter client IDs, separated by spaces")))
		   (mapcar #'parse-integer (parse-input (prompt-read "Enter employee IDs, separated by spaces")))
		   (prompt-read "Room Number")
		   (date-time (parse-date (prompt-read "Date (mm/dd/yyyy)"))
			      (parse-time (prompt-read "Time (hh:mm) 24 hour time")))
		   (prompt-read "Duration (minutes)")
		   (prompt-read "Notes")))

(defun new (&optional object)
  (if object
      (funcall (eval (second (find-if #'(lambda (i)
					  (string-equal (first i) object))
				      *new-prompts*))))
      (new-prompt)))
