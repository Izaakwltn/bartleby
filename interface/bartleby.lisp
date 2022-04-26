;;;;bartleby.lisp
;;;;

(in-package :bartleby)

;;;;main interface for bartleby

;(defun help ()
;  (write-line "---*---*---*---*---*---*---*---*--*--*--")
;  (write-line (format nil "Hopefully this will make things a bit easier:~%"))
 ; (write-line "---*---*---*---*---*---*---*---*--*--*--")
 ; (write-line (format nil "~%-If you have thought better of this whole process and wish to depart,"))
 ; (write-line (format nil "~%please type either EXIT or QUIT.~%"))
;  (write-line "---*---*---*---*---*---*---*---*--*--*--")
;  (write-line "Browsing:")
;  (write-line (format nil "~%-You can enter a name of a list: appointments, clients, employees, or rooms ~%"))
;  (write-line (format nil "-Or use the command BROWSE along with the name of a list.~%"))
;  (write-line (format nil "-If in despair, merely type BROWSE and I will do my best to help.~%"))
;  (write-line "---*---*---*---*---*---*---*---*--*--*--")
  ;(write-line "Adding New Items:")
 ; (write-line (format nil "~%-Either use NEW with a type of object (client, employee, room, appointment) ~%"))
;  (write-line (format nil "-If in despair, merely type NEW and I will do my best to help.~%"))
 ; (write-line "---*---*---*---*---*---*---*---*--*--*--"))  

(defun interpret (lexed-input)
  "Interprets lexed input in Bartleby Interface"
  (cond ((string-equal (tok-type (first lexed-input))
		       "command")
	 (if (equal (length lexed-input) 1)
	     (funcall (eval (token (first lexed-input))))
	     (funcall (eval (token (first lexed-input)))
		      (eval (token (second lexed-input))))))
	     ;(funcall (eval (token (first lexed-input))) (mapcar  ;;;fix it for multiple arguments
	;;	   (loop :for i :in (rest lexed-input)
	;		 :collect (eval (token i))))))) ;;;for now just one command and one argument
	 ((string-equal (tok-type (first lexed-input))
		       "list")
	  (browse (eval (token (first lexed-input)))))
	 ;((string-equal (tok-type (first lexed-inputif it's an object, 
	 (t (write-line "I would prefer not to."))))
	 

(defun bartleby-quit ()
  (write-line "I would prefer not to. Goodbye!"))

(defun bartleby-prompt ()
  (let ((input (prompt-read "> ")))
    (if (or (string-equal input "exit")
	    (string-equal input "quit"))
	 (bartleby-quit)
	 (progn (interpret (lex-input input))
	        (bartleby-prompt)))))
	
(defun bartleby ()
  (progn (write-line (format nil "~%Welcome to Bartleby the Scheduler.~%~%You can interact directly with Bartleby using certain commands.~%~%To become familiar with the commands, type \"help,\"~%otherwise Bartleby may prefer not to complete your request.~%"))
	 (bartleby-prompt)))
