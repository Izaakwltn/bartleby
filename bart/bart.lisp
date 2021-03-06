;;;;bartleby.lisp
;;;;

(in-package :bart)

;;;;main interface for bartleby

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
	 

(defun bart-quit ()
  (write-line "I would prefer not to. Goodbye!"))

(defun bart-prompt ()
  (let ((input (prompt-read "> ")))
    (if (or (string-equal input "exit")
	    (string-equal input "quit"))
	 (bart-quit)
	 (progn (interpret (lex-input input))
	        (bart-prompt)))))
	
(defun bart ()
  (progn (write-line (format nil "~%Welcome to Bartleby the Scheduler.~%~%You can interact directly with Bartleby using certain commands.~%~%To become familiar with the commands, type \"help,\"~%otherwise Bartleby may prefer not to complete your request.~%"))
	 (bart-prompt)))
