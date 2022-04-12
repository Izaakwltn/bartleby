;;;;repl-interface.lisp
;;;;

(in-package :schedulizer)

;;;a system for inputing, editing, and managing appointments

;;;welcome to the Schedule-REPL
;;; Would you like to edit an appointment, client, employee, or room?
;;; What would you like to edit?

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

;;;search functions

;(defun client-interface ()
  

;(defun interface ()
 ; (prompt-read 
