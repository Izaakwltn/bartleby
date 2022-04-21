;;;;repl-interface.lisp
;;;;   maybe interface folder- lexing.lisp, prompts.lisp

(in-package :bartleby)

;;;a system for inputing, editing, and managing appointments

;;;welcome to the Schedule-REPL
;;; Would you like to edit an appointment, client, employee, or room?
;;; What would you like to edit?

(defvar *interface-commands* '(("help" #'help)
			       ("edit" #'edit)
			       ("browse" #'browse)
			       ("search" #'search)
                               ("add" #'add)))
					;command, arguments
(defvar *interface-lists* '(("clients" *clients*)
			    ("employees" *employees*)
			    ("appointments" *appointments*)
			    ("rooms" *rooms*)))

(defvar *interface-types* '("client"
			    "employee"
			    "room"
			    "appointment"))
  
(defun bartleby-error ()
  (format nil "I would prefer not to"))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun parse-input (input)
  "Parses each token separated by spaces"
  (loop :with current-token := ""
	:with parsed := nil
	
	:for i :from 1 :to (length input)
	:if (string-equal (subseq input (- i 1) i) " ")
	  :do (progn (push current-token parsed)
		     (setq current-token ""))
	:else
	  :do (setq current-token (concatenate 'string current-token (subseq input (- i 1) i)))
	:finally (progn (setq parsed (cons current-token parsed))
			(return (reverse parsed)))))

(defclass lexeme ()
  ((tok-type :initarg :tok-type
	     :accessor tok-type)
   (token    :initarg :token
	     :accessor token)))

(defmethod print-object ((obj lexeme) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((tok-type tok-type)
		     (token      token))
	obj
      (format stream "~a | ~a" tok-type token))))

(defun make-lexeme (tok-type token)
  (make-instance 'lexeme :tok-type tok-type
		         :token token))

(defun find-token (token list)
  "Searches the list for a given token string."
  (find-if #'(lambda (i)
	       (string-equal (first i) token))
	   list))

(defun lex-token (token)
  (cond ((find-token token *interface-commands*)
	 (make-lexeme "command" (second (find-token token *interface-commands*))))
	((find-token token *interface-lists*)
	 (make-lexeme "list" (second (find-token token *interface-lists*))))
	((find-token token *types*)
	 (make-lexeme "object" token))
	(t (make-lexeme "keyword" token))))
	;(t (format nil "I would prefer not to."))))

(defun lex-list (parsed-list)
  (loop :for token :in parsed-list
	:collect (lex-token token)))

;(defun interpret (input)
 ; (

;;;;browse-prompt new-prompt 
;;;;;optional prompt-interface
;(defun browse-prompt ()
 ; (format nil "What would you like to browse?~%appointments~%clients~%employees~%rooms~%?")
  ;(interpret (prompt-read "> ")))

;(defun interface-browse (&optional category)
 ; "Browses items, if not prompts with categories."
  ;(if category
   ;   (browse category)
 ;     (browse-prompt)))
;
;(defun main-screen ()
 ; (format nil "Welcome to Bartleby the Scheduler.")
  ;(format nil "type \"help\" for help"))

;(defun help-sheet ()
 ; (format nil "Bartleby Help Menu: "))

  ;edit...
   ;     ...clients
    ;    ...employees
   ;    ...appointments
					;rooms
;add...
      ;...client
      ;...appointment
;browse ... clients employees appointments etc
;search.... .....
;

;(defgeneric interface-add (object)
 ; (:documentation "Prompt for adding an object through the interface"))

;(defgeneric interface-edit (object))
;
;(defun interface-browse (object-list))

;(defgeneric 
;;;search functions
;(defvar *search-results* "The current list of search results, for ease of access."
;(defun client-interface ()
  

;(defun interface ()
 ; (prompt-read 

			  
