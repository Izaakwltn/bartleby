;;;;lexing.lisp
;;;;

(in-package :bart)

(defvar *commands* '(("help"      #'help)
		     ("browse"    #'browse)
		     ("view"      #'view)
		     ("new"       #'new)
		     ("edit"      #'edit)
		     ("check-out" #'check-out)
		     ("invoice"   #'invoice)
		     ("search"    #'search))) ;delete

(defvar *lists*    '(("clients"       bartleby::*clients*)
		     ("employees"     bartleby::*employees*)
		     ("rooms"         bartleby::*rooms*)
		     ("appointments"  (sort (copy-list bartleby::*appointments*)
				       #'(lambda (a1 a2)
					   (bartleby::later-date-time-p
					    (bartleby::dt a1)
					    (bartleby::dt a2)))))));;;;fix appointments sort

(defvar *objects*   '("client"
		      "employee"
		      "room"
		      "appointment")) ;receipt, invoice?

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
  "Generates a lexeme from a given token"
  (cond ((find-token token *commands*)
	 (make-lexeme "command" (second (find-token token *commands*))))
	((find-token token *lists*)
	 (make-lexeme "list" (second (find-token token *lists*))))
	((find-if #'(lambda (tok)
		     (string-equal token tok))
		  *objects*)
	 (make-lexeme "object"
		      (find-if #'(lambda (tok)
				   (string-equal token tok))
			       *objects*)))
	(t (make-lexeme "keyword" token))))

(defun lex-list (parsed-list)
  "Takes a list of parsed tokens, returns lexemes for each."
  (loop :for token :in parsed-list
	:collect (lex-token token)))

(defun lex-input (input)
  "Parses then lexes the input."
  (lex-list (parse-input input)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))
