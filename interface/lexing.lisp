;;;;lexing.lisp
;;;;

(in-package :bartleby)

(defvar *commands* '(("help"   #'help) ;maybe (help) and then (eval command)
		     ("edit"   #'edit)
		     ("browse" #'browse)
		     ("search" #'search)
		     ("new"    #'new)
		     ("view"   #'view))) ;delete

(defvar *lists*    '(("clients"         *clients*)
		     ("employees"     *employees*)
		     ("rooms"          *rooms*)
		     ("appointments"  *appointments*)))

;(defvar *lists* '(("clients"      ;#'(lambda ()
;		   (copy-list
;		    ;*clients*))
;		  ("employees"   ; #'(lambda ()
;				      (copy-list *employees*))
;		  ("rooms"        ;#'(lambda ()
;				      (copy-list *rooms*))
;		  ("appointments" ;#'(lambda ()
;				      (copy-list *appointments*))))

(defvar *objects*   '("client"
		      "employee"
		      "room"
		      "appointment"))

;(defvar *attributes* '(

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
	 (make-lexeme "object" (find-if #'(lambda (tok)
		     (string-equal token tok))
		  *objects*)))
	(t (make-lexeme "keyword" token))))

(defun lex-list (parsed-list)
  "Takes a list of parsed tokens, returns lexemes for each."
  (loop :for token :in parsed-list
	:collect (lex-token token)))

(defun lex-input (input)
  (lex-list (parse-input input)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))