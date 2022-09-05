;;;; search.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :bartleby)

;;; Search methods

;;;; add phone number searching by xxx-xxx-xxxx, (xxx)-xxx-xxxx and xxxxxxxxxx

;;; one option- parse search input,
;;; if one of the words is "client" "employee" or "appointment" it will only search one of those categories

(deftype token ()
  '(cons keyword t))

(defun tok (type &optional val)
  (cons type val))

(alexa:define-string-lexer search-lexer
  "A Lexer for bartleby search queries."
  (;(:hyphen-word "[A-Za-z]\\-[A-Za-z]")
   (:phone "[0-9][0-9]*|[0-9][0-9][0-9\-[0-9][0-9")
   (:word "[A-Za-z][A-Za-z]*")
   (:email "[A-Za-z0-9][A-Za-z0-9]*@[A-Za-z][A-Za-z]*\.[A-Za-z][A-Za-z]*")
   (:space " "))
  ;("{{HYPHEN-WORD}}" (return (tok :hyphen-word (princ-to-string $@))))
  ("{{WORD}}"        (return (tok :word (princ-to-string $@))))
  ("{{NUM}}"         (return (tok :number (princ-to-string $@))))
  ("{{EMAIL}}"       (return (tok :email (princ-to-string $@))))
  ("{{SPACE}}" nil))

(defun lex-query (query)
  "Breaks down a formula string into tokens."
  (loop :with lexer := (search-lexer query)
	:for tok := (funcall lexer)
	:while tok
	:collect tok))

					; Add services, make service enumeration for searching ease

(defun word-search (word)
  (union (client-first-name-search word)
	 (client-last-name-search word)

					;	 (employee-first-name-search word) ;this function doesn't exist
					;	 (employee-last-name-search word)  ;add this function
	 
;(defun id-search (id)
;  (cond ((client-id-search id)
;;	 (client-id-search id))
;	((employee-id-search id)
;	 (employee-id-search id))
;	((appointment-id-search id)
;	 (appointment-id-search id))
;	(t nil)))




;(defun parse-query (input)
 ; "Parses a search query into words."
  ;(loop :with parsed       := nil
;	:with current-word := ""
;	
;	:for i :from 1 :to (length input)
;	:if (string-equal (subseq input (- i 1) i) " ")
;	  :do (progn (setq parsed (cons current-word parsed))
;		     (setq current-word ""))
;	:else
;	  :do (setq current-word (concatenate 'string current-word (subseq input (- i 1) i)))
;	:finally (return (reverse parsed))))


;(defun client-search (input) ;taken as string
 ; (loop :for c :in *clients*
;	:if (or (string-equal input (first-name c))
;		(string-equal input (last-name c))
;		(equal (parse-integer input) (id c))
;		(string-equal input (concatenate 'string (area (phone c)) (middle (phone c)) (end (phone c))));
;		(string-equal input (concatenate 'string (username (email c)) (domain (email c))))
;		(equal input (address c))
;;		(equal input (credit-minutes c))
;		(string-equal input (notes c)))
;	  :collect c :into results
;	:finally (return results)))

(defun date-search (date-string)
  "Searches for a date input as mm/dd/yyyy") ;also make functions for converting Dates to dates

(defun system-search ()
  "Searches through clients, employees, and relevant appointments.")
;;;;return clients top, employees next, then appointments
