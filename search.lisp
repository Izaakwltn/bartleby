;;;;search.lisp

(in-package :schedulizer)

;;;;------------------------------------------------------------------------
;;;;Search methods
;;;;------------------------------------------------------------------------

;;;one option- parse search input,
;;;;if one of the words is "client" "employee" or "appointment" it will only search one of those categories

(defun parse-query (input)
  "Parses a search query into words."
  (loop :with parsed       := nil
	:with current-word := ""
	
	:for i :from 1 :to (length input)
	:if (string-equal (subseq input (- i 1) i) " ")
	  :do (progn (setq parsed (cons current-word parsed))
		     (setq current-word ""))
	:else
	  :do (setq current-word (concatenate 'string current-word (subseq input (- i 1) i)))
	:finally (return (reverse parsed))))


(defun client-search (input) ;taken as string
  (loop :for c :in *clients*
	:if (or (string-equal input (first-name c))
		(string-equal input (last-name c))
		(equal (parse-integer input) (id c))
		(string-equal input (concatenate 'string (area (phone c)) (middle (phone c)) (end (phone c))))
		(string-equal input (concatenate 'string (username (email c)) (domain (email c))))
		(equal input (address c))
		(equal input (credit-minutes c))
		(string-equal input (notes c)))
	  :collect c :into results
	:finally (return results)))

(defun date-search (date-string)
  "Searches for a date input as mm/dd/yyyy") ;also make functions for converting Dates to dates

(defun system-search ()
  "Searches through clients, employees, and relevant appointments."
;;;;return clients top, employees next, then appointments
