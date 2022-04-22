;;;;browse.lisp
;;;;
;;;;


(in-package :bartleby)

;;;;browse functions

(defgeneric browse-print (object)
  (:documentation "prints an object for browsing"))

(defmethod browse-print ((client client))
  (format nil "~a ~a: ID ~a"
	  (first-name client)
	  (last-name client)
	  (write-to-string (id client))))

(defmethod browse-print ((employee employee))
  (format nil "~a ~a: ID ~a"
	  (first-name employee)
	  (last-name employee)
	  (write-to-string id client)))

(defun browse-prompt ()
  (browse (token (lex-token (prompt-read (format nil "What would you like to browse?~%clients~%employees~%appointments~%or rooms"))))))
  
(defun browse (&optional ls)
  (if ls (loop :for o :in ls
	       :do (format t "~a~%" (browse-print o)))
      (browse-prompt)))

;;;;;maybe sort lists before browsing them
