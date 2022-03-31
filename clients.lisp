;;;;students.lisp

(in-package :schedulizer)

;;;;------------------------------------------------------------------------
;;;;Client lists and backups
;;;;------------------------------------------------------------------------

(defvar *clients* nil)

(defun add-client-to-list (client)
  (push client *clients*))

;;;;------------------------------------------------------------------------
;;;;Defining the client class
;;;;------------------------------------------------------------------------
(defclass client ()
  ((first-name     :initarg :first-name
                   :accessor first-name)
   (last-name      :initarg :last-name
	           :accessor last-name)
   (client-id      :initarg :client-id
	           :accessor client-id)
   (phone          :initarg :phone
		   :accessor phone) ;default to nil
   (email          :initarg :email
		   :accessor email)
   (makeup-credits :initarg :makeups
		   :accessor makeups) ;default 0
   ;(lesson-history :initarg :history
;		   :accessor history)
   ;(bookshelf     :initarg :bookshelf   
   (notes          :initarg :notes
		   :accessor notes)))

(defmethod print-object ((obj client) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((first-name first-name)
		     (last-name last-name)
		     (client-id client-id)
		     (phone phone)
		     (email email)
		     (makeups makeups)
		     (notes notes))
	obj
      (format stream
	      "~%Name: ~a ~a~% ID: ~a~% Phone: ~a~%Email: ~a~%Makeup Minutes: ~a~%Notes: ~a~%"
	      first-name last-name client-id phone email makeups notes))))

(defun make-client (first-name last-name client-id makeups phone email notes)
  (make-instance 'client :first-name first-name
		         :last-name  last-name
			 :client-id         client-id
			 :phone      phone
			 :email      email
			 :makeups    makeups
			 :notes      notes))

(defun new-client (first-name last-name phone email notes)
  "generates a client with a new id and default makeups"
  (make-instance 'client :first-name first-name
		         :last-name  last-name
			 :client-id         (new-id)
			 :phone      phone
			 :email      email
			 :makeups    0
			 :notes      notes))

(add-client-to-list (make-client "joe" "jonas" 1001 "45" "4043872185" "joejoe@joe.joe" "not the best jonas"))
;;;;------------------------------------------------------------------------
;;;;ID generation
;;;;------------------------------------------------------------------------
(defvar last-id (client-id (first *clients*)))

(defun new-id ()
  (setq last-id (+ last-id 1))
  last-id)

;;;;search
(add-client-to-list (new-client "nick" "jonas" "423251324" "nicknick@joe.joe" "i guess"))

(add-client-to-list (new-client "kevin" "jonas" "405321235" "kevinisbest@joe.joe" "the best"))

(add-client-to-list (new-client "jeff" "jonass" "4032458590" "jeff@jonass.com" "ghmm?:"))

;;;;------------------------------------------------------------------------
;;;;search and organization
;;;;------------------------------------------------------------------------

(defun clients-with-makeups ()
  (loop for client in *clients*
	if (> (parse-integer (makeups client)) 0)
	  collect client))

(defun id-search (id)
  (loop for client in *clients*
	if (equal (write-to-string id) (write-to-string (client-id client)))
	  do (return client)))

(defun last-name-search (last-name)
  (loop for client in *clients*
	if (equal last-name (last-name client))
	  do (return client)))

(defun first-name-search (first-name)
  (loop for client in *clients*
	if (equal first-name (first-name client))
	  do (return client)))

;;;;------------------------------------------------------------------------
;;;;Client Database
;;;;------------------------------------------------------------------------

