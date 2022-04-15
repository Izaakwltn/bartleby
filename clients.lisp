;;;;clients.lisp

(in-package :bartleby)

;;;;------------------------------------------------------------------------
;;;;Clients
;;;;------------------------------------------------------------------------

(defclass client ()
  ((first-name     :initarg :first-name
                   :accessor first-name)
   (last-name      :initarg :last-name
	           :accessor last-name)
   (id             :initarg :id
	           :accessor id)
   (phone          :initarg :phone
		   :accessor phone)
   (email          :initarg :email
		   :accessor email)
   (address        :initarg :address
		   :accessor address)
   (credit-minutes :initarg :credit-minutes
		   :accessor credit-minutes)
   (notes          :initarg :notes
		   :accessor notes)))

(defmethod print-object ((obj client) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((first-name first-name)
		     (last-name last-name)
		     (id id)
		     (phone phone)
		     (email email)
		     (address address)
		     (credit-minutes credit-minutes)
		     (notes notes))
	obj
      (format stream
	      "~%Name: ~a ~a~% ID: ~a~% Phone: ~a~%Email: ~a~%Address: ~a~%Makeup Minutes: ~a~%Notes: ~a~%"
	      first-name last-name id phone email address credit-minutes notes))))

(defun make-client (first-name last-name id credit-minutes phone email address  notes)
  (make-instance 'client :first-name     first-name
		         :last-name      last-name
			 :id             id
			 :phone          phone
			 :email          email
			 :address        address
			 :credit-minutes credit-minutes
			 :notes          notes))

;;;;------------------------------------------------------------------------
;;;;Adding to, removing from, and editing *clients*
;;;;------------------------------------------------------------------------

(defvar *clients* nil)

(defmethod add-client ((client client))
  "Add a client to *clients*"
  (push client *clients*)
  (refresh-client-backup))

(defmethod remove-client ((client client))
  "Removes the client from *clients*, backup tbd."
  (setq *clients*
	(remove-if #'(lambda (c)
		       (equal (id c) (id client)))
		   *clients*))
  (refresh-client-backup))

(defmethod replace-client ((client client) new-client)
  "Removes the client, adds a new client in its place."
  (remove-client client)
  (add-client new-client))

;;;;------------------------------------------------------------------------
;;;;Editing one attribute at a time
;;;;------------------------------------------------------------------------

(defmethod change-first-name ((client client) first-name)
  "Changes the first name of a client"
  (replace-client client (make-client first-name
				      (last-name client)
				      (id client)
				      (credit-minutes client)
				      (phone     client)
				      (email     client)
				      (address     client)
				      (notes     client))))

(defmethod change-last-name ((client client) last-name)
  "Changes the last name of a client."
  (replace-client client (make-client (first-name client)
				      last-name
				      (id client)
				      (credit-minutes   client)
				      (phone     client)
				      (email     client)
				      (address   client)
				      (notes     client))))

(defmethod change-id ((client client) client-id)
  "Changes the client ID of a client"
  (replace-client client (make-client (first-name client)
				      (last-name  client)
				      id 
				      (credit-minutes client)
				      (phone      client)
				      (email      client)
				      (address    client)
				      (notes      client))))

(defmethod change-credits    ((client client) credits)
  "Changes the credit minutes of a client"
  (replace-client client (make-client (first-name client)
				      (last-name client)
				      (id client)
				      credits
				      (phone     client)
				      (email     client)
				      (address   client)
				      (notes     client))))

(defmethod change-phone ((client client) new-phone)
  "Changes the phone number of a client"
  (replace-client client (make-client (first-name     client)
 				      (last-name      client)
				      (id      client)
				      (credit-minutes client)
				      new-phone
				      (email          client)
				      (address        client)
				      (notes          client))))

(defmethod change-email ((client client) email)
  "Changes the email of a client"
  (replace-client client (make-client (first-name     client)
 				      (last-name      client)
				      (id      client)
				      (credit-minutes client)
				      (phone          client)
				      email
				      (address        client)
				      (notes          client))))

(defmethod change-address ((client client) address)
  "Changes the address of a client"
  (replace-client client (make-client (first-name     client)
 				      (last-name      client)
				      (id      client)
				      (credit-minutes client)
				      (phone          client)
				      (email          client)
				      address
				      (notes          client))))

(defmethod change-notes ((client client) notes)
  "Changes notes on a client"
  (replace-client client (make-client (first-name     client)
 				      (last-name      client)
				      (id      client)
				      (credit-minutes client)
				      (phone          client)
				      (email          client)
				      (address        client)
				      notes)))

;;;;------------------------------------------------------------------------
;;;;Adding new clients
;;;;------------------------------------------------------------------------

(defvar last-client-id (if (first *clients*)
			   (id (first *clients*))
			   1001))

(defun new-client-id ()
  "Generates a new client-id, takes note of the most recent id."
  (setq last-client-id (+ last-client-id 1))
  last-client-id)

(defun new-client (first-name last-name phone email address notes)
  "generates a client with a new id and default makeups"
  (add-client (make-instance 'client :first-name first-name
		                     :last-name  last-name
			             :id  (new-client-id)
			             :phone      phone
			             :email      email
				     :address    address
			             :credit-minutes    0
			             :notes      notes)))

;;;;------------------------------------------------------------------------
;;;;Backing up clients
;;;;------------------------------------------------------------------------

(defmethod backup-unit ((client client))
  (format nil "(load-saved-item (make-client ~a ~a ~a ~a ~a ~a ~a ~a))~%"
	  (write-to-string (first-name client))
	  (write-to-string (last-name client))
	  (id client)
	  (credit-minutes client)
	  (backup-unit (phone client))
	  (backup-unit (email client))
	  (backup-unit (address client))
	  (write-to-string (notes client))))

(defun refresh-client-backup ()
  (make-backup "clients" *clients*))

(defmethod load-saved-item ((client client))
  (push client *clients*))
;;;;------------------------------------------------------------------------
;;;;Searching for clients
;;;;------------------------------------------------------------------------
;;;;composite search- return combined list of items that match each search

;might end up just in search.lisp 
(defun clients-with-credits ()
  "Returns a list of all clients with makeup credits."
  (loop :for client :in *clients*
	:if (> (parse-integer (credit-minutes client)) 0)
	  :collect client))

(defun client-id-search (id)
  "Searches for a client by their client id."
  (loop :for client :in *clients*
	:if (equal (write-to-string id) (write-to-string (id client)))
	  :do (return client)))

(defun last-name-search (last-name)
  "Searches for a client by their last name."
  (loop :for client :in *clients*
	:if (equal last-name (last-name client))
	  :do (return client)))

(defun first-name-search (first-name)
  "Searches for a client by their first name."
  (loop :for client :in *clients*
	:if (equal first-name (first-name client))
	  :do (return client)))

(defun full-name-search (first-name last-name)
  "Searches for a client by their full name."
  (loop :for client :in *clients*
	:if (and (equal first-name (first-name client))
		(equal last-name  (last-name client)))
	  :do (return client)))

;;;;------------------------------------------------------------------------
;;;;
;;;;------------------------------------------------------------------------

