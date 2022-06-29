;;;;clients.lisp

(in-package :bartleby)

;;;;------------------------------------------------------------------------
;;;;Clients
;;;;------------------------------------------------------------------------

(defclass client ()
  ((id             :initarg :id
		   :accessor id)
   (first-name     :initarg :first-name
                   :accessor first-name)
   (last-name      :initarg :last-name
	           :accessor last-name)
   (phone          :initarg :phone
		   :accessor phone)
   (email          :initarg :email
		   :accessor email)
   ;(address        :initarg :address
;		   :accessor address) ;;;;to be added back when brave, or figure out ways to accomodate nil
   (credits        :initarg :credits
		   :accessor credits)
   (notes          :initarg :notes
		   :accessor notes)))

(mito:deftable client ()
  ((id :col-type (:int))
   (first-name :col-type (:varchar 64))
   (last-name :col-type (:varchar 64))
   (phone     :col-type (:int 64) ;;;;can I make a special type?

(defmethod print-object ((obj client) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((first-name first-name)
		     (last-name  last-name)
		     (id         id)
		     (phone      phone)
		     (email      email)
		     (notes      notes))
	obj
      (format stream
	      "~%Name: ~a ~a~% ID: ~a~% Phone: ~a~%Email: ~a~%Address: ~a~%Credit Minutes: ~a~%Notes: ~a~%"
	      first-name last-name id phone email (total-credit-minutes client) notes))))

(defun make-client (id first-name last-name phone email credits notes)
  (make-instance 'client :id         id
			 :first-name first-name
		         :last-name  last-name
			 :phone      phone
			 :email      email
			 :credits    credits
			 :notes      notes))

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

(defun probably-do-not-remove-all-clients ()
  (mapcar #'remove-client *clients*))

(defmethod replace-client ((client client) new-client)
  "Removes the client, adds a new client in its place."
  (remove-client client)
  (add-client new-client))

;;;;------------------------------------------------------------------------
;;;;Editing one attribute at a time
;;;;------------------------------------------------------------------------

(defmethod change-first-name ((client client) first-name)
  "Changes the first name of a client"
  (replace-client client (make-client (id client)
				      first-name
				      (last-name client)
				      (phone     client)
				      (email     client)
				      (credits client)
	    			      (notes     client))))

(defmethod change-last-name ((client client) last-name)
  "Changes the last name of a client."
  (replace-client client (make-client (id client)
				      (first-name client)
				      last-name
				      (phone      client)
				      (email      client)
				      (credits client)
				      (notes      client))))

(defmethod change-id ((client client) client-id)
  "Changes the client ID of a client"
  (replace-client client (make-client id
				      (first-name client)
				      (last-name  client)
				      (phone      client)
				      (email      client)
				      (credits client)
				      (notes      client))))

(defmethod change-phone ((client client) new-phone)
  "Changes the phone number of a client"
  (replace-client client (make-client (id         client)
				      (first-name client)
 				      (last-name  client)
				      new-phone
				      (email      client)
				      (credits client)
				      (notes      client))))

(defmethod change-email ((client client) email)
  "Changes the email of a client"
  (replace-client client (make-client (id client)
				      (first-name client)
 				      (last-name  client)
				      (credits    client)
				      (phone      client)
				      email
				      (credits client)
				      (notes      client))))

;(defmethod change-address ((client client) address)
 ; "Changes the address of a client"
  ;(replace-client client (make-client (first-name     client)
 ;				      (last-name      client)
;				      (id      client)
;				      (credit-minutes client)
;				      (phone          client)
;				      (email          client)
;				      address
;				      (notes          client))))

(defmethod change-notes ((client client) notes)
  "Changes notes on a client"
  (replace-client client (make-client (id             client)
				      (first-name     client)
 				      (last-name      client)
				      (phone          client)
				      (email          client)
				      (credits client)
				      notes)))

;;;;------------------------------------------------------------------------
;;;;Adding new clients
;;;;------------------------------------------------------------------------

(defvar last-client-id (if (first *clients*)
			   (id (first *clients*))
			   1000))

(defun new-client-id ()
  "Generates a new client-id, takes note of the most recent id."
  (setq last-client-id (+ last-client-id 1))
  last-client-id)

(defun new-client (first-name last-name string-phone string-email address notes)
  "generates a client with a new id and default makeups"
  (add-client (make-client (new-client-id)
			   first-name
			   last-name
			   phone-string
			   email-string
			   nil
			   notes)))
			   ;(make-instance 'client :first-name first-name
	;	                     :last-name  last-name
	;		             :id  (new-client-id)
	;		             :phone      (make-phone-number string-p;hone)
	;		             :email      (make-email string-email);
	;			     :address    address
	;		             :credit-minutes    0
	;		             :notes      notes)))

;;;;------------------------------------------------------------------------
;;;;Backing up clients
;;;;------------------------------------------------------------------------
					;offer sql option asap

(defmethod backup-unit ((credit credit))
  (format nil "(make-credit ~a ~a (client-id-search ~a) (appointment-id-search ~a) ~a ~a)"
	  (backup-unit (date-added credit))
	  (backup-unit (expiration-date credit))
	  (id (client credit))
	  (id (orig-appointment credit))
	  (minutes credit)))

(defmethod backup-unit ((client client))
  (format nil "(load-saved-item (make-client ~a ~a ~a ~a ~a ~a ~a ~a))~%"
	  (id client)
	  (write-to-string (first-name client))
	  (write-to-string (last-name client))
	  (id client)
	  (credit-minutes client)
	  (backup-unit (phone client))
	  (backup-unit (email client))
	  (backup-unit (credits client))
	  (write-to-string (notes client))))

(defun refresh-client-backup ()
  (make-backup "clients" (sort (copy-list *clients*) #'(lambda (client1 client2)
				   (< (id client1) (id client2))))))

(defmethod load-saved-item ((client client))
  (push client *clients*))

(defun update-last-client-id ()
  (if (null *clients*)
      (setq last-client-id 1001)
      (setq last-client-id (id (first *clients*)))))

;;;;------------------------------------------------------------------------
;;;;Credit Minutes            ----maybe this should be with receipts, or at least used there
;;;;------------------------------------------------------------------------

(defmethod total-credit-minutes ((client client))
  (loop :for c :in (credits client)
	:sum (minutes c)))


(defmethod add-credit ((client client) date-added minutes &optional expiration-days)
  (make-credit date-added
	       client
               nil
	       minutes
	       (if expiration-days
		   expiration-days
		   nil)))



(defmethod change-credits    ((client client) credits)
  "Changes the credit minutes of a client"
  (replace-client client (make-client (id client)
				      (first-name client)
				      (last-name client)
				      (phone     client)
				      (email     client)
				      (credits client)
				      (notes     client))))

(defmethod add-credits ((client client) new-credits)
  (change-credits client (+ (credit-minutes client) new-credits)))

(defmethod use-credits ((client client) used-credits)
  (change-credits client (- (credit-minutes client) used-credits)))

;;;;------------------------------------------------------------------------
;;;;Searching for clients
;;;;------------------------------------------------------------------------
;;;;composite search- return combined list of items that match each search

;might end up just in search.lisp 
;(defun clients-with-credits ()
 ; "Returns a list of all clients with makeup credits."
  ;(loop :for client :in *clients*
;	:if (> (parse-integer (credit-minutes client)) 0)
;	  :collect client))

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

