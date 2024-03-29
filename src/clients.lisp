;;;; clients.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :bartleby)

;;; Client class/sql object

(mito:deftable client ()
  ((first-name :col-type (:varchar 64))
   (last-name  :col-type (:varchar 64))
   (phone      :col-type (or (:char 10) :null))
   (email      :col-type (or (:varchar 64) :null))
   (address    :col-type (or (:varchar 64) :null))
   (notes      :col-type (or (:varchar 128) :null)))
  (:conc-name client-))

(mito:ensure-table-exists 'client)

(defmethod print-object ((obj client) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((first-name client-first-name)
		     (last-name  client-last-name)
		     (phone      client-phone)
		     (email      client-email)
                     (address    client-address)
		     (notes      client-notes))
	obj
      (format stream
	      "~%Name: ~a ~a~%Phone: ~a~%Email: ~a~%Address: ~a~%Credit Minutes:~%Notes: ~a~%"
	      first-name last-name phone email address  notes))))

(defmethod pretty-print ((client client))
  (format nil "~a ~a" (client-first-name client) (client-last-name client)))


(defun make-client (first-name last-name phone email address notes)
  (make-instance 'client :first-name first-name
                         :last-name  last-name
                         :phone      phone
                         :email      email
                         :address    address
                         :notes      notes))

;;; Adding and removing clients

(defmethod add-client ((client client))
  "Adds a client to the Client sql table"
  (mito:insert-dao client))

(defmethod remove-client ((client client))
  "Removes the client from the Client sql table"
  (mito:delete-dao client))

(defmethod replace-client ((client client) new-client)
  "Removes the client, adds a new client in its place."
  (remove-client client)
  (add-client new-client))

;;; Editing one attribute at a time

(defmethod change-first-name ((client client) first-name)
  "Changes the first name of a client."
  (setf (slot-value client 'first-name) first-name)
  (mito:save-dao client))

(defmethod change-last-name ((client client) last-name)
  "Changes the last name of a client."
  (setf (slot-value client 'last-name) last-name)
  (mito:save-dao client))

(defmethod change-phone ((client client) new-phone)
  "Changes the phone number of a client"
  (setf (slot-value client 'phone) new-phone)
  (mito:save-dao client))

(defmethod change-email ((client client) new-email)
  "Changes the email of a client"
  (setf (slot-value client 'email) new-email)
  (mito:save-dao client))

(defmethod change-address ((client client) new-address)
  "Changes the address of a client"
  (setf (slot-value client 'address) new-address)
  (mito:save-dao client))

(defmethod change-credits ((client client) new-credits)
  "Changes the credit minutes of a client"
  (setf (slot-value client 'credits) new-credits)
  (mito:save-dao client))

;(defmethod change-notes ((client client) new-notes)
 ; "Changes notes on a client"
  ;(setf (slot-value client 'notes) new-notes)
  ;(;mito:save-dao client))

;;; Find clients

(defun client-count ()
  "Returns the number of clients stored in the database"
  (mito:count-dao 'client))

(defun client-id-search (client-id)
  "Searches for a client by id"
  (mito:find-dao 'client :id client-id))

(defun all-clients ()
  (loop :with clients := nil
	:with cc      := (client-count)

	:for i :upfrom 1
	:do (setq clients (if (null (client-id-search i))
			      clients
			      (cons (client-id-search i) clients)))
	:when (equal (length clients) cc)
	  :do (return clients)))

(defun client-first-name-search (first-name)
  (remove-if-not #'(lambda (c)
	       (string-equal first-name (client-first-name c)))
	   (all-clients)))

(defun client-last-name-search (last-name)
  (remove-if-not #'(lambda (c)
	       (string-equal last-name (client-last-name c)))
	   (all-clients)))
