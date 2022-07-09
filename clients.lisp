;;;; clients.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :bartleby)

;;; Client class/sql object

(mito:deftable client ()
  (;(id :col-type (:varchar 36)
    ;   :primary-key t)
   (first-name :col-type (:varchar 64))
   (last-name  :col-type (:varchar 64))
   (phone      :col-type (or (:char 10) :null))
   (email      :col-type (or (:varchar 64) :null))
   (address    :col-type (or (:varchar 64) :null))
   ;(credits    :col-type (:int))
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
                     ;(credits    client-credits)
		     (notes      client-notes))
	obj
      (format stream
	      "~%Name: ~a ~a~%Phone: ~a~%Email: ~a~%Address: ~a~%Credit Minutes:~%Notes: ~a~%"
	      first-name last-name phone email address  notes))))

(defun make-client (first-name last-name phone email address notes)
  (make-instance 'client :first-name first-name
                         :last-name  last-name
                         :phone      phone
                         :email      email
                         :address    address
                         :notes      notes))

;;; Adding and removing clients

(defmethod add-client ((client client))
  (mito:insert-dao client))

(defun new-client (first-name last-name phone email address notes)
  (mito:create-dao (make-client first-name last-name phone email address 0 notes)))

(defmethod remove-client ((client client))
  "Removes the client from the Client sql table"
  (mito:delete-dao client))

(defmethod replace-client ((client client) new-client)
  "Removes the client, adds a new client in its place."
  (remove-client client)
  (add-client new-client))

;;; Editing one attribute at a time

(defmethod change-first-name ((client client) first-name)
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

(defmethod change-notes ((client client) new-notes)
  "Changes notes on a client"
  (setf (slot-value client 'notes) new-notes)
  (mito:save-dao client))

;;; Find clients

(defun client-count ()
  (mito:count-dao 'client))

(defun client-id-search (client-id)
  (mito:find-dao 'client :id client-id))

(defun client-first-name-search (first-name)
  (loop :for i :from 1 :to (client-count)
        :if (string-equal (client-first-name (mito:find-dao 'client :id i))
                          first-name)
          :collect (mito:find-dao 'client :id i) :into matches
        :finally (return matches)))

(defun client-last-name-search (last-name)
  (loop :for i :from 1 :to (client-count)
        :if (string-equal (client-last-name (mito:find-dao 'client :id i))
                          last-name)
          :collect (mito:find-dao 'client :id i) :into matches
        :finally (return matches)))

;(defun client-full-name-search (first-name last-name)
 ; (intersection (client-first-name-search first-name)
  ;              (client-last-name-search last-name)))


;;; Client Credits

(defun credit-count ()
  (mito:count-dao 'credit))

(defmethod total-credit-minutes ((client client))
  (loop :for i :from 1 :to (credit-count)
	:if (string-equal (client-id client)
                          (client-id (credit-client (mito:find-dao 'credit :id i))))
          :sum (credit-minutes (mito:find-dao 'credit :id i)) :into total-minutes
        :finally (return total-minutes)))


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

;(defun last-name-search (last-name)
 ; "Searches for a client by their last name."
  ;(loop :for client :in *clients*
;	:if (equal last-name (last-name client))
;	  :do (return client)))

;(defun first-name-search (first-name)
;  "Searches for a client by their first name."
 ; (loop :for client :in *clients*
;	:if (equal first-name (first-name client))
;	  :do (return client)))

;(defun full-name-search (first-name last-name)
 ; "Searches for a client by their full name."
  ;(loop :for client :in *clients*
;	:if (and (equal first-name (first-name client))
;		(equal last-name  (last-name client)))
;;	  :do (return client)))
