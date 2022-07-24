;;;; web-bartleby/new-client.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :web-bartleby)

(hunchentoot:define-easy-handler (new-client-form :uri "/new-client") ()
  (with-page (:title "New Client")
    (:h1 "New Client")
    (:form
     (:label :for "fname"
	     "First Name")
     (:input :type "text" :id "fname" :name "fname")
     (:hr)
     (:label :for "lname"
	     " Last Name ")
     (:input :type "text" :id "lname" :name "lname")
     (:hr)
     (:label :for "phone"
	     "Phone Number XXXXXXXXXX ")
     (:input :type "text" :id "phone" :name "phone")
     (:hr)
     (cl-bootstrap:bs-form-email ())
     (:hr)
     (:label :for "address"
	     "Address ")
     (:input :type "text" :id "address" :name "address")
     (:hr)
     (:label :for "notes"
	     "Notes ")
     (:input :type "text" :id "notes" :name "notes")
	          (cl-bootstrap:bs-form-checkbox "Check me out")
		  (:button :type "submit" :class "btn btn-default" "Submit"))))

;(hunchentoot:define-easy-handler (submit-new-client :uri "/submit-new-client") ()
 ; (
