;;;; webbartleby/new-employee.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :webbartleby)

(hunchentoot:define-easy-handler (new-employee-form :uri "/new-employee") ()
  (with-page (:title "New Employee")
    (:h1 "New Employee")
    (:form :action "/submit-new-employee"
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
     (:label :for "email"
	     "Email ")
     (:input :type "text" :id "email" :name "email")
     (:hr)
     (:label :for "address"
	     "Address ")
     (:input :type "text" :id "address" :name "address")
     (:hr)
     (:label :for "hourly-rate"
	     "Hourly Rate ")
     (:input :type "text" :id "hourly-rate" :name "hourly-rate")
     (:hr)
     (:label :for "services"
             "Services- list separated by spaces")
     (:input :type "text" :id "services" :name "services")
     (:hr)
     (:label :for "notes"
	     "Notes ")
     (:input :type "text" :id "notes" :name "notes")
	         ; (cl-bootstrap:bs-form-checkbox "Check me out")
		  (:button :type "submit" :class "btn btn-default" "Submit"))))

(hunchentoot:define-easy-handler (submit-new-employee :uri "/submit-new-employee")
  (fname lname phone email address hourly-rate services notes)
  (setf (hunchentoot:content-type*) "text/html")
  (bartleby:add-employee (bartleby::make-employee fname lname phone email address (parse-integer hourly-rate) (bartleby::absorb-services services) notes))
  (browse-employees))
