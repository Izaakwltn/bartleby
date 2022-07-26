;;;; web-bartleby/new-employee.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :web-bartleby)

(hunchentoot:define-easy-handler (new-room-form :uri "/new-room") ()
  (with-page (:title "New Room")
    (:h1 "New Room")
    (:form :action "/submit-new-room"
     (:label :for "num"
	     "Room Number")
     (:input :type "text" :id "num" :name "num")
     (:label :for "name"
	     "Room Name")
     (:input :type "text" :id "name" :name "name")
     (:hr)
     (:label :for "capacity"
	     "Capacity ")
     (:input :type "text" :id "capacity" :name "capacity")
     (:hr)
     (:label :for "notes"
	     "Notes ")
     (:input :type "text" :id "notes" :name "notes")
	          (cl-bootstrap:bs-form-checkbox "Check me out")
		  (:button :type "submit" :class "btn btn-default" "Submit"))))

(hunchentoot:define-easy-handler (submit-new-room :uri "/submit-new-room")
  (num name capacity notes)
  (setf (hunchentoot:content-type*) "text/html")
  (bartleby:add-room (bartleby::make-room num name (parse-integer capacity) notes))
  (browse-rooms))
