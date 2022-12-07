;;;; browse-rooms.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :webbartleby)

(hunchentoot:define-easy-handler (browse-rooms :uri "/browse-rooms") ()
  (with-page (:title "All Rooms")
    (:h1 "All Rooms")
    (cl-bootstrap:bs-table
      (:thead
       (:tr
	(:th "Room Number") (:th "Name") (:th "Capacity") (:th "Notes") (:th "Edit")))
      (:tbody
       (spinneret:with-html (loop :for r :in (bartleby::all-rooms)
	     :do (:tr (:td (write-to-string (mito:object-id r)))
		      (:td (bartleby::room-name r))
		      (:td (write-to-string (bartleby::room-capacity r)))
		      (:td (:a :href (concatenate 'string "/edit-room" (write-to-string
									  (mito:object-id r)))
			       "edit")))))))))
