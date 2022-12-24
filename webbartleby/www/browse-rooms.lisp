;;;; browse-rooms.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :webbartleby)

(defmethod browsable-meeting-room ((meeting-room bartleby::meeting-room))
  (let ((form-id (concatenate 'string
                              "view-"
                              (write-to-string
                               (mito:object-id meeting-room)))))
    (spinneret:with-html
      (:tr (:td (:form :id form-id :action "/view-meeting-room"
                       (:input :type "hidden" :form form-id :id "meeting-room-id" :name "meeting-room-id" :value (mito:object-id meeting-room))
                       (:button :type "submit" :form form-id (format nil "~a" (bartleby::room-num meeting-room)))))
           (:td (bartleby::room-name meeting-room))
           (:td (bartleby::room-capacity meeting-room))
           (:td (bartleby::room-notes meeting-room))
           (:td (:a :href (format nil "/edit-meeting-room~a" (mito:object-id meeting-room))
                    "edit"))))))

(hunchentoot:define-easy-handler (browse-rooms :uri "/browse-rooms") ()
  (with-page (:title "All Rooms")
    (:h1 "All Rooms")
    (cl-bootstrap:bs-table
      (:thead
       (:tr
	(:th "Room Number") (:th "Name") (:th "Capacity") (:th "Notes") (:th "Edit")))
      (:tbody
       (spinneret:with-html (loop :for r :in (bartleby::all-rooms)
                                  :do (browsable-meeting-room r)))))))
       ;(spinneret:with-html (loop :for r :in (bartleby::all-rooms)
	;     :do (:tr (:td (write-to-string (mito:object-id r)))
	;	      (:td (bartleby::room-name r))
	;	      (:td (write-to-string (bartleby::room-capacity r)))
	;	      (:td (:a :href (concatenate 'string "/edit-room" (write-to-string
	;								  (mito:object-id r)))
	;		       "edit")))))))))
