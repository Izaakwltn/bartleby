;;;; web-bartleby/browse-clients.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :web-bartleby)

;;;

(hunchentoot:define-easy-handler (browse-clients :uri "/browse-clients") ()
  (with-page (:title "All Clients")
    (:h1 "All Clients")
    (cl-bootstrap:bs-table
      (:thead
       (:tr
	(:th "Name") (:th "Phone") (:th "Email") (:th "Edit")))
      (:tbody
       (spinneret:with-html (loop :for c :in (bartleby::all-clients)
	     :do (:tr (:td (format nil "~a ~a" (bartleby::client-first-name c)
				   (bartleby::client-last-name c)))
		      (:td (bartleby::client-phone c))
		      (:td (bartleby::client-email c))
		      (:td (:a :href (concatenate 'string "/edit-client" (write-to-string
									  (mito:object-id c)))
			       "edit")))))))))
