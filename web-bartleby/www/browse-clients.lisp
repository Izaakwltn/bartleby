;;;; web-bartleby/browse-clients.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :web-bartleby)

;;;

(defmethod browsable-client ((client bartleby::client))
  (let ((form-id (concatenate 'string
                              "view-"
                              (write-to-string
                               (mito:object-id client)))))
    (spinneret:with-html
      (:tr (:td (:form :id form-id :action "/view-client"
                     (:input :type "hidden" :form form-id :id "client-id" :name "client-id" :value (mito:object-id client))
                                    (:button :type "submit" :form form-id (format nil "~a ~a" (bartleby::client-first-name client)
				   (bartleby::client-last-name client)))))
		      (:td (bartleby::client-phone client))
		      (:td (bartleby::client-email client))
		      (:td (:a :href (concatenate 'string "/edit-client" (write-to-string
									  (mito:object-id client)))
			       "edit"))))))

(hunchentoot:define-easy-handler (browse-clients :uri "/browse-clients") ()
  (with-page (:title "All Clients")
    (:h1 "All Clients")
    (cl-bootstrap:bs-table
      (:thead
       (:tr
	(:th "Name") (:th "Phone") (:th "Email") (:th "Edit")))
      (:tbody
       (spinneret:with-html
         (loop :for c :in (bartleby::all-clients)
               :do (browsable-client c)))))))


               ;:do (:tr (:td (:form :id "view-client" :action "/view-client"
                ;                    (:input :type "hidden" :form "view-client" :id "client-id" :name "client-id" :val;ue (mito:object-id c))
                  ;                  (:button :type "submit" :form "view-client" :value (mito:object-id c) (format nil ";~a ~a" (bartleby::client-first-name c)
		;		   (bartleby::client-last-name c)))))
		 ;     (:td (bartleby::client-phone c))
		  ;    (:td (bartleby::client-email c))
		   ;   (:td (:a :href (concatenate 'string "/edit-client" (write-to-string
		;							  (mito:object-id c)))
		;	       "edit")))))))))
