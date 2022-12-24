;;;; web-bartleby/browse-employees.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :webbartleby)

(defmethod browsable-employee ((employee bartleby::employee))
  (let ((form-id (concatenate 'string
                              "view-"
                              (write-to-string
                               (mito:object-id employee)))))
    (spinneret:with-html
      (:tr (:td (:form :id form-id :action "/view-employee"
                       (:input :type "hidden" :form form-id :id "employee-id" :name "employee-id" :value (mito:object-id employee))
                       (:button :type "submit" :form form-id (format nil "~a ~a" (bartleby::employee-first-name employee) (bartleby::employee-last-name employee)))))
           (:td (bartleby::employee-phone employee))
           (:td (bartleby::employee-email employee))
           (:td (:a :href (format nil "/edit-employee~a" (mito:object-id employee))
                    "edit"))))))

(hunchentoot:define-easy-handler (browse-employees :uri "/browse-employees") ()
  (with-page (:title "All Employees")
    (:h1 "All Employees")
    (cl-bootstrap:bs-table
      (:thead
       (:tr
	(:th "Name") (:th "Phone") (:th "Email") (:th "Edit")))
      (:tbody
       (loop :for e :in (bartleby::all-employees)
             :do (browsable-employee e))))))
       ;(spinneret:with-html (loop :for e :in (bartleby::all-employees)
	;     :do (:tr (:td (format nil "~a ~a" (bartleby::employee-first-name e)
	;			   (bartleby::employee-last-name e)))
	;	      (:td (bartleby::employee-phone e))
	;	      (:td (bartleby::employee-email e))
	;	      (:td (:a :href (concatenate 'string "/edit-employee" (write-to-string
	;								  (mito:object-id e)))
	;		       "edit")))))))))
