;;;; web-bartleby/www/search.lisp
;;;;
;;;; Copyright (c) 2022 Izaak Walton

(in-package :webbartleby)

(defgeneric search-format (object)
  (:documentation "Returns a convenient searchable format"))

(defmethod search-format ((client bartleby::client))
  (let ((form-id (concatenate 'string
                              "view-"
                              (write-to-string
                               (mito:object-id client)))))
    (spinneret:with-html
      (:p (:form :id form-id :action "/view-client"
                     (:input :type "hidden" :form form-id :id "client-id" :name "client-id" :value (mito:object-id client))
                     (:button :type "submit" :form form-id
                              (format nil "~a ~a | ~a | ~a"
                                      (bartleby::client-first-name client)
                                      (bartleby::client-last-name client)
                                      (bartleby::client-phone client)
                                      (bartleby::client-email client))))))))

(defmethod search-format ((employee bartleby::employee))
  (let ((form-id (concatenate 'string
                              "view-"
                              (write-to-string
                               (mito:object-id employee)))))
    (spinneret:with-html
      (:p (:form :id form-id :action "/view-employee"
                       (:input :type "hidden" :form form-id :id "employee-id" :name "employee-id" :value (mito:object-id employee))
                       (:button :type "submit" :form form-id
                                (format nil "~a ~a | ~a | ~a"
                                        (bartleby::employee-first-name employee)
                                        (bartleby::employee-last-name employee)
                                        (bartleby::employee-phone employee)
                                        (bartleby::employee-email employee))))))))

;;;
;;; Actual searching:
;;;


(hunchentoot:define-easy-handler (search-results :uri "/search-results") (query)
  (with-page (:title "Search Results")
    (spinneret:with-html
      (:h1 (format nil "Search Results for '~a'" query))
       (loop :for i :in (bartleby::bart-search query)
             :do (search-format i)))))

