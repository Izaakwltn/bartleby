;;;; web-bartleby/view-client.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :web-bartleby)

(hunchentoot:define-easy-handler (view-client :uri "/view-client") (client-id)
  (let ((client (bartleby::client-id-search (parse-integer client-id))))
    (with-page (:title "View Client")
      (spinneret:with-html
        (:h1 (format nil "Client: ~a ~a"
                 (bartleby::client-first-name client)
                 (bartleby::client-last-name client)))
        (:h3 (format nil "Phone: ~a" (bartleby::client-phone client)))
        (:h3 (format nil "Email: ~a" (bartleby::client-email client)))
        (:h3 (format nil "Phone: ~a" (bartleby::client-address client)))
        (:h3 (format nil "Notes: ~a" (bartleby::client-notes client)))
        (:h3 (format nil "Total Makeup Minutes: ~a" (bartleby::total-makeup-minutes client)))
        (:h3 (format nil "Upcoming Appointments:"))
        (loop :for a :in (bartleby::future-appointments (bartleby::appointments client))
              :do (:p (let ((e (bartleby::employee-id-search
                                (bartleby::appointment-employee-id a)))
                            (ts (bartleby::timestamp-from-sql
                                 (write-to-string (bartleby::appointment-timestamp a)))))
                        (format nil "~a ~a Employee: ~a ~a"
                                 (bartleby::pretty-print (bartleby::date-o ts))
                                 (bartleby::pretty-print (bartleby::time-o ts))
                                (bartleby::employee-first-name e)
                                (bartleby::employee-last-name e)))))))))
 ;;;;eventually use the same function as browse appointments
                              
        
