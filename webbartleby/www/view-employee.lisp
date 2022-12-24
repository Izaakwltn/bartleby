;;;; webbartleby/www/view-employee.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :webbartleby)

(hunchentoot:define-easy-handler (view-employee :uri "/view-employee") (employee-id)
  (let ((employee (bartleby::employee-id-search (parse-integer employee-id))))
    (with-page (:title (format nil "Employee ~a" employee-id))
      (spinneret:with-html
        (:h1 (format nil "Employee: ~a ~a"
                     (bartleby::employee-first-name employee)
                     (bartleby::employee-last-name employee)))
        (:h3 (format nil "Phone: ~a" (bartleby::employee-phone employee)))
        (:h3 (format nil "Email: ~a" (bartleby::employee-email employee)))
        (:h3 (format nil "Phone: ~a" (bartleby::employee-address employee)))
        (:h3 (format nil "Services: ~{~a ~}" (bartleby::return-services employee)))))))
        ;(:h3 (format nil "Upcoming Appointments:"))))))
        ;(loop :for a :in (bartleby::future-appointments (bartleby::appointments employee))
         ;     :do (:p (let ((c (bartleby::client-id-search
          ;                      (bartleby::appointment-client-id a)))
           ;                 (ts (bartleby::timestamp-from-sql
            ;                     (write-to-string (bartleby::appointment-timestamp a)))))
             ;           (format nil "~a ~a Employee: ~a ~a"
              ;                   (bartleby::pretty-print (bartleby::date-o ts))
                  ;               (bartleby::pretty-print (bartleby::time-o ts))
               ;;                 (bartleby::client-first-name c)
                 ;               (bartleby::client-last-name c)))))))))
