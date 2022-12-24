;;;; web-bartleby/www/monthly-calendar.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :webbartleby)

(defmethod monthly-calendar-day ((date bartleby::date))
  (let ((form-id (concatenate 'string
                              "day-"
                              (write-to-string date))))
    (spinneret:with-html
      (:form :id form-id :action "/daily-calendar"
           (:input :type "hidden" :form form-id :id "date" :name "date" :value (bartleby::sql-print date))
           (:button :type "submit" :form form-id
                    (format nil "~a   ~a"
                            (bartleby::d date)
                            (if (bartleby::appointments date)
                                (concatenate 'string
                                             "("
                                             (write-to-string
                                              (length (bartleby::appointments date)))
                                             ")")
                                " ")))))))
           
   
(hunchentoot:define-easy-handler (monthly-calendar :uri "/monthly-calendar") (&optional date)
  (let ((select-date (if date date (bartleby::today))))
    (with-page (:title "Monthly Calendar")
    (:h1 "Monthly Calendar")
    (cl-bootstrap:bs-table
      (:thead
       (:tr
        (:th "Sunday")
        (:th "Monday")
        (:th "Tuesday")
        (:th "Wednesday")
        (:th "Thursday")
        (:th "Friday")
        (:th "Saturday")))
      (:tbody
       (spinneret:with-html
         (loop :for w :in (bartleby::gather-weeks (bartleby::month
                                                   select-date))
                                  :do (:tr (loop :for d :in (bartleby::days w)
                                                 :do (:td (monthly-calendar-day d)))))))))))

