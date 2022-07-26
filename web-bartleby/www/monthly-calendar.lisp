;;;; web-bartleby/monthly-calendar.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :web-bartleby)

(defmethod monthly-calendar-day ((date date))
   
(hunchentoot:define-easy-handler (monthly-calendar :uri "/monthly-calendar") (&optional date)
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
       (spinneret:with-html (loop :for w :in (bartleby::gather-weeks (bartleby::month (if date date (bartleby::today))))
                                  :do (:tr (loop :for d :in (bartleby::days w)
                                                 :do (:td (format nil "~a" (bartleby::d d)))))))))))

