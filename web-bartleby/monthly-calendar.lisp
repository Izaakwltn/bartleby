;;;; web-bartleby/monthly-calendar.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :web-bartleby)

(defmethod monthly-calendar-day ((date bartleby::date))
  (format nil "~a   ~a"
          (bartleby::d date)
          (if (bartleby::appointments date)
              (concatenate 'string
                           "("
                           (write-to-string (length (bartleby::appointments date)))
                           ")")
              " ")))
   
(hunchentoot:define-easy-handler (monthly-calendar :uri "/monthly-calendar") (&optional select-date)
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
                                                   (if select-date
                                                       select-date
                                                       (bartleby::today))))
                                  :do (:tr (loop :for d :in (bartleby::days w)
                                                 :do (:td (monthly-calendar-day d))))))))))

