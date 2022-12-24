;;;; daily-calendar.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :webbartleby)
                                        ; daily calendar should show a list of appointments
                                        ; in chronological order
                                        ; rather than some sort of time grid

                                        ; appointment boxes that let you click for more information,
                                        ; maybe with links to view the client or employee or room
                                        ;


(hunchentoot:define-easy-handler (daily-calendar :uri "/daily-calendar") (date)
  (let ((select-date (if date (bartleby::date-from-sql date) (bartleby::today))))
    (with-page (:title "Daily Calendar")
      (:h1 "Daily Calendar")
      (cl-bootstrap:bs-table
	(:thead
	 (:tr
	  (:th select-date)))
	(:tbody (spinneret:with-html
		  (loop :for a :in (bartleby::appointments select-date)
			:do (browsable-appointment a))))))))

(defgeneric available-timeslots (object)
  (:documentation "Provides all available timeslots for the object"))

(defun unavailable-timeslots (date object)
  "Provides a list of all appointments for the object on this date."
  (intersection (appointments date) (appointments object)))

;(defmethod available-timeslots ((date date) object)
 ; (
