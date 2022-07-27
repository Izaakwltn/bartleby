;;;; daily-calendar.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :web-bartleby)

(hunchentoot:define-easy-handler (daily-calendar :uri "/daily-calendar") (&optional date)
  (let ((select-date (if date date (bartleby::today))))
    (with-page (:title "Daily Calendar")
      (:h1 "Daily Calendar")
      (cl-bootstrap:bs-table
	(:thead
	 (:tr
	  (:th select-date)))
	(:tbody (spinneret:with-html
		  (loop :for a :in (bartleby::appointments select-date)
			:do (:tr (:td (format nil "~a" a))))))))))
