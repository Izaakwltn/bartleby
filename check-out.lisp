;;;; check-out.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :bartleby)

;;; Functions for Checking out Appointments

(defmethod check-out ((appointment appointment) attendance makeup notes)
  (add-receipt (make-receipt appointment attendance makeup notes))
  (remove-appointment appointment))

;;;;in lieu of the proper web-service:

(defun prompt-read (prompt)
  (format *query-io*)
  (force-output *query-io*)
  (read-line *query-io*))

;(defmethod check-it-out ((appointment appointment))
 ; (check-out appointment
  ;           (prompt-read "Attendance: Arrived, 
(defgeneric receipts (object)
  (:documentation "returns all receipts associated with the given object"))



