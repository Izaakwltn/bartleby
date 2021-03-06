;;;; check-out.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :bartleby)

;;; Functions for Checking out Appointments

(defmethod check-out ((appointment appointment) attendance notes)
  (add-receipt (make-receipt appointment attendance notes))
  (remove-appointment appointment))

(defgeneric receipts (object)
  (:documentation "returns all receipts associated with the given object"))



