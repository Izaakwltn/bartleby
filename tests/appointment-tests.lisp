;;;;appointment-tests.lisp

(in-package :bartleby)


;;;;------------------------------------------------------------------------
;;;;Generating Test Appointments 
;;;;------------------------------------------------------------------------

(defun random-client ()
  (nth (random (length *clients*)) *clients*))

(defun random-employee ()
  (nth (random (- (length *employees*) 1)) *employees*))

;(defun random-future-date ()
 ; (

;(defun appointment-tests (number-of-appointments)
 ; (loop :for i :from 1 :to number-of-appointments
;	:do (recurring (make-appointment (random-client) (random-employee) 
;
