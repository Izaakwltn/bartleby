;;;;employee-tests.lisp
;;;;

(in-package :schedulizer)


;;;;;;;;;;;;;probably move somewhere else

;(defun random-address ()
 ; "2022 Johnson scuba, Denver, Kansas 90000")

(defun generate-employees (number-of-employees)
  (loop :for i :from 1 :to number-of-employees
	:do (let ((fn (random-first))
		  (ln (random-last)))
	      (new-employee fn ln (random-phone) (random-email fn ln) (random-address) 37))))
