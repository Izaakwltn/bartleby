;;;;package.lisp

(defpackage #:schedulizer
  (:use #:cl)

  ;calendar.lisp
  (:export #:date
	   #:later-date
	   #:equal-date
	   #:day-cycle
	   #:leap-year-p
	   #:day-nth
	   #:day-of-week
	   #:today
	   #:set-time
	   #:add-time
	   #:current-time)

  ;clients.lisp
  (:export #:make-client
	   #:new-client
	   #:id-search
	   #:last-name-search
	   #:first-name-search)

  ;appointments.lisp
  (:export #:make-employee
	   #:employee-search
	   #:make-appointment))
