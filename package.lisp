;;;;package.lisp

(defpackage #:schedulizer
  (:use #:cl)

  ;calendar.lisp
  (:export #:date ;class and function
	   #:later-date
	   #:equal-date
	   #:day-cycle
	   #:leap-year-p
	   #:day-nth
	   #:day-of-week
	   #:number-suffix
	   #:today
	   #:set-time
	   #:add-time
	   #:current-time
	   #:later-time
	   #:equal-time
	   #:time-conflict-p)

  ;contact.lisp
  (:export #:address
	   #:make-address
	   #:random-address
	   #:phone-number
					;#:make-phone-number
	   #:random-email
	   #:random-phone)

  ;clients.lisp
  (:export #:client
	   #:make-client
	   #:new-client
	   #:add-client
	   #:id-search
	   #:last-name-search
	   #:first-name-search)

  ;employees.lisp
  (:export #:employee
	   #:make-employee
	   #:add-employee
	   #:new-employee
	   #:employee-search)
  
  ;appointments.lisp
  (:export #:meeting-room
	   #:make-room
	   #:add-room
	   #:room-search
	   #:appointment
	   #:make-appointment
	   #:add-appointment
	   #:recurring
	   #:past-appointment-p))

;receipts.lisp
;invoices.lisp
