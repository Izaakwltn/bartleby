;;;; package.lisp
;;;;
;;;; Copyright (c) Izaak Walton 2022

(defpackage #:bartleby
  (:use #:cl)

  ;;; sql.lisp
  (:export #:connect-postgres)
  ;(:export #:bartleby-connect)
  
  ;;; system-generics.lisp
  (:export #:sql-print
           #:pretty-print

           ;;; altering sql objects
	   #:change-first-name
	   #:change-last-name
	   #:change-id
	   #:change-credits
	   #:change-phone
	   #:change-email
	   #:change-address
	   #:change-notes
	   #:change-hourly
	   #:change-capacity
	   #:change-name
	   #:change-date
	   #:change-time
	   #:change-timestamp)

  ;;; time.lisp
  (:export #:set-time ; class and function
	   #:add-time
	   #:current-time
	   #:later-time-p
	   #:later-time
	   #:equal-time
	   #:time-conflict-p
           #:tz ;timezone
           #:*tz-offset*
           #:*tz-offset-minutes*)
  
  ;;; dates.lisp
  (:export #:calendar-month ; type
           #:calendar-year ; type
           #:valid-day-of-week ; type
   #:date ;class and function
	   #:date-from-sql
	   #:later-date-p
	   #:later-date
	   #:equal-date
	   #:month-days
	   #:add-days
	   #:sub-days
	   #:next-year
           #:month-name
	   #:day-of-week
	   #:day-of-week-name
	   #:number-suffix
	   #:today
	   #:next-day
	   #:tomorrow

	   #:week ;class and method
	   #:this-week
	   #:last-week
	   #:next-week

	   #:month ;class and method
	   #:this-month
	   #:last-month
	   #:next-month
           #:gather-weeks)

  ;;; timestamps.lisp
  (:export #:timestamp ; class and function
           #:parse-timestamp
           #:timestamp-from-sql
           #:current-timestamp
           #:moment
           #:change-date
           #:change-time
           #:later-timestamp-p
           #:later-timestamp
           #:future-p
           #:previous-day)
  
  ;;; contact.lisp
  (:export #:phone-number
	   #:make-phone-number

	   #:email-address
	   #:parse-email
	   #:make-email)

  ;;; clients.lisp
  (:export #:client
	   #:make-client
	   #:add-client
	   #:remove-client
	   #:replace-client

	   #:client-count
	   #:client-id-search
	   #:all-clients
	   #:client-last-name-search
	   #:client-first-name-search)

  ;;; rooms.lisp
  (:export #:meeting-room
	   #:make-room
	   #:add-room
	   #:remove-room
	   #:replace-room

	   #:room-count
	   #:room-id-search
	   #:all-rooms)
  
  ;;; employees.lisp
  (:export #:employee
	   #:make-employee
	   
	   #:add-employee
	   #:new-employee
	   #:*standard-hourly*
	   #:remove-employee
	   #:replace-employee

	   #:employee-count
	   #:employee-id-search
	   #:all-employees) ;;;;move to search.lisp
  
  ;;; appointments.lisp
  (:export #:appointment
	   #:make-appointment
	   #:add-appointment
	   #:remove-appointment
	   #:replace-appointment
	   
	   #:copy-new-timestamp
	   #:recurring
	   #:weekly
	   #:monthly
	   #:yearly

	   #:appointment-count
	   #:appointment-id-search
           #:all-appointments
	   #:all-future-appointments
	   #:all-past-appointments)
	   
  ;;; availability.lisp
  (:export #:appointments
	   #:block-off
	   #:overlap-p
	   #:available-p
	   #:availability-cycle
	   #:available-slots)

  ;;; receipts.lisp
  (:export #:*receipts*
	   #:receipt
	   #:make-receipt

	   #:add-receipt
	   #:remove-receipt
	   #:refresh-receipt-backup

	   #:last-receipt-id
	   #:new-receipt

	   #:past-p
	   #:ready-appointments
	   #:month-appointments)

  ;;; invoices.lisp
  (:export #:invoice
	   #:draft-invoice
	   
	   #:invoice-total
	   #:month-receipts
	   #:chronological-receipts

	   #:print-invoice
	   #:invoice-it)

  ;;; test-populations.lisp
  (:export #:generate-clients
	   #:generate-employees
	   #:generate-rooms))
	   ;#:generate-appointments)
  

					;receipts.lisp
 
;invoices.lisp
