;;;;package.lisp

(defpackage #:bartleby
  (:use #:cl)

  ;system-generics.lisp
  (:export #:sort-by-first-name
	   #:sort-by-last-name
	   #:sort-by-id
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
	   #:backup-unit)

  ;time.lisp
  (:export #:set-time ;;class and function
	   #:add-time
	   #:current-time
	   #:later-time
	   #:equal-time
	   #:time-conflict-p)
  
  ;dates.lisp
  (:export #:date ;class and function
	   #:later-date
	   #:equal-date
	   #:month-days
	   #:add-days
	   #:sub-days
	   #:leap-year-p
	   #:day-nth
	   #:day-of-week
	   #:number-suffix
	   #:today

	   #:week ;class and method
	   #:this-week
	   #:last-week
	   #:next-week

	   #:month ;class and method
	   #:this-month
	   #:last-month
	   #:next-month)

  ;contact.lisp
  (:export #:address
	   #:make-address
	   #:random-address
	   
	   #:phone-number
	   #:make-phone-number
	   #:random-phone

	   #:email-address
	   #:parse-email
	   #:make-email
	   #:auto-email)

  ;clients.lisp
  (:export #:client
	   #:make-client
	   #:*clients*
	   #:add-client
	   #:remove-client
	   #:replace-client
	   #:new-client
	   #:refresh-client-backup

	   #:clients-with-credits ;;;;these will probably get moved to search.lisp
	   #:client-id-search
	   #:last-name-search
	   #:first-name-search)

  ;rooms.lisp
  (:export #:meeting-room
	   #:make-room
	   #:*rooms*
	   #:add-room
	   #:remove-room
	   #:replace-room
	   #:new-room
	   #:refresh-room-backup

	   #:room-search)
  
  ;employees.lisp
  (:export #:employee
	   #:make-employee
	   #:add-employee
	   #:remove-employee
	   #:replace-employee
	   #:refresh-employee-backup
	   #:new-employee
	   
	   #:employee-search)
  
  ;appointments.lisp
  (:export #:appointment
	   #:make-appointment
	   ;#:equal-appointments-p
	   #:*appointments*
	   #:add-appointment
	   #:remove-appointment
	   #:replace-appointment
	   #:new-appointment
	   #:refresh-appointment-backup
	   
	   #:recurring)
	   ;#:past-p)

  ;availability.lisp
  (:export #:appointments
	   #:block-off
	   #:overlap-p
	   #:available-p
	   #:availability-cycle
	   #:available-slots)

  ;receipts.lisp
  (:export #:*receipts*
	   #:receipt
	   #:make-receipt

	   #:ready-appointments
	   #:month-appointments
	   #:check-out-appointment
	   #:check-out)

  ;invoices.lisp
  (:export #:invoice
	   #:draft-invoice
	   
	   #:invoice-total
	   #:month-receipts
	   #:chronological-receipts

	   #:print-invoice)

  (:export #:generate-clients
	   #:generate-employees))
	   ;#:generate-rooms
	   ;#:generate-appointments)
  

					;receipts.lisp
 
;invoices.lisp
