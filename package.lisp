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
	   #:change-date
	   #:change-time
	   #:change-date-time
	   #:backup-unit)

  ;backup.lisp
  (:export #:backup-unit
	   #:make-backup
	   #:blank-backup
	   #:load-saved-item)
	   
  ;time.lisp
  (:export #:set-time 
	   #:add-time
	   #:current-time
	   #:later-time-p
	   #:equal-time
	   #:time-conflict-p)
  
  ;dates.lisp
  (:export #:date ;class and function
	   #:later-date-p
	   #:equal-date
	   #:month-days
	   #:add-days
	   #:sub-days
	   #:next-year
	   #:day-of-week
	   #:day-of-week-name
	   #:number-suffix
	   #:today
	   #:next-day
	   #:tomorrow

	   #:date-time
	   #:current-date-time
	   #:moment
	   #:later-date-time-p

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
	   #:last-client-id ;var
	   #:new-client
	   #:refresh-client-backup
	   #:update-last-client-id

	   #:change-credits
	   #:add-credits
	   #:use-credits

	   
	   #:clients-with-credits ;;;;these will probably get moved to search.lisp
	   #:client-id-search     ;;;;in search they will return any client or employee
	   #:last-name-search
	   #:first-name-search
	   #:full-name-search)

  ;rooms.lisp
  (:export #:meeting-room
	   #:make-room
	   #:*rooms*
	   #:add-room
	   #:remove-room
	   #:replace-room
	   #:last-room-num ;var
	   #:new-room
	   #:refresh-room-backup
	   #:update-last-room-num

	   #:room-search ;;;;move to search.lisp

	   #:*room-names* ;;;;move to room-tests.lisp
	   #:random-room
	   #:generate-rooms)
  
  ;employees.lisp
  (:export #:employee
	   #:make-employee
	   #:*employees*
	   #:add-employee
	   #:remove-employee
	   #:replace-employee
	   #:refresh-employee-backup
	   #:last-employee-id
	   #:new-employee
	   
	   #:employee-id-search) ;;;;move to search.lisp
  
  ;appointments.lisp
  (:export #:appointment
	   #:printable-people
	   #:make-appointment
	   #:*appointments*
	   #:add-appointment
	   #:remove-appointment
	   #:replace-appointment
	   #:new-appointment
	   #:refresh-appointment-backup

	   #:recurring
	   #:weekly
	   #:monthly
	   #:yearly
	   
	   #:appointment-id-search ;;;;move to search.lisp
	   )
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

	   #:add-receipt
	   #:remove-receipt
	   #:refresh-receipt-backup

	   #:last-receipt-id
	   #:new-receipt

	   #:past-p
	   #:ready-appointments
	   #:month-appointments)

  ;invoices.lisp
  (:export #:invoice
	   #:draft-invoice
	   
	   #:invoice-total
	   #:month-receipts
	   #:chronological-receipts

	   #:print-invoice
	   #:invoice-it)

  (:export #:generate-clients
	   #:generate-employees))
	   ;#:generate-rooms
	   ;#:generate-appointments)
  

					;receipts.lisp
 
;invoices.lisp
