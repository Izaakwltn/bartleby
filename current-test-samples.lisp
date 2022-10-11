;;;; current-test-samples.lisp
;;;;
;;;;

(in-package :bartleby)

;;; adding weekly appointments

(weekly (make-appointment 1 3 1 (timestamp (date 4 6 2022) (set-time 17 00)) 30 "violin"))

(weekly (make-appointment 1 3 1 (timestamp (date 4 6 2022) (set-time 17 30)) 15 "makeup"))

(weekly (make-appointment 2 3 1 (timestamp (date 4 6 2022) (set-time 10 30)) 30 "violin"))

(weekly (make-appointment 2 3 1 (timestamp (date 4 6 2022) (set-time 11 00)) 15 "makeup"))

;;;;
;;;;
;;;Because of August mishap:

(weekly (make-appointment 1 3 1 (timestamp (date 8 31 2022) (set-time 17 00)) 30 "violin"))

(weekly (make-appointment 1 3 1 (timestamp (date 8 31 2022) (set-time 17 30)) 15 "makeup"))
;;;;initializing their previous makeups

(new-makeup 1 105 (sql-print (timestamp (date 3 30 2022) (set-time 12 00))))

(new-makeup 2 75 (sql-print (timestamp (date 3 30 2022) (set-time 12 00))))


;;; for fast checking

(total-makeup-minutes (client-id-search 2))

(total-makeup-minutes (client-id-search 1))


;;; testing an invoice

(pdf-invoice (make-invoice (employee-id-search 3) 4 2022))

(pdf-invoice (make-invoice (employee-id-search 3) 5 2022))

(pdf-invoice (make-invoice (employee-id-search 3) 6 2022))

(pdf-invoice (make-invoice (employee-id-search 3) 7 2022))

(pdf-invoice (make-invoice (employee-id-search 3) 8 2022))

(pdf-invoice (make-invoice (employee-id-search 3) 9 2022))

(pdf-invoice (make-invoice (employee-id-search 3) 10 2022))
