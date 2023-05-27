;;;; scheduling/date.lisp
;;;;
;;;; Copyright Izaak Walton (C) 2022-2023

(in-package #:bartleby-scheduling)

(coalton-toplevel

  (define-type Date
    (Date Integer Integer Integer)) ; Y-M-D

  (define-instance (Eq Date)
    (define (== a b)
      (== (match a
            ((Date y m d) (make-list y m d)))
          (match b
            ((Date y m d) (make-list y m d))))))

  (define-instance (Ord Date)
    (define (<=> a b)
      (let (Date y1 m1 d1) = a)
      (let (Date y2 m2 d2) = b)
      (match (<=> y1 y2)
        ((LT) LT)
        ((GT) GT)
        ((EQ) (match (<=> m1 m2)
                ((LT) LT)
                ((GT) GT)
                ((EQ) (<=> d1 d2)))))))

  ;;;
  ;;; Handling month days
  ;;;

  (declare leap-year-p (Integer -> Boolean))
    (define (leap-year-p year)
    (cond ((not (arith:zero? (mod year 4)))
           False)
          ((not (arith:zero? (mod year 100)))
           True)
          ((not (arith:zero? (mod year 400)))
           False)
          (True True)))

  (define common-year-days (make-list (Tuple 1 31)
                                      (Tuple 2 28)
                                      (Tuple 3  31)
                                      (Tuple 4  30)
                                      (Tuple 5  31)
                                      (Tuple 6  30)
                                      (Tuple 7  31)
                                      (Tuple 8  31)
                                      (Tuple 9  30)
                                      (Tuple 10 31)
                                      (Tuple 11 30)
			              (Tuple 12 31)))

  (define leap-year-days (make-list (Tuple 1 31)
			            (Tuple 2 29)
			            (Tuple 3  31)
			            (Tuple 4  30)
			            (Tuple 5  31)
			            (Tuple 6  30)
		                    (Tuple 7  31)
	     	                    (Tuple 8  31)
      		                    (Tuple 9  30)
		                    (Tuple 10 31)
			            (Tuple 11 30)
			            (Tuple 12 31)))

  (declare month-days (Integer -> Integer -> Integer))
  (define (month-days month year)
    (let ((find-month-days (fn (month days-list)
                        (snd (unwrap (list:find (fn (x)
                                                  (== (fst x) month))
                                                days-list))))))
      (if (leap-year-p year)
          (find-month-days month leap-year-days)
          (find-month-days month common-year-days))))

  ;;;
  ;;; Adding days
  ;;;
  
  (define (add-days this-date days)
    (if (arith:zero? days)
        this-date
        (match this-date
          ((Date y 12 31)
           (add-days (Date (1+ y) 1 1)
                     (1- days)))
          ((Date y m d)
           (if (== d (month-days m y))
               (add-days (Date y (1+ m) 1)
                         (1- days))
               (add-days (Date y m (1+ d))
                         (1- days)))))))

  ;;;
  ;;; Current/relational time
  ;;;;

  (define (today)
    (Lisp Date () (cl:let ((loc (local-time:now)))
                    (Date (local-time:timestamp-year loc)
                          (local-time:timestamp-month loc)
                          (local-time:timestamp-day loc)))))
  (define (tomorrow)
    (add-days (today) 1))

  )
