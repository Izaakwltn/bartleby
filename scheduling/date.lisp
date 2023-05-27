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
  )
