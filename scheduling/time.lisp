;;;; time.lisp
;;;;
;;;; Copyright Izaak Walton (C) 2022-2023

(in-package #:bartleby-scheduling)

(coalton-toplevel
  
  #+ignore (repr :native clock-time) ; probably unecessary
  (define-type Clock-time
    (Clock-time Integer Integer))


  (define-instance (Eq Clock-time)
    (define (== a b)
      (== (match a
            ((Clock-time h m) (make-list h m)))
          (match b
            ((Clock-time h m) (make-list h m))))))

  (define-instance (Ord Clock-time)
    (define (<=> a b)
      (let (Clock-time h1 m1) = a)
      (let (Clock-time h2 m2) = b)
      (match (<=> h1 h2)
        ((LT) LT)
        ((GT) GT)
        ((EQ) (<=> m1 m2)))))

  (declare add-time (Clock-time -> Integer -> Clock-time))
  (define (add-time time minutes)
    (if (arith:zero? minutes)
        time
        (match time
          ((Clock-time 24 59)
           (add-time (Clock-time 1 0) (1- minutes)))
          ((Clock-time h 59)
           (add-time (Clock-time (1+ h) 0) (1- minutes)))
          ((Clock-time h m)
           (add-time (Clock-time h (1+ m)) (1- minutes))))))

  ;;;
  ;;; Handling Local Time and Timezones
  ;;;

  (define (current-time)
    "Returns the current local time."
    (Clock-time
     (lisp Integer () (local-time:timestamp-hour (local-time:now)))
     (lisp Integer () (local-time:timestamp-minute (local-time:now)))))
  
  (define (current-timezone)
    (Lisp (List String) ()
      (cl:multiple-value-list local-time:*default-timezone*)))

  (define (current-timezone-offset)
    (Lisp Integer ()
      (cl:/ (cl:nth 9 (cl:multiple-value-list (local-time:decode-timestamp (local-time:now))))
            3600)))

  (define (current-timezone-offset-minutes)
    (* (current-timezone-offset) 60)))

