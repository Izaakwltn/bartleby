;;;; time.lisp
;;;;
;;;; Copyright Izaak Walton (C) 2022-2023

(in-package #:bartleby-scheduling)

(coalton-toplevel
  (repr :native clock-time)
  (define-type Clock-time
    (Clock-time UFix UFix))

  ;(declare make-time (Clock-hour -> Clock-minutes -> Clock-time))
  #+ignore(define (make-time hour minutes)
    (if (not (and (and (>= minutes 0) (< minutes 60))
                  (and (>= hour 0) (< hour 24))))
        (error "Invalid hour:minutes ~a:~a")
        (Clock-time 10 30)))

  (define (valid-hour n)
    (and (> n 0)
         (<= n 24)))

  (define (valid-minutes n)
    (and (>= n 0)
         (< n 60)))
  
  (declare extract (Clock-time -> (Tuple UFix UFix)))
  (define (extract clock-time)
    (match clock-time
      ((Clock-time hour minutes) (Tuple hour minutes))))
      ;(_ (error "improper clock-time"))))

  #+ignore
  (declare add-time (Clock-time -> UFix -> Clock-time))

  (define (add-time time minutes)
    (let t = (extract time))
    (cond
      ((coalton-library/math/arith:zero? minutes) time)
      ((and (== (fst t) 24)
            (== (snd t) 59))
       (add-time (Clock-time 1 0) (1- minutes)))
      ((== (snd t) 59)
       (add-time (Clock-time (1+ (fst t)) 0) (1- minutes)))
      (True (add-time (Clock-time (fst t) (1+ (snd t))) (1- minutes)))))


  )

