;;;; weekly-calendar.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :web-bartleby) ; weekly calendar should be limited by availability?


(defmethod week-day-pairing ((week bartleby::week))
  (spinneret:with-html
    (loop :for d :in (bartleby::days week)
          :do (:th (format nil "~a ~a"
                           (bartleby::day-of-week-name d)
                           (bartleby::d d))))))

(defgeneric time-it (object)
  (:documentation "Adds time-legend while also including relevant appointments"))

(defmethod readable-time ((set-time bartleby::set-time))
  (let ((hour (bartleby::hour set-time))
        (minutes (bartleby::minutes set-time)))
    (format nil "~a:~a ~a"
            (if (> hour 12) (- hour 12) hour)
            (if (equal 1 (length (write-to-string minutes)))
	        (concatenate 'string "0" (write-to-string minutes))
                minutes)
            (if (and (< hour 24) (>= hour 12)) "pm" "am"))))

(defmethod readable-date ((date bartleby::date))
  (let ((m (bartleby::m date))
        (d (bartleby::d date))
        (y (bartleby::y date)))
    (format nil "~a, ~a ~a~a, ~a"
	    (second (assoc (bartleby::day-of-week (bartleby::date m d y)) bartleby::days-of-week))
	    (second (assoc m bartleby::month-names))
            d
	    (bartleby::number-suffix d)
	    y)))

(defun fifteen-minutes ()
  (reverse (loop :with tm := (bartleby::set-time 24 00)
        :with tms := nil

        :for i :from 1 :to 96
        :do (setq tms (cons tm tms))
        :do (setq tm (bartleby::add-time tm 15))
        :finally (return tms))))

(defmethod time-it ((week bartleby::week))
  (spinneret:with-html
    (:tbody
     (loop :for m :in (mapcar #'readable-time (fifteen-minutes))
           :do (:tr (:td m
                    (loop :for a :from 1 :to 7
                          :do (:td (if (> (random 30) 27)
                                       "appointment"
                                       "")))))))))

(hunchentoot:define-easy-handler (weekly-calendar :uri "/weekly-calendar") (&optional date)
  (let ((select-date (if date date (bartleby::today))))
    (with-page (:title  "Weekly Calendar")
      (spinneret::with-html (:h1 (format nil "Week of ~a" (readable-date (bartleby::most-recent-sunday select-date)))))          
      ;(:p (:a :href (weekly-calendar (bartleby::add-days select-date 7) "Next Week")))
      (let ((w (bartleby::week select-date)))
        (cl-bootstrap:bs-table
          (:thead
           (:tr
            (:th "time")
            (week-day-pairing w)))
          (time-it w))))))

