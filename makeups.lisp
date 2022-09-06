;;;; makeups.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :bartleby)

(mito:deftable makeup-credit ()
  ((client-id :col-type (:int))
   (duration  :col-type (:int))
   (timestamp :col-type (:timestamp)))
  (:conc-name makeup-))

(mito:ensure-table-exists 'makeup-credit)

(defmethod print-object ((obj makeup-credit) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((client-id makeup-client-id)
                     (duration  makeup-duration)
                     (timestamp makeup-timestamp))
        obj
      (format stream
              "client ~a ~a ~a"
              (client-id-search client-id)
              duration
              timestamp))))

(defun make-makeup (client-id duration sql-timestamp)
  (make-instance 'makeup-credit :client-id client-id
                                :duration  duration
                                :timestamp sql-timestamp))

(defmethod add-makeup ((makeup-credit makeup-credit))
  (mito:insert-dao makeup-credit))

(defun new-makeup (client-id duration sql-timestamp)
  (add-makeup (make-makeup client-id duration sql-timestamp)))

(defmethod remove-makeup ((makeup-credit makeup-credit))
  (mito:delete-dao makeup-credit))

(defmethod replace-makeup ((makeup-credit makeup-credit) new-makeup)
  (remove-makeup makeup-credit)
  (add-makeup makeup-credit))

(defmethod credit-appointment ((appointment appointment))
  (new-makeup (appointment-client-id appointment)
              (appointment-duration  appointment)
              (appointment-timestamp appointment)))

(defmethod partial-use ((makeup-credit makeup-credit) amount-used)
  (setf (slot-value makeup-credit 'duration) (- (makeup-duration makeup-credit) amount-used))
  (mito:save-dao makeup-credit))

(defun makeup-count ()
  (mito:count-dao 'makeup-credit))

(defun makeup-id-search (makeup-id)
  (mito:find-dao 'makeup-credit :id makeup-id))

(defun all-makeups ()
  (loop :with makeups := nil
	:with mc      := (makeup-count)

	:for i :upfrom 1
	:do (setq makeups (if (null (makeup-id-search i))
			      makeups
			      (cons (makeup-id-search i) makeups)))
	:when (equal (length makeups) mc)
	  :do (return makeups)))

(defun makeups-by-date ()
  (sort (all-makeups) #'(lambda (x y)
                          (later-timestamp-p (timestamp-from-sql (write-to-string y))
                                        (timestamp-from-sql (write-to-string x))))))

(defmethod makeups ((client client))
  (remove-if-not #'(lambda (i)
                     (eq (makeup-client-id i) (mito:object-id client)))
                 (makeups-by-date)))

(defmethod use-makeups ((client client) amount-used)
  (loop :with amount-to-use := amount-used

        :for m :in (makeups client)
        :do (cond ((zerop amount-to-use)
                   (return (total-makeup-minutes client)))
                  ((> amount-to-use (makeup-duration m))
                   (progn (setq amount-to-use (- amount-to-use (makeup-duration m)))
                         (remove-makeup m)))
                  (t (progn (partial-use m amount-to-use)
                            (setq amount-to-use 0))))))

(defmethod total-makeup-minutes ((client client))
  (reduce #'+ (mapcar #'makeup-duration (makeups client)))) 


(defmethod total-makeup-minutes-before ((client client) date)
  "Returns total makeup minutes before arbitrary date") ; for calculating month-start and month-end makeup minutes
