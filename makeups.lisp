;;;; makeups.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :bartleby)

(mito:deftable makeup-credit ()
  ((client-id  :col-type (:int))
   (duration   :col-type (:int))
   (timestamp  :col-type (:timestamp))
   (expiration :col-type (:timestamp))
   (active     :col-type (:boolean)))
  (:conc-name makeup-))

(mito:ensure-table-exists 'makeup-credit)

(defmethod print-object ((obj makeup-credit) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((client-id makeup-client-id)
                     (duration  makeup-duration)
                     (timestamp makeup-timestamp)
                     (expiration makeup-expiration)
                     (active     makeup-active))
        obj
      (format stream "~%~a~%~a~%~a minutes~%Granted:~%~a~%Expires:~%~a~%~%"
              (if active "PASSIVE" "ACTIVE")
              (client-id-search client-id)
              duration
              (timestamp-from-sql (write-to-string timestamp))
              (timestamp-from-sql (write-to-string expiration))))))

(defun make-makeup (client-id duration sql-timestamp expiration &optional active-status)
  (make-instance 'makeup-credit :client-id  client-id
                                :duration   duration
                                :timestamp  sql-timestamp
                                :expiration expiration
                                :active     (if active-status
                                                active-status
                                                1)))

(defmethod add-makeup ((makeup-credit makeup-credit))
  (mito:insert-dao makeup-credit))

(defvar *default-makeup-expiration* 180)

(defun new-makeup (client-id duration sql-timestamp &optional expiration)
  (add-makeup (make-makeup client-id
                           duration
                           sql-timestamp
                           (if expiration
                               expiration
                               (sql-print
                                (add-days
                                 (timestamp-from-sql (write-to-string sql-timestamp))
                                 *default-makeup-expiration*))))))

(defmethod new-counter-makeup ((makeup-credit makeup-credit) amount-used)
  (add-makeup (make-makeup (makeup-client-id makeup-credit)
                           (- amount-used)
                           (makeup-timestamp makeup-credit)
                           (makeup-expiration makeup-credit)
                           0)))
              
(defmethod remove-makeup ((makeup-credit makeup-credit))
  (mito:delete-dao makeup-credit))

(defmethod replace-makeup ((makeup-credit makeup-credit) new-makeup)
  (remove-makeup makeup-credit)
  (add-makeup makeup-credit))

(defmethod credit-appointment ((appointment appointment))
  (new-makeup (appointment-client-id appointment)
              (appointment-duration  appointment)
              (appointment-timestamp appointment)))

(defmethod passive-partial-makeup ((makeup-credit makeup-credit) amount-used)
  (add-makeup (make-makeup (makeup-client-id makeup-credit)
                           amount-used
                           (makeup-timestamp makeup-credit)
                           (makeup-expiration makeup-credit)
                           0)))

(defmethod active-partial-makeup ((makeup-credit makeup-credit) amount-used)
  (add-makeup (make-makeup (makeup-client-id makeup-credit)
                           (- (makeup-duration makeup-credit) amount-used)
                           (makeup-timestamp makeup-credit)
                           (makeup-expiration makeup-credit)
                           1)))

(defmethod partial-use ((makeup-credit makeup-credit) amount-used)
  "Splits a makeup-credit into active and passive pieces, generates counter-credit for passive piece"
  (progn (new-counter-makeup makeup-credit amount-used)
         (passive-partial-makeup makeup-credit amount-used)
         (active-partial-makeup makeup-credit amount-used)
         (remove-makeup makeup-credit)))
  ;(setf (slot-value makeup-credit 'duration) (- (makeup-duration makeup-credit) amount-used))
 ; (mito:save-dao makeup-credit))

(defmethod deactivate-makeup ((makeup-credit makeup-credit))
  (setf (slot-value makeup-credit 'active) 0)
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

(defun all-active-makeups ()
  (remove-if-not #'(lambda (i)
                     (makeup-active i))
                 (all-makeups)))

            ;(eq (makeup-active (makeup-id-search i))
                                      
(defun makeups-by-date ()
  (sort (all-makeups) #'(lambda (x y)
                          (later-timestamp-p
                           (timestamp-from-sql (write-to-string (makeup-timestamp y)))
                           (timestamp-from-sql (write-to-string (makeup-timestamp x)))))))

(defmethod makeups ((client client))
  (remove-if-not #'(lambda (i)
                     (eq (makeup-client-id i) (mito:object-id client)))
                 (makeups-by-date)))

(defmethod active-makeups ((client client))
  (remove-if-not #'(lambda (i)
                     (makeup-active i))
                 (makeups client)))

(defmethod use-makeups ((client client) amount-used)
  (loop :with amount-to-use := amount-used

        :for m :in (active-makeups client)
        :do (cond ((zerop amount-to-use)
                   (return (format nil "Total Makeup-minutes ~a" (total-makeup-minutes client))))
                  ((> amount-to-use (makeup-duration m))
                   (progn (setq amount-to-use (- amount-to-use (makeup-duration m)))
                         (deactivate-makeup m)))
                  (t (progn (partial-use m amount-to-use)
                            (setq amount-to-use 0))))))

;(defmethod use-makeups ((client client) amount-to-use)
;;  (cond ((zerop amount-to-use) (total-makeup-minutes client))
 ;       ((> amount-to-use (makeup-duration 

(defmethod total-positive-minutes ((client client))
  (reduce #'+ (mapcar #'makeup-duration
                      (remove-if-not #'(lambda (m)
                                         (> (makeup-duration m) 0))
                                     (makeups client)))))

(defmethod total-negative-minutes ((client client))
  (reduce #'+ (mapcar #'makeup-duration (remove-if-not #'(lambda (m)
                                 (< (makeup-duration m) 0))
                             (makeups client)))))
               
(defmethod total-makeup-minutes ((client client))
  (reduce #'+ (mapcar #'makeup-duration (makeups client)))) 


(defmethod makeups-before ((client client) timestamp)
  "Returns all makeups before arbitrary date"
  (remove-if-not #'(lambda (m)
                     (later-timestamp-p timestamp
                                        (timestamp-from-sql
                                         (write-to-string (makeup-timestamp m)))))
                 (makeups client)))

(defmethod past-makeups ((client client))
  (makeups-before client (current-timestamp)))

(defmethod makeup-minutes-before ((client client) timestamp)
  (reduce #'+ (mapcar #'makeup-duration (makeups-before client timestamp))))
                          
                                        ; for calculating month-start and month-end makeup minutes
