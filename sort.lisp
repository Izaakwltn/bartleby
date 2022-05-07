;;;;sort.lisp
;;;;

(in-package :bartleby)

;;;;------------------------------------------------------------------------
;;;;
;;;;------------------------------------------------------------------------

(defun first-name-sort (object-list)
  (sort (copy-list object-list) #'(lambda (o1 o2)
				    (string< (first-name o1) (first-name o2)))))

(defun last-name-sort (object-list)
  (sort (copy-list object-list) #'(lambda (o1 o2)
				    (string< (last-name o1) (last-name o2)))))

(defun id-sort (object-list)
  (sort (copy-list object-list) #'(lambda (o1 o2)
				    (< (id o1) (id o2)))))

;;;;maybe date-time/date/time sorting
(defun date-time-sort (object-list)
  (sort (copy-list object-list) #'(lambda (o1 o2)
				    (later-date-time-p (dt o1) (dt 02)))))
