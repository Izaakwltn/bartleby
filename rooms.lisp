;;;; rooms.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :bartleby)

;;; Room Class

(mito:deftable meeting-room ()
  ((num  :col-type (:int))
   (name :col-type (:varchar 64))
   (capacity  :col-type (:int))
   (notes     :col-type (:varchar 128)))
  (:conc-name room-))

(mito:ensure-table-exists 'meeting-room)

(defmethod print-object ((obj meeting-room) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((room-num room-num)
		     (room-name room-name)
		     (room-capacity room-capacity)
		     (room-notes room-notes))
	obj
      (format stream "~%Room ~a, ~a~%Capacity: ~a~%Notes: ~a~%"
	      room-num
	      room-name
	      room-capacity
	      room-notes))))

(defun make-room (num name capacity notes)
  (make-instance 'meeting-room :num       num
		               :name      name
			       :capacity  capacity
		               :notes     notes))

;;; Adding and removing rooms

(defmethod add-room ((meeting-room meeting-room))
  "Adds a meeting room to the meeting_room sql db"
  (mito:insert-dao meeting-room))
  
(defmethod remove-room ((meeting-room meeting-room))
  "Removes a room from *rooms*"
  (mito:delete-dao meeting-room))

(defmethod replace-room ((meeting-room meeting-room) new-meeting-room)
  "Removes room, adds a replacement room."
  (remove-room meeting-room)
  (add-room new-meeting-room))

;;; Editing one attribute at a time

(defmethod change-num ((meeting-room meeting-room) new-number)
  (setf (slot-value meeting-room 'num) new-number)
  (mito:save-dao meeting-room))

(defmethod change-name ((meeting-room meeting-room) new-name)
  (setf (slot-value meeting-room 'name) new-name)
  (mito:save-dao meeting-room))

(defmethod change-capacity ((meeting-room meeting-room) new-capacity)
  (setf (slot-value meeting-room 'capacity) new-capacity)
  (mito:save-dao meeting-room))

(defmethod change-notes((meeting-room meeting-room) new-notes)
  (setf (slot-value meeting-room 'notes) new-notes)
  (mito:save-dao meeting-room))

;;; Searching for rooms

(defun room-count ()
  (mito:count-dao 'meeting-room))

(defun room-id-search (room-id)
  (mito:find-dao 'meeting-room :id room-id))

(defun all-rooms ()
  (loop :for i :from 1 :to (room-count)
        :collect (room-id-search i)))

(defun all-rooms ()
  (loop :with rooms := nil
	:with rc      := (room-count)

	:for i :upfrom 1
	:do (setq rooms (if (null (room-id-search i))
			      rooms
			      (cons (room-id-search i) rooms)))
	:when (equal (length rooms) rc)
	  :do (return rooms)))

(defun room-name-search (name)
  (find-if #'(lambda (r)
	       (string-equal name (room-name r)))
	   (all-rooms)))
	       
