;;;;rooms.lisp
;;;;

(in-package :schedulizer)

;;;;------------------------------------------------------------------------
;;;;Room Class
;;;;------------------------------------------------------------------------

(defclass meeting-room ()
  ((room-num   :initarg :room-num
	       :accessor room-num)
   (room-name  :initarg :room-name
	       :accessor room-name)
   (capacity   :initarg :capacity
	       :accessor capacity)
   (notes      :initarg :notes
	       :accessor notes)))

(defmethod print-object ((obj meeting-room) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((room-num room-num)
		     (room-name room-name)
		     (capacity capacity)
		     (notes notes))
	obj
      (format stream "~%Room ~a, ~a~%Capacity: ~a~%Notes: ~a~%"
	      room-num
	      room-name
	      capacity
	      notes))))

(defun make-room (room-num room-name capacity notes)
  (make-instance 'meeting-room :room-num  room-num
		               :room-name room-name
			       :capacity  capacity
		               :notes     notes))

;;;;------------------------------------------------------------------------
;;;;Adding to, removing from, and editing *clients*
;;;;------------------------------------------------------------------------

(defvar *rooms* nil)

(defvar *room-backup* nil) ; do the backup stuff on each other class
;move to backups.lisp
;(defun make-backup (filename list)
 ; "Make a backup for the list in the named file.")
  ;to add or remove items from the backup, for now, just rewrite the file with the updated list
   
(defun update-room-backup ()
  "later man")

(defmethod add-backup-room ((meeting-room meeting-room) filename)
  "Backs up the room to the room backup.")

(defmethod remove-backup-room ((meeting-room meeting-room) filename)
  "Removes, somehow, the room from the backup.")

(defmethod add-room ((meeting-room meeting-room))
  "Adds a meeting room to *rooms*"
  (if *room-backup*
      (add-backup-room meeting-room *room-backup*))
  (push meeting-room *rooms*))

(defmethod remove-room ((meeting-room meeting-room))
  "Removes a room from *rooms*"
  (remove-if #'(lambda (r)
		 (equal (room-num r) (room-num meeting-room)))
	     *rooms*))

(defmethod replace-room ((meeting-room meeting-room)
			 room-name capacity notes)
  "Removes room, adds a replacement room."
  (remove-room meeting-room)
  (add-room (make-room (room-num meeting-room)
		       room-name
		       capacity
		       notes)))

;;;;Editing one attribute at a time
(defmethod change-id ((meeting-room meeting-room) new-number)
  (replace-room meeting-room (make-room new-number
					(room-name meeting-room)
				        (capacity meeting-room)
					(notes meeting-room))))

(defmethod change-name ((meeting-room meeting-room) new-name)
  (replace-room meeting-room (make-room (room-num meeting-room)
					new-name
				        (capacity meeting-room)
					(notes meeting-room))))

(defmethod change-capacity ((meeting-room meeting-room) new-capacity)
  (replace-room meeting-room (make-room (room-num meeting-room)
					(room-name meeting-room)
					new-capacity
					(notes meeting-room))))

(defmethod change-notes((meeting-room meeting-room) new-notes)
  (replace-room meeting-room (make-room (room-num meeting-room)
					(room-name meeting-room)
					(capacity meeting-room)
					new-notes)))
;;;;------------------------------------------------------------------------
;;;;Adding New Rooms
;;;;------------------------------------------------------------------------
(defvar last-room-num (if (first *rooms*)
			  (+ (room-num (first *rooms*)) 1)
			  1))

(defun new-room-num ()
  "Increments the last room number."
  (setq last-room-num (+ last-room-num 1))
  last-room-num)

(defun new-room (room-name capacity notes)
  "Generates a new room with a new room number."
  (add-room (make-room (new-room-num) room-name capacity notes)))



;;;;------------------------------------------------------------------------
;;;;Searching for rooms
;;;;------------------------------------------------------------------------

(defun room-search (room-num)
  "Searches for rooms by room number."
  (loop :for r :in *rooms*
	:if (equal room-num (room-num r))
	  :do (return r)))

(add-room (make-room 0 "Virtual" 1000 "Default, Virtual lesson space."))
