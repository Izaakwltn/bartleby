;;;; webbartleby/www/view-room.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :webbartleby)

(hunchentoot:define-easy-handler (view-meeting-room :uri "/view-meeting-room") (meeting-room-id)
  (let ((meeting-room (bartleby::room-id-search (parse-integer meeting-room-id))))
    (with-page (:title (format nil "Meeting-Room ~a" meeting-room-id))
      (spinneret:with-html
        (:h1 (format nil "Meeting-Room ~a: ~a"
                     (bartleby::room-num meeting-room)
                     (bartleby::room-name meeting-room)))
        (:h3 (format nil "Capacity: ~a" (bartleby::room-capacity meeting-room)))
        (:h3 (format nil "Services: ~{~a ~}" (bartleby::return-services meeting-room)))
        (:h3 (format nil "Notes: ~a" (bartleby::room-notes meeting-room)))))))
