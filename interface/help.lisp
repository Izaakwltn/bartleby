;;;;help.lisp
;;;;

(in-package :bartleby)


(defun help ()
  (write-line "---*---*---*---*---*---*---*---*--*--*--")
  (write-line (format nil "Hopefully this will make things a bit easier:~%"))
  (write-line "---*---*---*---*---*---*---*---*--*--*--")
  (write-line (format nil "~%-If you have thought better of this whole process and wish to depart,"))
  (write-line (format nil "~%please type either EXIT or QUIT.~%"))
  (write-line "---*---*---*---*---*---*---*---*--*--*--")
  (write-line "Browsing:")
  (write-line (format nil "~%-You can enter a name of a list: appointments, clients, employees, or rooms ~%"))
  (write-line (format nil "-Or use the command BROWSE along with the name of a list.~%"))
  (write-line (format nil "-If in despair, merely type BROWSE and I will do my best to help.~%"))
  (write-line "---*---*---*---*---*---*---*---*--*--*--")
  (write-line "Adding New Items:")
  (write-line (format nil "~%-Either use NEW with a type of object (client, employee, room, appointment) ~%"))
  (write-line (format nil "-If in despair, merely type NEW and I will do my best to help.~%"))
  (write-line "---*---*---*---*---*---*---*---*--*--*--"))
