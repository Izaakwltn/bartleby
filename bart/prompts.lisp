;;;;prompts.lisp

(in-package :bartleby)

;;;;browse prompt add prompt

(defun browse-prompt ()
  (format nil "What would you like to browse?")
  (format nil "clients~%employees~%appointments~%or rooms?"))
  
