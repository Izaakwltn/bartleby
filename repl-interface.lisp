;;;;repl-interface.lisp
;;;;

(in-package :bartleby)

;;;a system for inputing, editing, and managing appointments

;;;welcome to the Schedule-REPL
;;; Would you like to edit an appointment, client, employee, or room?
;;; What would you like to edit?

(defvar *interface-commands* '(("edit" #'interface-edit)
			       ("browse" #'interface-browse)
                               ("add" #'interface-add)))

(defvar *error-response* "I would prefer not to")

(defun browse-prompt ()
  (format nil "What would you like to browse?~%appointments~%clients~%employees~%rooms~%?"))

(defun interface-browse (&optional category)
  "Browses items, if not prompts with categories."
  (if category
      (browse category)
      (browse-prompt)))

(defun main-screen ()
  (format nil "Welcome to Bartleby the Scheduler.")
  (format nil "type \"help\" for help"))

(defun help-sheet ()
  (format nil "Bartleby Help Menu: "))

  ;edit...
   ;     ...clients
    ;    ...employees
   ;    ...appointments
					;rooms
;add...
      ;...client
      ;...appointment
;browse ... clients employees appointments etc
;search.... .....
;

(defgeneric interface-add (object)
  (:documentation "Prompt for adding an object through the interface"))

(defgeneric interface-edit (object))

(defun interface-browse (object-list))

(defgeneric 
;;;search functions
(defvar *search-results* "The current list of search results, for ease of access."
;(defun client-interface ()
  

;(defun interface ()
 ; (prompt-read 

			  
