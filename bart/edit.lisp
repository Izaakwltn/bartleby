;;;;edit.lisp
;;;;

(in-package :bartleby)

;;;;editing interface

(defvar *edit-commands* '(("first"     #'edit-first-name)
			  ("last"      #'edit-last-name)
			  ("id"        #'edit-id)
			  ("credits"   #'edit-credits)
			  ("phone"     #'edit-phone)
			  ("email"     #'edit-email)
			  ("address"   #'edit-address);;;;maybe add later
			  ("notes"     #'edit-notes)
			  ("hourly"    #'edit-hourly)
			  ("capacity"  #'edit-capacity)
			  ("name"      #'edit-name)
			  ("date"      #'edit-date)
			  ("time"      #'edit-time)
			  ("date-time" #'edit-date-time)))

(defgeneric edit-first-name (object first-name)
  (:documentation "Prompt for editing the first name of an object"))

(defmethod edit-first-name ((client bartleby:client))
  (bartleby:change-first-name client (prompt-read "New First Name")))

(defmethod edit-first-name ((employee bartleby:employee))
  (bartleby:change-first-name employee (prompt-read "New First Name")))

(defmethod edit-last-name ((client bartleby:client))
  (bartleby:change-last-name client (prompt-read "New Last Name")))

(defmethod edit-last-name ((employee bartleby:employee))
  (bartleby:change-last-name employee (prompt-read "New Last Name")))

(defgeneric edit-prompt (object)
  (:documentation "A prompt for editing a given object"))

(defmethod edit-prompt ((client bartleby:client))
  (write-line "What would you like to edit?")
  (funcall
   (second
    (find-if #'(lambda (x)
		 (string-equal (first x)
			       (prompt-read "first, last, id, phone, email, credits, or notes")))
	     *edit-commands))
   client))

(defmethod edit-prompt ((employee bartleby:employee))
  (write-line "What would you like to edit?")
  (funcall
   (second
    (find-if #'(lambda (x)
		 (string-equal (first x)
			       (prompt-read "first, last, id, phone, email, or hourly")))
	     *edit-commands))
   employee))
  

(defun edit-id-prompt ()
  (write-line "Please enter a client, employee, or appointment ID.")
  (edit (parse-integer (prompt-read "ID"))))

(defun edit (&optional id)
  (if id
      (edit-prompt (bartleby::id-search (if (numberp id)
					    id
					    (parse-integer id))))
      (edit-id-prompt)))
  
