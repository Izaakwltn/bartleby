;;;;edit.lisp
;;;;

(in-package :bart)

;;;;------------------------------------------------------------------------
;;;;editing interface
;;;;------------------------------------------------------------------------

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

;------------------------------------------------------------------------
;Editing First Names
;------------------------------------------------------------------------

(defgeneric edit-first-name (object)
  (:documentation "Prompt for editing the first name of an object"))

(defmethod edit-first-name ((client bartleby:client))
  (bartleby:change-first-name client (prompt-read "New First Name")))

(defmethod edit-first-name ((employee bartleby:employee))
  (bartleby:change-first-name employee (prompt-read "New First Name")))

;------------------------------------------------------------------------
;Editing Last Names
;------------------------------------------------------------------------

(defgeneric edit-last-name (object)
  (:documentation "Prompt for editing the last name of an object"))

(defmethod edit-last-name ((client bartleby:client))
  (bartleby:change-last-name client (prompt-read "New Last Name")))

(defmethod edit-last-name ((employee bartleby:employee))
  (bartleby:change-last-name employee (prompt-read "New Last Name")))

;------------------------------------------------------------------------
;Editing IDs
;------------------------------------------------------------------------

(defgeneric edit-id (object)
  (:documentation "Prompt for editing the last name of an object"))

(defmethod edit-id ((client bartleby:client))
  (bartleby:change-id client (parse-integer (prompt-read "New ID"))))

(defmethod edit-id ((employee bartleby:employee))
  (bartleby:change-id employee (parse-integer (prompt-read "New ID"))))

(defmethod edit-id ((meeting-room bartleby::meeting-room))
  (bartleby:change-id meeting-room (parse-integer (prompt-read "New ID"))))

(defmethod edit-id ((appointment bartleby:appointment))
  (bartleby:change-id appointment (parse-integer (prompt-read "New ID"))))

;------------------------------------------------------------------------
;Editing Phone Numbers
;------------------------------------------------------------------------

(defgeneric edit-phone (object)
  (:documentation "Prompt for editing the last name of an object"))

(defmethod edit-phone ((client bartleby:client))
  (bartleby:change-phone client (make-phone-number (prompt-read "New Phone, xxxxxxxxxx"))))

(defmethod edit-phone ((employee bartleby:employee))
  (bartleby:change-phone employee (make-phone-number (prompt-read "New Phone, xxxxxxxxxx"))))

;------------------------------------------------------------------------
;Editing Emails
;------------------------------------------------------------------------

(defgeneric edit-email (object)
  (:documentation "Prompt for editing the email of an object"))

(defmethod edit-email ((client bartleby:client))
  (bartleby:change-email client (make-email (prompt-read "New Email, username@domain.com"))))

(defmethod edit-email ((employee bartleby:employee))
  (bartleby:change-email employee (make-email (prompt-read "New Email, username@domain.com"))))

;------------------------------------------------------------------------
;Editing Notes
;------------------------------------------------------------------------

(defgeneric edit-notes (object)
  (:documentation "Prompt for editing the notes for an object"))

(defmethod edit-notes ((client bartleby:client))
  (bartleby:change-notes client (prompt-read "New Notes")))

(defmethod edit-notes ((meeting-room bartleby:meeting-room))
  (bartleby:change-notes meeting-room (prompt-read "New Notes")))

(defmethod edit-notes ((appointment bartleby:appointment))
  (bartleby:change-notes appointment (prompt-read "New Notes")))

;------------------------------------------------------------------------
;Editing Misc.
;------------------------------------------------------------------------

(defmethod edit-credits ((client bartleby:client))
  (bartleby:change-credits client (parse-integer (prompt-read "New Credit Minutes Value"))))

(defmethod edit-name ((meeting-room bartleby:meeting-room))
  (bartleby:change-name meeting-room (prompt-read "New Name for Room")))

(defmethod edit-capacity ((meeting-room bartleby:meeting-room))
  (bartleby:change-capacity meeting-room (parse-integer (prompt-read "New Capacity for Room"))))

;(defmethod edit-time ((appointment bartleby:appointment))

					; (bartleby:change-app
;------------------------------------------------------------------------

(defgeneric edit-prompt (object)
  (:documentation "A prompt for editing a given object"))

(defmethod edit-prompt ((client bartleby:client))
  (write-line "What would you like to edit?")
  (funcall
   (eval (second
	  (find-if #'(lambda (x)
		       (string-equal (first x)
			             (prompt-read "first, last, id, phone, email, credits, or notes")))
	           *edit-commands*)))
   client))

(defmethod edit-prompt ((employee bartleby:employee))
  (write-line "What would you like to edit?")
  (funcall
   (eval (second
	  (find-if #'(lambda (x)
		       (string-equal (first x)
			             (prompt-read "first, last, id, phone, email, or hourly")))
	           *edit-commands*)))
   employee))

(defmethod edit-prompt ((meeting-room bartleby:meeting-room))
  (write-line "What would you like to edit?")
  (funcall
   (eval (second
	  (find-if #'(lambda (x)
		       (string-equal (first x)
			             (prompt-read "name, id, capacity, or notes")))
	           *edit-commands*)))
	 meeting-room))

;(defmethod edit-prompt ((appointment appointment))
  

(defun edit-id-prompt ()
  (write-line "Please enter a client, employee, or appointment ID.")
  (edit (parse-integer (prompt-read "ID"))))

(defun edit (&optional id)
  (if id
      (edit-prompt (bartleby::id-search (if (numberp id)
					    id
					    (parse-integer id))))
      (edit-id-prompt)))
  
