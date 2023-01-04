;;;; webbartleby/page-template.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :webbartleby)

(setf (cl-bootstrap::html-mode) :html5)

(defmacro with-page ((&key title) &body body)
  `(cl-who:with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     (:doctype)
     (:html :lang "en"
      (:head
       (:meta :charset "utf-8")
       (:meta :name "viewport" :content "width=device-width, initial-scale=1")
       (:link :type "text/css"
              :rel "stylesheet"
	      :href ,cl-bootstrap:*bootstrap-css-url*)
       (:title ,title)
       (:script :src ,cl-bootstrap:*jquery-url*)
       (:script :src ,cl-bootstrap:*bootstrap-js-url*)))
     (:body
      (cl-bootstrap:bs-navbar (:brand "Bartleby")
	(cl-bootstrap:bs-navbar-nav ()
	  (cl-bootstrap:bs-nav-dropdown (:title "New")
	    (cl-bootstrap:bs-nav-li (:href "/new-appointment") "Appointment")
            (:li (:a :href "/new-client" "Client"))
            (:li (:a :href "/new-employee" "Employee"))
            (:li (:a :href "/new-room" "Room")))
	  (cl-bootstrap:bs-nav-dropdown (:title "Browse")
              (cl-bootstrap:bs-nav-li (:href "/browse-appointments") "Appointments")
              (:li (:a :href "/browse-clients" "Clients"))
              (:li (:a :href "/browse-employees" "Employees"))
	      (:li (:a :href "/browse-rooms" "Rooms")))
	  (cl-bootstrap:bs-nav-dropdown (:title "Calendar")
              (cl-bootstrap:bs-nav-li (:href "/daily-calendar") "Daily")
              (:li (:a :href "/weekly-calendar" "Weekly"))
	    (:li (:a :href "/monthly-calendar" "Monthly")))
          (cl-bootstrap:bs-nav-li (:href "/appointments-check-out") "Check Out")
              (:form :id "query" :action "/search-results"
                 (:input :type "search" :id "query" :name "query" :placeholder "Search")
                 (:button "Submit"))))

                                        ;(cl-bootstrap:bs-navbar-form ()
	;	    (:div
	;		(:div :class "form-group" :action "/search-results"
	;		    (:input :type "text" :class "form-control" :id "query" :name "query" :placeholder "Search"))
	;		(:button :type "submit" :form "query" :class "btn btn-default" "Submit")))))
      (cl-bootstrap:bs-container ()
        (cl-bootstrap:bs-row
          (cl-bootstrap:bs-col-md ()
        ,@body)
      (:footer "Bartleby the Scheduler - Copyright (c) 2021-2022 "
	       (:a :href "https://www.github.com/izaakwltn/bartleby"
		   "Written using Common Lisp")))))))

;;;;HTML generation

;(hunchentoot:define-easy-handler (home-page :uri "/") ()
 ; (with-page (:title "Home")
  ;  (:h1 "Bartleby the Scheduler")
   ; (:p "Welcome to Bartleby the Scheduler, a barebones approach to scheduling, written in Common Lisp")
   ; (:hr)))

;(hunchentoot:define-easy-handler (new-client-form :uri "/new-client") ()
 ; (with-page (:title "New Client")
  ;  (:h1 "New Client")
   ; (:form
    ; (:label :for "fname"
;	     "First Name")
 ;    (:input :type "text" :id "fname" :name "fname")
  ;   (:hr)
   ;  (:label :for "lname"
;	     " Last Name ")
   ;  (:input :type "text" :id "lname" :name "lname")
 ;    (:hr)
  ;   (:label :for "phone"
;	     "Phone Number XXXXXXXXXX ")
 ;    (:input :type "text" :id "phone" :name "phone")
  ;   (:hr)
   ;  (cl-bootstrap:bs-form-email ())
    ; (:hr)
     ;(:label :for "address"
;	     "Address ")
 ;    (:input :type "text" :id "address" :name "address")
  ;   (:hr)
  ;   (:label :for "notes"
;	     "Notes ")
 ;    (:input :type "text" :id "notes" :name "notes")
;	          (cl-bootstrap:bs-form-checkbox "Check me out")
;	     (:button :type "submit" :class "btn btn-default" "Submit"))))

(defun generate-html-file (filename html-text)
  (with-open-file (output
                   (concatenate 'string
                                (asdf:system-relative-pathname "web-bartleby" "web-bartleby/www/")
                                filename)
			  :direction :output
			  :if-exists :overwrite
			  :if-does-not-exist :create)
  (format output "~a" html-text)))
