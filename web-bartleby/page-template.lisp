;;;; web-bartleby/page-template.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :web-bartleby)

(setf (html-mode) :html5)

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
      ;(:header
      ; (:nav ;maybe eventually fill with a loop from a list of pages
;	(:a :href "index.html" "Home") ; new appointment ; new client...
;	(:a :href "Schedule.html" "Schedule")
;	(:a :href "lessons.html" "Lessons")
;	(:a :href "resonance.html" "Resonance Calculator")
;	(:a :href "library/refdesk.html" "Sheet Music Library")))
     (:body
      (cl-bootstrap:bs-navbar (:brand "cl-bootstrap")
        (cl-bootstrap:bs-navbar-nav ()
            (cl-bootstrap:bs-nav-dropdown (:title "New")
              (cl-bootstrap:bs-nav-li (:href "/new-appointment") "Appointment")
              (:li (:a :href "/new-client" "Client"))
              (:li (:a :href "/new-employee" "Employee"))
              (:li (:a :href "/new-room" "Room")))))
      (cl-bootstrap:bs-container ()
        (cl-bootstrap:bs-row
          (cl-bootstrap:bs-col-md ()
        ,@body)
      (:footer "Bartleby the Scheduler - Copyright (c) 2021-2022 "
	       (:a :href "https://www.github.com/izaakwltn/bartleby"
		   "Written using Common Lisp")))))))

;;;;HTML generation

(hunchentoot:define-easy-handler (app :uri "/") ()
  (with-page (:title "Home")
    (cl-bootstrap:bs-well
      (:p "I would prefer not to")
      (:p (cl-bootstrap:bs-icon (:glyph "search"))))))

(defun generate-html-file (filename html-text)
  (with-open-file (output
                   (concatenate 'string
                                (asdf:system-relative-pathname "web-bartleby" "web-bartleby/www/")
                                filename)
			  :direction :output
			  :if-exists :overwrite
			  :if-does-not-exist :create)
  (format output "~a" html-text)))
