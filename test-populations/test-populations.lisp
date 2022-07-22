;;;; test-populations.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :bartleby)

;;; Functions for generating people

(defvar *first-names*
  (uiop:read-file-lines
   (asdf:system-relative-pathname
    "bartleby" "test-populations/first-names.txt")))

(defvar *last-names*
  (uiop:read-file-lines
   (asdf:system-relative-pathname
    "bartleby" "test-populations/last-names.txt")))

(defun random-first ()
  (nth (random 199) *first-names*))

(defun random-last ()
  (nth (random 999) *last-names*))

;;; Randomized contact information

(defvar *email-domains* '("gmail.com" "yahoo.com" "hotmail.com" "aol.com" "msn.com"))

(defun random-email (first-name last-name)
  (concatenate 'string last-name "." first-name "@" (nth (random 4) *email-domains*)))

(defun random-phone-digits ()
  (loop :with number := nil
        :for i :from 1 :to 10
	:do (setf number (concatenate 'string number (write-to-string (random 9))))
	:finally (return number)))

(defvar *street-types* '("Street" "Avenue" "Boulevard" "Road" "Drive" "Circle"))

(defun random-address ()
  (concatenate 'string
               (write-to-string (random 5000))
               " "
               (random-last)
               " "
               (nth (random 5) *street-types*)
               " Denver, CO 80808"))

;;; Generating People

(defun generate-clients (number-of-clients)
  (loop :for i :from 1 :to number-of-clients
	:do (let ((fn (random-first))
		  (ln (random-last)))
	      (add-client (make-client fn
                                       ln
			               (random-phone-digits)
                                       (random-email fn ln)
                                       (random-address)
                                       "n/a")))))

(defun generate-employees (number-of-clients)
  (loop :for i :from 1 :to number-of-clients
	:do (let ((fn (random-first))
		  (ln (random-last)))
	      (add-employee (make-employee fn
                                           ln
			                   (random-phone-digits)
                                           (random-email fn ln)
                                           (random-address)
                                           *standard-hourly*
                                           "n/a")))))
               
;;; Generating Rooms

(defvar *room-names* '("Violin" "Piano" "Two Pianos" "The Library" "Room with the Broken Chair""Guitar Room" "The Chokey" "The Kitchen" "The room where everything works" "The room where nothing works"))

(defun random-room (room-names)
  (nth (random (length room-names)) room-names))

(defun generate-rooms (number-of-rooms)
  (loop :for i :from (+ 1 (room-count)) :to number-of-rooms
	:do (add-room (make-room i (random-room *room-names*) (random 15) "test room"))))
