;;;; web-bartleby/www/home-page.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :web-bartleby)

(hunchentoot:define-easy-handler (home-page :uri "/") ()
  (with-page (:title "Home")
    (:h1 "Bartleby the Scheduler")
    (:p "Welcome to Bartleby the Scheduler, a barebones approach to scheduling, written in Common Lisp")
    (:hr)))
