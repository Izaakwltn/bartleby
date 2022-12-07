;;;; web-bartleby/www/home-page.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :webbartleby)

(hunchentoot:define-easy-handler (home-page :uri "/") ()
  (with-page (:title "Home")
    (:h1 "Bartleby the Scheduler")
    (:h3 "Welcome to Bartleby the Scheduler, a simple scheduling system for reluctant schedulers.")
    (:hr)))
