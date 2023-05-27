;;;; scheduling/package.lisp
;;;;
;;;; Copyright Izaak Walton (C) 2022-2023

(cl:defpackage #:bartleby-scheduling
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
   (#:sql #:bartleby-sql)))
