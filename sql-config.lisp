;;;; sql-config.lisp
;;;;
;;;; Copyright Izaak Walton (C) 2022-2023

(defpackage #:bartleby-sql
  (:use #:mito))

(in-package #:bartleby-sql)

(defun connect-postgres (database-name username password)
  "Connects to an extant postgres database given the database name, username, and password."
  (mito:connect-toplevel :postgres :database-name database-name :username username :password password))

(defun bartleby-connect ()
  (connect-postgres "bartleby" "izaak" "johann-sebastian-bach"))

(bartleby-connect)
