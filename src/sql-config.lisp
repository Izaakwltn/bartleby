;;;; sql.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :bartleby)

;;; Connecting to postgreSQL via Mito

(defun connect-postgres (database-name username password)
  "Connects to a postgres database given the database name, username, and password."
  (mito:connect-toplevel :postgres :database-name database-name :username username :password password))

(defun bartleby-connect () ;change with your database, username, and password
  (connect-postgres "bartleby" "izaak" "johann-sebastian-bach"))

;docker run --name postgres-db -e POSTGRES_PASSWORD=johann-sebastian-bach -p 5432:5432 -d postgres

(bartleby-connect)

