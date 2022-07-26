;;;; web-bartleby/search-results.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :web-bartleby)

(ql:quickload :alexa)
;;; First, lexical analysis

(deftype token ()
  '(cons keyword t))

(defun tok (type &optional val)
  (cons type val))

(alexa:define-string-lexer search-lexer
  "Lexing search parameters"/
  ((:number "\\d+")
   (:word "[A-Za-z][A-Za-z]")
   (:email "[A-Za-z][A-Za-z0-9]\\@[A-Za-z].[A-Z][a-z]"))
  ("{{phone}}"
   (return (tok :number (intern $@))))
  ("{{email}}"
   (return (tok :email (intern  $@))))
  ("{{word}}"
   (return (tok :word (intern  $@))))
  ("\\s+" nil))
