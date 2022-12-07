;;;; web-bartleby/www/search.lisp
;;;;
;;;; Copyright (c) 2022 Izaak Walton

(in-package :web-bartleby)

(hunchentoot:define-easy-handler (search-results :uri "/search") (query)
  (with-page (:title "Search Results")
    (:h1 (format nil "query"))))
