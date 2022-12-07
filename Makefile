LISP ?= sbcl

build:
	$(LISP) --load webbartleby.asd \
	--eval '(ql:quickload :webbartleby)' \
		--eval '(asdf:make :webbartleby)' \
		--eval '(quit)'
