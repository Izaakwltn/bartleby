FROM clfoundation/sbcl:2.2.2

ARG QUICKLISP_DIST_VERSION=2022-02-20
ARG QUICKLISP_ADD_TO_INIT_FILE=true

RUN apt-get update && apt-get install make

WORKDIR /bartleby
COPY /webbartleby .
COPY /src .
COPY  webbartleby.asd .
COPY bartleby.asd .
COPY /invoices .
COPY Makefile .
COPY launch-webbartleby .


EXPOSE 4242

RUN set -x; \
  /usr/local/bin/install-quicklisp \

RUN make
RUN ./launch-webbartleby

# sbcl --eval '(ql:quickload :web-bartleby)' --eval '(web-bartleby::main)'
