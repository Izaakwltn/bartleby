FROM clfoundation/sbcl:2.2.2

ARG QUICKLISP_DIST_VERSION=2022-02-20
ARG QUICKLISP_ADD_TO_INIT_FILE=true

WORKDIR /web-bartleby
COPY . /web-bartleby

RUN set -x; \
  /usr/local/bin/install-quicklisp \
  sbcl --eval '(ql:quickload :web-bartleby)' --eval '(web-bartleby::launch)'

EXPOSE 4242