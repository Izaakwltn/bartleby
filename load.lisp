(ql:register-local-projects)
(ql:quickload :webbartleby)
(save-lisp-and-die "launch-webbartleby" :executable t :toplevel #'webbartleby::main)
