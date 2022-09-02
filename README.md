# Bartleby
###     the Scheduler

A scheduling system for reluctant schedulers, written in Common Lisp

### Getting Set Up
Before running the first time, set up a postgresql database with a login, and add the login information to #'bartleby-connect in sql.lisp. 

The database connection should automatically start up on quickload, but it might be necessary to run (bartleby-connect) if you start getting mito errors.

Note: Currently using SBCL and postgresql, hasn't been tested with other Lisp implementations or sqls.

### Testing
If you want to give it a try but don't have your own data to use, clients, employees, and rooms can be generated before starting the interface by using (generate-clients n), (generate-employees n), or (generate-rooms n), respectively. 

## Web GUI

Package :WEB-BARTLEBY provides a web-gui application for handling Bartleby operations. To start a local copy, run (web-bartleby::launch) from the repl and go to 127.0.0.1:4242 in your browser.
