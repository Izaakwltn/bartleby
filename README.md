#### Bartleby
###     the Scheduler

A scheduling system written in Common Lisp

### Getting Set Up

Before running the first time, set up a postgresql database with a login, and add the login information to #'bartleby-connect in sql.lisp.

The database connection should automatically start up on quickload, but it might be necessary to run (bartleby-connect) if you start getting mito errors.

### Testing
If you want to give it a try but don't have your own data to use, clients, employees, and rooms can be generated before starting the interface by using (generate-clients n), (generate-employees n), or (generate-rooms n), respectively. 

## Web GUI

Package :WEB-BARTLEBY provides a web-gui application for handling Bartleby operations. Could be used on server 
