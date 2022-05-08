#### Bartleby
###     the Scheduler

A scheduling system written in Common Lisp

Current Features:
Client/Employee/Room/Appointment generation with backups
Invoice generation
Scheduling interface with limited commands (currently BROWSE, VIEW, NEW, EDIT, and CHECKOUT)
In progress: Availability checking for safe scheduling

## BART Interface
To use the BART interface, load :bart, otherwise load :bartleby.

In bart, type HELP to see an explanation of commands, and to exit type either QUIT or EXIT.


### Testing
If you want to give it a try but don't have your own data to use, clients, employees, and rooms can be generated before starting the interface by using (generate-clients n), (generate-employees n), or (generate-rooms n), respectively. 
