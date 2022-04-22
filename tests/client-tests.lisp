;;;;client-tests.lisp
;;;;

(in-package :bartleby)

;;;;------------------------------------------------------------------------
;;;;Generating Test Clients
;;;;------------------------------------------------------------------------

(defvar first-names nil) ;later work from a txt file

(defvar last-names nil)

(defun random-first ()
  (write-to-string (nth (random 199) first-names)))

(defun random-last ()
  (write-to-string (nth (random 199) last-names)))

;(defvar email-domains '("gmail.com" "yahoo.com" "hotmail.com" "aol.com" "msn.com"))

;(defun random-email (first-name last-name)
 ; (concatenate 'string last-name "." first-name "@" (nth (random 4) email-domains)))
  
(defun random-phone-digits ()
  (loop :with number := nil
        :for i :from 1 :to 10
	:do (if (or (equal i 4)
	            (equal i 7))
		(setf number
		      (concatenate 'string number "-" (write-to-string (random 9))))
		(setf number
	              (concatenate 'string number (write-to-string (random 9)))))
	:finally (return number)))
		       
(defun generate-clients (number-of-clients)
  (loop :for i :from 1 :to number-of-clients
	:do (let ((fn (random-first))
		  (ln (random-last)))
	      (new-client fn
			  ln
			  (random-phone-digits)
			  (concatenate 'string
				       (username (auto-email fn ln))
				       "@"
				       (domain (auto-email fn ln)))
			  (random-address) "n/a"))))

(setq first-names '(Michael Christopher Jessica Matthew Ashley Jennifer Joshua Amanda Daniel David James Robert John Joseph Andrew Ryan Brandon Jason Justin Sarah William Jonathan Stephanie Brian Nicole Nicholas Anthony Heather Eric Elizabeth Adam Megan
Melissa
Kevin
Steven
Thomas
Timothy
Christina
Kyle
Rachel
Laura
Lauren
Amber
Brittany
Danielle
Richard
Kimberly
Jeffrey
Amy
Crystal
Michelle
Tiffany
Jeremy
Benjamin
Mark
Emily
Aaron
Charles
Rebecca
Jacob
Stephen
Patrick
Sean
Erin
Zachary
Jamie
Kelly
Samantha
Nathan
Sara
Dustin
Paul
Angela
Tyler
Scott
Katherine
Andrea
Gregory
Erica
Mary
Travis
Lisa
Kenneth
Bryan
Lindsey
Kristen
Jose
Alexander
Jesse
Katie
Lindsay
Shannon
Vanessa
Courtney
Christine
Alicia
Cody
Allison
Bradley
Samuel
Shawn
April
Derek
Kathryn
Kristin
Chad
Jenna
Tara
Maria
Krystal
Jared
Anna
Edward
Julie
Peter
Holly
Marcus
Kristina
Natalie
Jordan
Victoria
Jacqueline
Corey
Keith
Monica
Juan
Donald
Cassandra
Meghan
Joel
Shane
Phillip
Patricia
Brett
Ronald
Catherine
George
Antonio
Cynthia
Stacy
Kathleen
Raymond
Carlos
Brandi
Douglas
Nathaniel
Ian
Craig
Brandy
Alex
Valerie
Veronica
Cory
Whitney
Gary
Derrick
Philip
Luis
Diana
Chelsea
Leslie
Caitlin
Leah
Natasha
Erika
Casey
Latoya
Erik
Dana
Victor
Brent
Dominique
Frank
Brittney
Evan
Gabriel
Julia
Candice
Karen
Melanie
Adrian
Stacey
Margaret
Sheena
Wesley
Vincent
Alexandra
Katrina
Bethany
Nichole
Larry
Jeffery
Curtis
Carrie
Todd
Blake
Christian
Randy
Dennis
		    Alison))

(setq last-names '(Chung
Chen
Melton
Hill
Puckett
Song
Hamilton
Bender
Wagner
McLaughlin
McNamara
Raynor
Moon
Woodard
Desai
Wallace
Lawrence
Griffin
Dougherty
Powers
May
Steele
Teague
Vick
Gallagher
Solomon
Walsh
Monroe
Connolly
Hawkins
Middleton
Goldstein
Watts
Johnston
Weeks
Wilkerson
Barton
Walton
Hall
Ross
Woods
Mangum
Joseph
Rosenthal
Bowden
Underwood
Jones
Baker
Merritt
Cross
Cooper
Holmes
Sharpe
Morgan
Hoyle
Allen
Rich
Grant
Proctor
Diaz
Graham
Watkins
Hinton
Marsh
Hewitt
Branch
O'Brien
Case
Christensen
Parks
Hardin
Lucas
Eason
Davidson
Whitehead
Rose
Sparks
Moore
Pearson
Rodgers
Graves
Scarborough
Sutton
Sinclair
Bowman
Olsen
Love
McLean
Christian
Lamb
James
Chandler
Stout
Cowan
Golden
Bowling
Beasley
Clapp
Abrams
Tilley
Morse
Boykin
Sumner
Cassidy
Heath
Blanchard
McAllister
McKenzie
Byrne
Schroeder
Gross
Perkins
Robertson
Palmer
Brady
Rowe
Zhang
Hodge
Li
Justice
Glass
Willis
Hester
Floyd
Fischer
Norman
Chan
Hunt
Byrd
Lane
Kaplan
Heller
Jennings
Hanna
Locklear
Holloway
Glover
O'Donnell
Goldman
McKenna
Starr
Stone
McClure
Watson
Abbott
Singer
Farrell
Atkins
Sykes
Reid
Finch
Hobbs
Adkins
Kinney
Whitaker
Alexander
Conner
Waters
Becker
Rollins
Black
Fox
Hatcher
Wu
Lloyd
Joyce
Welch
Matthews
Chappell
MacDonald
Kane
Butler
Pickett
Kennedy
Thornton
McNeill
Weinstein
Moss
Carlton
Schultz
Nichols
Harvey
Stevenson
Houston
Dunn
West
Barr
Snyder
Cain
Boswell
Pittman
Weiner
Petersen
Davis
Coleman
Terrell
Burch
Parrott
Henry
Gray))
