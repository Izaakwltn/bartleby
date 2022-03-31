;;;;invoices.lisp
;;;;

(in-package :schedulizer)

;;;;------------------------------------------------------------------------

;;;two steps: appointments are stored first as unchecked
;;;then have a repl cycle which goes through each unchecked appointment,
;;;prompts for either No Show, Cancelled Makeup Added, or Arrived on each lesson
;;;maybe also prompt for makeups added or used

;;;;two lists: *unchecked-appointments* and *checked-appointments*
