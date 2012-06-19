;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; date.el -- Date and time functions
;; 
;; Author          : Dewey M. Sasser
;; Created On      : Oct 19 1993
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defun insert-current-date-and-time ()
;;   (defun insert-current-date ()
;;   (defvar preferred-date-format 'short)
;;   (defvar lookup-month::month-list nil)
;;   (defun current-date-string (&optional what-date)
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: date2.el,v $
;; $Revision: 1.3 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; $Id: date2.el,v 1.3 2004/08/20 21:24:25 dewey Exp $

;Elmakedoc:  Automatic Prototyping Destinations
;  functions: date.doc
;  variables: date.doc


;;;###autoload
(defun insert-current-date-and-time ()
  "Inserts the date and time into the buffer at the current point."
  (interactive)
  (insert (current-time-string)))

;;;###autoload
(defun insert-current-date ()
  "Inserts the Current Date"
  (interactive)
  (insert (current-date-string)))


(defvar preferred-date-format 'short)

(defvar lookup-month::month-list nil)


;;;###autoload
(defun current-date-string (&optional what-date)
  "Return the Current Date as a String"
  (let (
	(date (current-time-string))
	)
    (setq what-date
	  (if what-date
	      what-date
	    preferred-date-format))
    (cond
     ((eq what-date 'normal)
      (format-time-string "%m/%d/%Y"))
     ((eq what-date 'short)
      (format-time-string "%Y-%m-%d"))
     ((eq what-date 'month-year)
      (format-time-string "%m/%Y")))))


