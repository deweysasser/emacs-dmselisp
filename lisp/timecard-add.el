;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; timecard-add.el -- additions to timecard-mode.el
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser
;; Created On      : Mon Apr 01 10:58:04 1996
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defun timecard-sumarize-day ()
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: timecard-add.el,v $
;; $Revision: 1.3 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)

(defun timecard-sumarize-day ()
    (interactive)
  (let* ((timecard-account-regex "\\**\\([-._/A-Za-z:0-9]*\\)?\\**")
	(timecard-entry-regex  (format "^\\(%s\\):\t\\(%s\\|%s\\) - \\(%s\\)\t%s"
				       timecard-date-regex
				       timecard-time-regex timecard-continuation-str
				       timecard-time-regex
				       timecard-account-regex)))
  (let (day tin tout time last-time last-tout emark tallies total acct
	    (all (make-hash-table :test 'equal)))

    ;; find end of day's timecard entries
    (beginning-of-line)
    (cond ((looking-at "^\\s *$")
           (skip-chars-backward " \t\n")))
    (re-search-forward "^\\s *$" nil 'go)
    (goto-char (match-beginning 0))
    (delete-horizontal-space)
    (setq emark (point-marker))
    (if (save-excursion (forward-line -1) (looking-at timecard-summary-regex))
        (kill-line -1))
      
    ;; find beginning
    (forward-char -1)
    (re-search-backward "^\\s *$" nil t)

    (save-restriction
      (narrow-to-region (point) emark)
      (while (re-search-forward timecard-entry-regex nil 'go)

        ;; under what day shall this entry be recorded?
        (or day (setq day (timecard-mstring 1)))
	(setq acct (timecard-mstring 4))

        ;; calculate number of minutes for this entry
        (setq tin (timecard-mstring 2)
              tout (timecard-mstring 3)
              last-time 0)
        (cond ((string-match "^ " tin)
               (or tallies
                   (error "First entry of day must be have a start time"))
               (setq tin last-tout
                     last-time (car tallies)
                     tallies (cdr tallies))))
        (setq last-tout tout
              tin (timecard-eval-timestr tin)
              tout (timecard-eval-timestr tout tin)
              time (+ last-time (- tout tin))
              tallies (cons time tallies))
	(setf (gethash acct all)
	      (+ (- tout tin)
		 (if (gethash acct all)
		     (gethash acct all)
		   0)))
	)
      

      (or (bolp) (insert ?\n))
      (setq total (apply '+ tallies))
      (insert (format "%s:: [ %s ] = %d min = %s\n"
                      day (timecard-hhmm-string total) total
                      (mapconcat 'int-to-string (nreverse tallies) " + ")))
      (loop for x being the hash-keys of all
	    do
	    (insert (format "%-20s: %s\n"
			    x (timecard-hhmm-string (gethash x all)))))
      (forward-line -1)))))


