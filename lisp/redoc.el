;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; redoc.el -- redocument-c-function
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser
;; Created On      : Thu Mar 23 09:53:00 1995
;; Last Modified By: Dewey M. Sasser
;; Last Modified On: Mon Apr 10 14:45:22 1995
;; Update Count    : 57
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defstruct function-desc
;;   (defun find-doc-information ()
;;   (defun get-function-returns ()
;;   (defun get-function-description ()
;;   (defun get-function-args ()
;;   (defun get-function-args-internal (theList)
;;   (defun get-function-notes ()
;;   (defun get-function-caveats ()
;;   (defun get-function-tests ()
;;   (defun get-function-field (&optional skip)
;;   (defun get-brief-description ()
;;   (defun get-author-name ()
;;   (defun get-until-match (theRegexp)
;;   (defun get-rest-of-line ()
;;   (defun function-find-arg-description (theFunction theArg)
;;   (defun function-find-arg-description-internal (someArgs theArg)
;; 
;;  OPTIONS
;;    Update Tests:  no
;; 
;; $RCSfile: redoc.el,v $
;; $Revision: 1.2 $
;; $Log: redoc.el,v $
;; Revision 1.2  1995/06/05 20:27:38  dewey
;; Elisp source
;;
; Revision 1.1  1995/04/10  13:24:51  dewey
; Many changes
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'cl)

(defstruct function-desc
  "Structure which holds information about function documentation"
  brief					;brief description
  author				;author
  description				;description
  args					;args (name, description pair)
  returns				;return commens
  notes					;notes
  caveats				;caveats
  tests)				;test section

(defun find-doc-information ()
  "Return the proper function structure holding all existing documentation"
  (save-excursion
    (let ((parts (save-excursion
		   (find-c-function-by-parts)))
	  (header-string (header-prefix-string))
	  (limit)
	  (start)
	  (fun (make-function-desc)))
      (search-backward "/*")
      (setq limit (save-excursion
		    (search-forward "*/")
		    (backward-char 2)
		    (point)))
      (setq start (point))
      (if (search-forward-regexp (concat (function-get-name parts) "\\(()\\)? -- ?") limit 't)
	  (setf (function-desc-brief fun) (get-brief-description)))
      (if (search-forward "AUTHOR: " limit 't)
	  (setf (function-desc-author fun) (get-author-name)))
      (if (search-forward "DESCRIPTION" limit 't)
	  (setf (function-desc-description fun) (get-function-description)))
      (if (search-forward "ARGUMENTS" limit 't)
	  (setf (function-desc-args fun) (get-function-args)))
      (if (search-forward "RETURNS" limit 't)
	  (setf (function-desc-returns fun) (get-function-returns)))
      (if (search-forward "NOTES" limit 't)
	  (setf (function-desc-notes fun) (get-function-notes)))
      (if (search-forward "CAVEATS" limit 't)
	  (setf (function-desc-caveats fun) (get-function-caveats)))
      (if (search-forward "TESTS" limit 't)
	  (setf (function-desc-tests fun) (get-function-tests)))
      fun)))



(defun get-function-returns ()
  (if (looking-at " NOTHING")
      "NOTHING"
    (search-forward " -- " limit t)
    (get-function-field)))

(defun get-function-description ()
  (get-function-field 't))

(defun get-function-args ()
  (if (looking-at ": NONE")
      "NONE"
    (forward-line 1)
    (beginning-of-line)
    (let ((limit (save-excursion
		   (search-forward "RETURNS" limit 't)
		   (beginning-of-line)
		   (1+ (point)))))
    (get-function-args-internal nil))))

(defun get-function-args-internal (theList)
  (let ((argname))
    (if (not (search-forward-regexp "\\([a-zA-Z_][a-zA-Z_0-9]*\\) (" limit 't))
	theList
      (setq argname (buffer-substring (match-beginning 1)
				      (match-end 1)))
      (search-forward " -- " limit 't)
      (get-function-args-internal (cons (cons argname
						(get-until-match
						 "\\(^[^\n]* -- \\|^ \\* RETURNS\\)"))
					  theList)))))
    
      
  

(defun get-function-notes ()
  (get-function-field 't))

(defun get-function-caveats ()
  (get-function-field 't))

(defun get-function-tests ()
  (beginning-of-line)
  (forward-line 1)
  (get-until-match (regexp-quote " */")))


(defun get-function-field (&optional skip)
  "Get a functions description"
  (if (not skip)
      nil
    (beginning-of-line)
    (forward-line 1))
  (get-until-match (concat (regexp-quote header-string )
			   "[^ \t]")))



(defun get-brief-description ()
  (get-rest-of-line))

(defun get-author-name ()
  (get-rest-of-line))


(defun get-until-match (theRegexp)
  "Get lines until match"
  (let ((start (point))
	(end)
	(string))
    (while (and (not (looking-at theRegexp)) (< (point) limit))
      (forward-line 1)
      (beginning-of-line))
    (if (> (point) limit)
	(goto-char limit))
    (if (= (char-after (- (point) 1))
	   10)
	(backward-char 1))
    (setq end (point))
    (buffer-substring start (point))))


(defun get-rest-of-line ()
  "Return the rest of the line as a string"
  (let ((start (point))
	(end (save-excursion (end-of-line)
			     (point))))
    (buffer-substring start end)))
	

(defun function-find-arg-description (theFunction theArg)
  "Find a return the description matching arg"
  (if (function-p theFunction)
      (function-find-arg-description-internal (function-args theFunction) theArg)
    ""))

  
(defun function-find-arg-description-internal (someArgs theArg)
  (if someArgs
      (if (string= (car (car someArgs)) theArg)
	  (cdr (car someArgs))
	(function-find-arg-description-internal (cdr someArgs) theArg))
    ""))
      
(provide 'redoc)