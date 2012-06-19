;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; testsup.el -- support testing fields in functions
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser
;; Created On      : Tue Jan 10 22:11:15 1995
;; Status          : $State: Exp $
;; Name            : $Name: HEAD $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defvar function-test-mode-list '(c-mode c++-mode emacs-lisp-mode)
;;   (defvar function-test-mode-exclustion-string-list '("\\.h$")
;;   (defvar header-tests-on nil
;;   (defun header-options ()
;;   (defun string-match-listp (string list)
;;   (defun symbol-in-listp (symbol list)
;;   (defun update-function-test-fields ()
;;   (defun insert-function-change-records ()
;;   (defun update-function-tests-by-list (function-list)
;;   (defun find-function-data (funname function-list)
;;   (defun update-function-tests (fundata funname)
;;   (defun checksum-function ()
;;   (defun clear-function-test-flags (funname)
;; 
;;  OPTIONS
;;    Update Tests:  no
;; 
;; $RCSfile: testsup.el,v $
;; $Revision: 1.7 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'protos)

(defvar function-test-mode-list '(c-mode c++-mode emacs-lisp-mode)
  "List of modes in which to activate function test updates")

(defvar function-test-mode-exclustion-string-list '("\\.h$")
  "List of REGEXPs to not include options")

(defvar header-tests-on nil
  "Non nil if test should be updated")

(defun header-options ()
  "Insert header options line"
  (let ((update-test "no"))
    (if (and
	 (symbol-in-listp major-mode function-test-mode-list)
	 (not (string-match-listp (buffer-file-name)
				  function-test-mode-exclustion-string-list)))
	(setq update-test "yes"))
    (insert header-prefix-string " OPTIONS\n"
	  header-prefix-string "   Update Tests:  " update-test "\n")))


(defun string-match-listp (string list)
  "Return 't if STRING matches any one of LIST of regexps"
  (let ((returnval nil))
    (while (and (not returnval) (car list))
      (if (string-match (car list) string)
	  (setq returnval 't))
      (setq list (cdr list)))
    returnval))
  

(defun symbol-in-listp (symbol list)
  "Return 't if symbol is in list, nil otherwise"
  (let ((returnval nil))
    (while (and (not returnval) (car list))
      (if (eq symbol (car list))
	  (setq returnval 't))
      (setq list (cdr list)))
    returnval))
	  

(defun update-function-test-fields ()
  "Called automatically by update toc, updates function test blocks"
  (if header-tests-on
      (let ((function-list))
	(save-excursion
	  (goto-char (point-max))
	  (if (search-backward "Function Change Records" nil 't) ; find list of functions
	      (if (search-forward "START:" nil 't) ;find the starting point
		  nil
		(insert-function-change-records))
	    (insert-function-change-records))
	  (setq function-list (read (current-buffer)))
	  (backward-sexp 1)
	  (setq function-list (update-function-tests-by-list function-list))
	  (save-excursion
	    (if function-list
		(prin1 function-list (current-buffer))
	      (insert "()"))
	    (kill-sexp 1))
	  (delete-matching-lines "^[ \t]*$")))))

(defun insert-function-change-records ()
  "Insert appropriate function change records"
  (let ((header-prefix-string (header-prefix-string)))
    (insert "\n\n\n\n")
    (insert comment-start)
    (fill-line-with-preceding-character)
    (insert "\n" header-prefix-string
	    " Function Change Records (for internal use only)\n"
	    header-prefix-string "   START:  ")
    (save-excursion (insert "()\n" comment-end))))
	

(defun update-function-tests-by-list (function-list)
  "Update all function tests"
  (let ((function)
	(funname)
	(fundata)
	(new-list ()))
    (save-excursion
      (goto-char (point-min))		;goto beginning of buffer
      (while
	  (setq function (find-next-c-function))
	(string-match "\\([_a-zA-Z][_a-zA-Z0-9:]*\\)[ \t]*(" function)
	(setf funname
	      (substring function (match-beginning 1) (match-end 1)))
	(setq fundata (find-function-data funname function-list))
	(setq new-list (cons (update-function-tests fundata funname)
			      new-list))))
    new-list))

(defun find-function-data (funname function-list)
  "Return the function data, or nil"
  (let ((temp))
    (while function-list
      (if (string= (car (car function-list))
		   funname)
	  (setq temp (car function-list)))
      (setq function-list (cdr function-list)))
    temp))
    

(defun update-function-tests (fundata funname)
  "Update the test data of a function, given name and data"
  (let ((cksum))
;;  (insert "stuff")
    (save-excursion
      (backward-sexp 1)
      (setq cksum (checksum-function)))
    (if (eq cksum (cdr fundata))
	nil
      (save-excursion
	(clear-function-test-flags funname)))
    (cons funname cksum)))



(defun checksum-function ()
  "Checksum the function after the current point"
  (let ((sum 0)
	(begin (point))
	(end (save-excursion (forward-sexp)
			     (point))))
    (while (< begin end)
      (if (eq (char-syntax (char-after begin)) 32)
	  nil
	(setq sum (+ sum (char-after begin))))
      (setq begin (1+ begin)))
    sum))


(defun clear-function-test-flags (funname)
  "Clear the test flags for a function"
  (let ((value))
    (setq value
	  (catch 'no-header 
	    (let ((beglimit (save-excursion
			      (if (not (search-backward (concat funname "() --") nil 't))
				  (throw 'no-header 'no-header))
			      (point)))
		  (limit))
	      (if (not (search-backward "TESTS AND COVERAGE" beglimit 't))
		  (throw 'no-header 'no-test-section))
	      (setq limit (save-excursion
			    (search-forward comment-end nil 't)
			    (point)))
	      (while (re-search-forward "_[^:]*_" limit t)
		(replace-match "__" nil nil)))))
    (cond
     ((eq value 'no-header)
	(message "No documentation header was found")
	(beep)
	(sleep-for 2))
     ((eq value 'no-test-section)
      (message "No test section found to update")))))


(provide 'testsup)