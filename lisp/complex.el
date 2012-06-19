;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; complex.el -- measure code complexity
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser
;; Created On      : Thu Mar 14 11:34:28 1996
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defmacro count-multiple-statements (&rest statements)
;;   (defun insert-mccabe-complexity ()
;;   (defun next-function-mccabe-complexity ()
;;   (defun count-statements (statement)
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: complex.el,v $
;; $Revision: 1.2 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (string-lessp emacs-version "19.30")
    (require 'backquote))

(defmacro count-multiple-statements (&rest statements)
  `(apply '+
	  (mapcar 'count-statements
		  ',statements)))

;;;###autoload
(defun insert-mccabe-complexity ()
  "Just that"
  (interactive)
  (let ((header-prefix (header-prefix-string))
	(ret (next-function-mccabe-complexity)))
    (insert
     header-prefix "MCCABE COMPLEXITY\n"
     header-prefix "  Complexity: " (int-to-string (car ret)) "\n"
     header-prefix "     Returns: " (int-to-string (cadr ret)) "\n"
     )))
    

;;;###autoload
(defun next-function-mccabe-complexity ()
  "Find the McCabe complexity of the following function"
  (save-excursion
    (let ((start (point))
	  complex
	  returns
	  switches
	  cases
	  (fnname (find-next-c-function))
	  (end (point)))
      (save-restriction
	(widen)
	(narrow-to-region start end)
	(let ((old-buf (current-buffer)))
	  (in-temp-buffer-same-mode
	   (insert-buffer old-buf)
	   (kill-all-comments)
	   (setq complex (count-multiple-statements "if"
						    "else"
						    "while"
						    "for"))
	   (incf complex)		;add default complexity
	   (setq returns (count-multiple-statements
			  "return"
			  "exit"
			  "throw"))
	   (setq switches (count-statements "switch"))
	   (setq cases (count-multiple-statements
			"case"
			"catch"
			"default")))
	  (list (+ complex cases) returns))))))

(defun count-statements (statement)
  "Count all of STATEMENT in reachable portion of buffer"
  (let ((count 0))
    (save-excursion
      (beginning-of-buffer)
      (while (re-search-forward
	      (concat
	       "\\b"
	       (regexp-quote statement)
	       "\\b")
	      nil 't)
	(incf count)))
    count))

      