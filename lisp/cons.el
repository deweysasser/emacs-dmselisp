;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cons.el -- c constructors
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser
;; Created On      : Sun Apr 09 15:34:15 1995
;; Status          : $State: Exp $
;; Name            : $Name: HEAD $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; NOTES
;;      This file is not currently used
;; 
;; TABLE OF CONTENTS
;;   (defun fill-constructor ()
;;   (defun insert-constructor-contents (theFunc)
;;   (defun insert-copy-args (theName theList)
;;   (defun return-copy-constructor (name ret-type)
;; 
;;  OPTIONS
;;    Update Tests:  no
;; 
;; $RCSfile: cons.el,v $
;; $Revision: 1.6 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun fill-constructor ()
  "Fill a function body appropriately"
  (interactive)
  (let ((theFunc (save-excursion (find-c-function-by-parts))))
    (search-forward "{" )
    (forward-char 1)
    (insert-constructor-contents theFunc)
    (beginning-of-defun)
    (indent-c-exp)))



(defun insert-constructor-contents (theFunc)
  "Insert into at the current point a constructor based on arg THEFUNC"
  (let* ((ret-type (function-get-return-type theFunc))
	(name (concat "the" ret-type)))
    (insert
     ret-type " the" ret-type "=calloc(1,sizeof(struct S_" ret-type "));\n")
    (insert
     "ThrowIfNULL(the" ret-type");\n\n")

    (insert-copy-args (concat "the" ret-type)
		      (function-get-args theFunc))
    (insert "return " name ";\n")))


(defun insert-copy-args (theName theList)
  (if (not theList)
      nil
    (let ((ret-type (argument-get-type (car theList)))
	  (name (argument-get-name (car theList))))
      (insert
       theName "->" name "=" (return-copy-constructor name ret-type) ";\n")      
      (insert-copy-args theName (cdr theList)))))


(defun return-copy-constructor (name ret-type)
  "Return way to copy type"
  (concat ret-type "__Copy(" name ")"))




    