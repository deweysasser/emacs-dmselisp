;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; classes.el -- support for documenting classes
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser
;; Created On      : Thu Mar 07 16:43:55 1996
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	This module will probably only every be applicable to c++ and
;; 	related  languages.  LISP classes would be trivially easy to
;; 	parse, and in any case would be have completely differen
;; 	symantics for public/private, and have no member variables.
;; 
;; TABLE OF CONTENTS
;;   (defstruct class
;;   (defun insert-class-documentation (class)
;;   (defun insert-class-member-functions (class)
;;   (defun insert-class-member-function-type (class type)
;;   (defun insert-class-member-variables (class))
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: classes.el,v $
;; $Revision: 1.2 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; will have to go back through this to watch out for name space
;; conflicts if I end up writing CLOS

(require 'cl)

(defstruct class
  name
  superclasses				;a list
  member-functions			;alist of ((type  elements)
					; (type elements)
  member-variables
  )

(defun insert-class-documentation (class)
  "Insert class documentation into the current buffer"
  (let ((prefix (header-prefix-string)))
    (make-divisor)
    (insert prefix (class-name class) " -- \n")
    (insert prefix "SUPERCLASSES: ")
    (mapcar '(lambda (name)
	       (insert " " name " "))
	       (class-superclasses class))
    (insert "\n")
    (insert-class-member-functions class)
    (insert-class-member-variables class)))

(defun insert-class-member-functions (class)
  "Insert all member functions into the buffer"
  (mapcar '(lambda (type)
	     (insert-class-member-function-type class type))
	  '(public protected private)))

(defun insert-class-member-function-type (class type)
  "Just that"
  (let ((type-name (upcase (symbol-name type)))
	(member-functions (cadr (assq type (class-member-functions class)))))
    (insert prefix type-name " MEMBER FUNCTIONS:\n")
    (mapcar '(lambda (name)
	       (insert prefix name "\n"))
	    member-functions)))

(defun insert-class-member-variables (class))
