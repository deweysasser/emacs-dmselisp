;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; timecats.el -- timecat categories
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser <dewey@newvision.com>
;; Created On      : Mon Jun 30 14:44:45 1997
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defvar timecat-category-directory "~/.timesheet-info"
;;   (defvar timecat-categories (make-vector 5 nil)
;;   (defvar timecat-starting-category nil
;;   (defstruct category
;;   (defstruct category-value
;;   (defmacro timecat::defcategory (name cats &rest options)
;;   (defun* timecat::define-category (name cats &key sub-category)
;;   (defun timecat::build-values (value)
;;   (defun timecat::defstart (name)
;;   (defun timecat::define-starting-category (name)
;;   (defun timecat::get-category ()
;;   (defun timecat::lookup-category (category)
;;   (defun timecat::complete-values (category)
;;   (defun timecat:insert-category ()
;;   (defun timecat:load-data ()
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: timecats.el,v $
;; $Revision: 1.3 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)
(require 'ptmacros)

(defvar timecat-category-directory "~/.timesheet-info"
  "Where to store categories, etc")

(defvar timecat-categories (make-vector 5 nil)
  "Named alist of subcategories.  Each entry is a list of possible
subcategories.")

(defvar timecat-starting-category nil
  "Where to start")

(defstruct category
  "A category"
  name
  values
  sub-category)

(defstruct category-value
  "A possible value for a category"
  name
  description
  sub-category)

(defmacro timecat::defcategory (name cats &rest options)
  "Define a category named by NAME containting categories CATS"
  `(apply 'timecat::define-category ',name ',cats ',options))

(put 'timecat::defcategory 'lisp-indent-function 1)

(defun* timecat::define-category (name cats &key sub-category)
  "Define a category"
  (let (sym
	(values (make-vector 50 nil)))
    (mapcar 'timecat::build-values cats)
    (setq sym (intern (symbol-name name) timecat-categories))
    (setf (symbol-value sym)
	  (make-category :name (symbol-name name) :values values
			 :sub-category sub-category))))

(defun timecat::build-values (value)
  "construct the values"
  (declare (special values))
  (if (atom value)
      (setf (symbol-value (intern value values)) t)
    (let ((name (car value)))
      (push ':name value)
      (setf (symbol-value
	     (intern name values))
	    (apply 'make-category-value value)))))

(defun timecat::defstart (name)
  `(timecat::define-starting-category ,name))

(defun timecat::define-starting-category (name)
  "Define the starting category"
  (setq timecat-starting-category name))

(defun timecat::get-category ()
  "Allow the user to specify a category"
  (let (value
	values
	next
	sym
	(current-category (timecat::lookup-category timecat-starting-category)))
    (while (if current-category
	       (progn
		 (setq value (timecat::complete-values current-category))
		 (push value values)
		 (setq sym (intern-soft value
					(category-values current-category)))
		 (setq current-category
		       (timecat::lookup-category
			(or
			 (and (category-value-p (symbol-value sym))
			      (category-value-sub-category (symbol-value sym)))
			 (category-sub-category current-category)))))))
    (nreverse values)))

(defun timecat::lookup-category (category)
  "Lookup the category by name and return it"
  (if (symbolp category)
      (timecat::lookup-category (symbol-name category))
    (symbol-value (intern-soft category timecat-categories))))

(defun timecat::complete-values (category)
  "Complete on the values in the current category"
  (completing-read "which-category? " (category-values category)
		   (function (lambda (x) (symbol-value x)))
		   t)
  )

(defun timecat:insert-category ()
  "Insert a time category"
  (interactive)
  (insert (join-strings ":" (timecat::get-category))))


(defun timecat:load-data ()
  "Load the category data"
  (interactive)
  (load-file (concat timecat-category-directory "/" "categories")))

(if (not (featurep 'timecats))
    (timecat:load-data))


(provide 'timecats);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;  Function Change Records (for internal use only)
;;    START:  ()
