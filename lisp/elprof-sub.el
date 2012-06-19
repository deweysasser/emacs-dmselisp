;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elprof-sub.el -- profiling support
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser <dewey@newvision.com>
;; Created On      : Tue Feb 18 10:31:45 1997
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defvar elprof::profile-snarfers nil
;;   (defmacro* elprof::add-snarfer (symbol function)
;;   (defmacro while-visiting (file &rest body)
;;   (defun profile-find-functions ()
;;   (defun profile-files (files)
;;   (defun profile-file (file)
;;   (defun profile-clear-functions ()
;;   (defun profile-add-this-file ()
;;   (defun profile-this-file ()
;;   (defun elprof::is-function (x)
;;   (defun elprof::function-name (x)
;;   (defun elprof::get-defun (x)
;;   (defun sort-profile-by-average-time ()
;;   (defun sort-profile-by-number-of-calls ()
;;   (defun sort-profile-by-total-time ()
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: elprof-sub.el,v $
;; $Revision: 1.2 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'elprof)
(require 'cl)

(defvar elprof::profile-snarfers nil
  "List of functions to return name of function to profile
Form is (symbol . function)")

(defmacro* elprof::add-snarfer (symbol function)
  `(push (cons ',symbol (function ,function)) elprof::profile-snarfers))

(defmacro while-visiting (file &rest body)
  `(progn
     (save-excursion
       (in-temp-buffer
	(insert-file-contents ,file 't)
	(set-auto-mode)
	,@body))))
       

(defun profile-find-functions ()
  "Find all functions in current buffer"
  (let (functions
	(standard-input (current-buffer)))
    (loop for x = (condition-case x (read) ('error nil))
	  if (not x) return functions
	  if (elprof::is-function x)
	  do (push (elprof::function-name x) functions))
    functions))
  
(defun profile-files (files)
  "Profile a list of files"
  (let ((functions
	 (remove-duplicates
	  (mapcan #'(lambda (x)
		     (while-visiting x
				     (profile-find-functions)))
		     files))))
    (setq elprof-function-list (union elprof-function-list functions))))

(defun profile-file (file)
  "Profile a single file"
  (interactive "f")
  (profile-files (list file)))

(defun profile-clear-functions ()
  "Clear the list"
  (interactive)
  (elprof-restore-list)
  (setq elprof-function-list nil))

(defun profile-add-this-file ()
  "Collect all functions in this file and add them to the profile list"
  (interactive)
  (let ((list (profile-find-functions)))
    (setq elprof-function-list (remove-duplicates (append
						elprof-function-list
						list)))))

(defun profile-this-file ()
  "Collect all the functions in this file into profile-functions-list"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (setq elprof-function-list
	  (union elprof-function-list
	  (remove-duplicates (profile-find-functions))))))

(defun elprof::is-function (x)
  (assoc (first x) elprof::profile-snarfers))

(defun elprof::function-name (x)
  (let ((function (assoc (first x) elprof::profile-snarfers)))
    (if function
	(funcall (cdr function) x)
      (error "No snarfer for %s" x))))

(defun elprof::get-defun (x)
  (cadr x))

(if elprof::profile-snarfers
    nil
  (elprof::add-snarfer defun elprof::get-defun)
  (elprof::add-snarfer defun* elprof::get-defun)
  (elprof::add-snarfer defcmethod (lambda (x) (cadr x)))
  (elprof::add-snarfer defmodemethod
		       (lambda (x)
			 (let ((name (cadr x))
			       (mode (caddr x)))
			   (intern (concat (symbol-name mode)
					   "$"
					   (symbol-name name)))))))

(defun sort-profile-by-average-time ()
  "Sort an elp list by average time"
  (interactive)
  (save-excursion
    (let ((start (progn (goto-char (point-min))
			(forward-line 2)
			(point))))
      (sort-numeric-fields 4 start (point-max))
      (reverse-region start (point-max)))))

(defun sort-profile-by-number-of-calls ()
  "Sort an elp list by average time"
  (interactive)
  (save-excursion
    (let ((start (progn (goto-char (point-min))
			(forward-line 2)
			(point))))
      (sort-numeric-fields 2 start (point-max))
      (reverse-region start (point-max)))))

(defun sort-profile-by-total-time ()
  "Sort an elp list by average time"
  (interactive)
  (save-excursion
    (let ((start (progn (goto-char (point-min))
			(forward-line 2)
			(point))))
      (sort-numeric-fields 3 start (point-max))
      (reverse-region start (point-max)))))
