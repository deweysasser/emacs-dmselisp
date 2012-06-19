;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; func-sup.el -- functions supplemental
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser <dewey@newvision.com>
;; Created On      : Wed Feb 19 11:36:06 1997
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defvar class::keep-track nil
;;   (defvar class::all-classes (make-hash-table :test 'equal)
;;   (defun track-class (class)
;;   (defun track-function (function)
;;   (defun find-header-end (prefix-string)
;;   (defun is-blank-field (field)
;;   (defvar program-object-key-table '(("class" . class)
;;   (defvar program-object-creator '((function . top-find-function-by-parts)
;;   (defun find-program-object ()
;;   (defcmethod class-find-member-function ((class class) (function string))
;;   (defcmethod class-find-member-function ((class class) (function function))
;;   (defcmethod class-find-member-function ((class string) function)
;;   (defcmethod class-find-class ((class string))
;;   (defun class-add-member-function (class function)
;;   (defcmethod class-remove-member-function ((class class)
;;   (defun function-add-self-to-class (function)
;;   (defun ensure-class (class-name)
;;   (defun ensure-member-function (function-name class-name)
;;   (defun snarf-program-objects (file)
;;   (defmacro with-independant-classes (&rest body)
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: func-sup.el,v $
;; $Revision: 1.3 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq field-structure-parsers '(("ARGUMENTS" . field-get-arguments)))

(defvar class::keep-track nil
  "*'t means keep track of classes when documenting")

(defvar class::all-classes (make-hash-table :test 'equal)
  "All the classes, hashed")

;;;###autoload
(defun track-class (class)
  "Track the class if that option is set"
  (if class::keep-track
      (setf (gethash (class-name class) class::all-classes) class)))

(defun track-function (function)
  "Track the function if appropriate"
  (if class::keep-track
      (if (member-function-p function)
	  (function-add-self-to-class function)))
  function)



(defun find-header-end (prefix-string)
  "Return the point at the end of the header"
  (save-excursion
    (if (looking-at (regexp-quote (trim-whitespace
				   comment-start)))
	(forward-line 1))
    (while (looking-at prefix-string)
      (forward-line 1))
    (if (looking-at (regexp-quote comment-end))
	(forward-line 1))
    (point)))
    



(defun is-blank-field (field)
  (or (not field)
      (string= "" field)
      (string= "|><|" field)))

(defvar program-object-key-table '(("class" . class)
				   ("#define" . define))
  "*Table to look up what type is the next object based on the first word")

(defvar program-object-creator '((function . top-find-function-by-parts)
				 (class . find-class-in-buffer))
  "*Table of functions to use to create various type program objects")

(defun find-program-object ()
  "Return the next object in the buffer"
  (let* ((word (save-excursion
		(skip-chars-forward " \t\n")
		(word-at-point)))
	 (object-type (assoc word program-object-key-table))
	 (function (if object-type
		       (assoc (cdr object-type) program-object-creator))))
    (if function (funcall (cdr function)))))
	

(defcmethod class-find-member-function ((class class) (function string))
  "Locate and return the appropriately named member function"
  (loop for x in (class-member-functions class)
	if (string= function (function-name x))
	return x))

(defcmethod class-find-member-function ((class class) (function function))
  "Locate and return the appropriately named member function"
  (let ((function (function-name function)))
    (loop for x in (class-member-functions class)
	  if (string= function (function-name x))
	  return x)))

(defcmethod class-find-member-function ((class string) function)
  "Locate and return the appropriately named member function"
  (let ((real-class (ensure-class class)))
    (class-find-member-function real-class function)))

(defcmethod class-find-class ((class string))
  "Find the class named CLASS"
  (gethash class class::all-classes))

(defun class-add-member-function (class function)
  "Add a member function to a class"
  (let ((func (class-find-member-function class function)))
    (if func
	(if (eq func function)
	    nil
	  (class-remove-member-function class func)
	  (setf (member-function-visibility function)
		(member-function-visibility func))
	  (class-add-member-function class function))
      (push function (class-member-functions class)))))

(defcmethod class-remove-member-function ((class class)
					  (function member-function))
  "Remove a member function object from a class object"
  ;; this function probably produces too much garbage and could be
  ;; done more efficiently 
  (setf (class-member-functions class)
	(loop for x in (class-member-functions class)
	      if (not (eq x function))
	      collect x)))

(defun function-add-self-to-class (function)
  "Add self to it's class"
  (if (function-class function)
    (let ((class (ensure-class (function-class function))))
      (class-add-member-function class function))))

(defun ensure-class (class-name)
  "Ensure that such a class exists"
  (let ((class (class-find-class class-name)))
    (if class
	class
      (track-class (make-class :name class-name)))))

(defun ensure-member-function (function-name class-name)
  "Ensure that such a member function exists"
  (let* ((class (ensure-class class-name))
	 (function (class-find-member-function class function-name)))
    (if function
	function
      (setq function (make-member-function :name function-name :class class-name))
      (function-add-self-to-class function)
      function)))

(defun snarf-program-objects (file)
  "Snarf all program objects from file"
  (interactive "f")
  (flet ((find-object-header
	  ()
	  (if (get-next-function-description)
	   (forward-line -1)
	   (beginning-of-line))))
    (in-temp-buffer
     (insert-file-contents file 't)
     (set-auto-mode)
     (beginning-of-buffer)
     (debug)
     (let (field obj)
       (while (find-object-header)
	 (setq field (get-fields default-documentation-alist))
	 (if (setq obj (find-program-object))
	     (push (cons ':documentation field)
		   (program-object-options obj))))))))

;;(defun end-of-file-header ()
;;  "go to end of file header"
;;  (let ((qprefix (regexp-quote (trim-whitespace (header-prefix-string)))))
;;    (while (looking-at qprefix)
;;      (forward-line 1))))

(defmacro with-independant-classes (&rest body)
  "Separate these read classes from normal ones"
  `(let ((class::all-classes (make-hash-table :test 'equal))
	 (class::keep-track 't))
     ,@body))