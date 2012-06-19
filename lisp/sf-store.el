;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sf-store.el -- superfun storage
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser <dewey@newvision.com>
;; Created On      : Wed Feb 19 17:40:27 1997
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defvar sf::object-file "objects.db"
;;   (defvar sf::object-buffer nil
;;   (defvar sf::classes (make-hash-table :test 'equal)
;;   (defvar sf::functions (make-hash-table :test 'equal)
;;   (defvar sf::in-store-object nil
;;   (defmacro with-cl-reader (&rest body)
;;   (defun object-to-section (object)
;;   (defun type-to-section (type)
;;   (defmacro sf::in-section (buffer-or-string section &rest body)
;;   (defun narrow-to-section (section)
;;   (defcmethod store-object ((obj sf::program-object))
;;   (defun sf::read-program-object (type name buffer-or-string)
;;   (defun sf::write-program-object (object name buffer-or-string)
;;   (defun sf::find-object-buffer ()
;;   (defcmethod save-object ((object sf::program-object))
;;   (defun read-object (name type)
;;   (defcmethod hook-into-maps ((object sf::class))
;;   (defcmethod hook-into-maps ((object sf::function))
;;   (defcmethod sf::flush-object ((object sf::program-object))
;;   (defcmethod sf::remove-from-maps ((object sf::class))
;;   (defcmethod sf::remove-from-maps ((object sf::function))
;;   (defun sf::ensure-class (name)
;;   (defun sf::ensure-function (name)
;;   (defun sf::find-class-member-function (class function)
;;   (defun sf::ensure-member-function (class function)
;;   (defcmethod initialize-instance ((object sf::member-function))
;;   (defun sf::class-add-member-function (class function)
;;   (defun sf::flush-all-functions ()
;;   (defun sf::flush-all-classes ()
;;   (defcmethod print-object ((obj sf::program-object))
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: sf-store.el,v $
;; $Revision: 1.3 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sf-objs)

(defvar sf::object-file "objects.db"
  "*Name of the Object Database")

(defvar sf::object-buffer nil
  "The object buffer")

(defvar sf::classes (make-hash-table :test 'equal)
  "all classes")

(defvar sf::functions (make-hash-table :test 'equal)
  "all functions")

(make-variable-buffer-local 'sf::object-buffer)

(defvar sf::in-store-object nil
  "Hack to determine if we've been called by store object")

(defmacro with-cl-reader (&rest body)
  `(let ((cl-read-active 't))
     ,@body))

(defun object-to-section (object)
  (cond
   ((sf::class-p object)
    "classes")
   ((sf::function-p object)
    "functions")))

(defun type-to-section (type)
  (case type
    ('class "classes")
    ('function "functions")))

(defmacro sf::in-section (buffer-or-string section &rest body)
  "Execute BODY with current buffer narrowed to SECTION of
BUFFER-OR-STRING"
  (let ((old-buf (gensym)))
    `(let ((,old-buf (current-buffer)))
       (unwind-protect
	   (progn
	     (set-buffer (get-buffer buffer-or-string))
	     (set-window-buffer (selected-window) (current-buffer))
	     (narrow-to-section section)
	     ,@body)
	 (widen)
	 (set-window-buffer (selected-window) ,old-buf)
	 (set-buffer ,old-buf)))))

(defun narrow-to-section (section)
  "Narrow the current buffer to the appropriate section"
  (let ((bkey (concat "beginning of " section))
	(ekey (concat "end of " section))
	start)
    (goto-char (point-min))
    (if (search-forward bkey nil 't)
	(progn
;	  (forward-char 1)
	  (setq start (point))
	  (search-forward ekey)
	  (beginning-of-line)
	  (forward-char -1)
	  (narrow-to-region start (point)))
      (insert "(" section " ; " bkey "\n")
      (setq start (point))
      (insert ") ;" ekey "\n")
      (narrow-to-region start start))))


(defcmethod store-object ((obj sf::program-object))
  (let* ((sf::in-store-object 't)
	 temp
	 (name (cclass-name (cclass-of obj)))
	 (slots (cclass-slots (cclass-of obj)))
	 values)
    (setq values (loop for x in slots
		       if (setq temp (slot-value obj x))
		       nconc (list (intern (format ":%s" x)) (list 'quote temp))))
    (push (list 'quote name) values)
    (push 'make-instance values)
    (princ (intern "#."))
    (prin1 values)))

(defun sf::read-program-object (type name buffer-or-string)
  "Read a program object from one of the sections of BUFFER-OR-STRING"
  (let* ((name-key (format ";; %s" name))
	 (name-key-regexp (concat "^" (regexp-quote name-key) "$"))
	 (section (type-to-section type)))
    (sf::in-section
     buffer-or-string section
     (with-cl-reader
      (goto-char (point-min))
      (if (re-search-forward name-key-regexp nil 't)
	  (progn
	    (beginning-of-line)
	    (read (current-buffer))))))))

(defun sf::write-program-object (object name buffer-or-string)
  "write a program object"
  (let* ((name-key (format ";; %s" name))
	 (name-key-regexp (concat "^" (regexp-quote name-key) "$"))
	 (section (object-to-section object)))
    (sf::in-section
     buffer-or-string section
     (let ((standard-output (current-buffer)))
       (goto-char (point-min))
     (if (re-search-forward name-key-regexp nil 't)
	 (progn
	   (forward-char 1)
	   (kill-sexp 1))
       (end-of-buffer)
       (princ "\n")
       (princ name-key)
       (princ "\n")
;       (forward-char -1)
       )
     (store-object object)
     ))))


(defun sf::find-object-buffer ()
  "Return the object buffer"
  (if (and
       sf::object-buffer
       (bufferp sf::object-buffer))
      sf::object-buffer
    (let (buf
	  (default-directory default-directory))
      (while (not (or (string= default-directory "")
		      (file-readable-p sf::object-file)))
	(setf default-directory (file-name-directory
				 (directory-file-name default-directory))))
      ;; at this point, default directory is the directory or ""
      (setq sf::object-buffer
	    (find-file-noselect sf::object-file)))))


(defcmethod save-object ((object sf::program-object))
  "Save a function object"
   (sf::write-program-object object (sf::program-object-name object)
			     (sf::find-object-buffer)))


(defun read-object (name type)
  "Restore an object"
  (let ((object
	 (sf::read-program-object type name
				  (sf::find-object-buffer))))
    (if object
	(hook-into-maps object))))

(defcmethod hook-into-maps ((object sf::class))
  "Hook a class (and all member functions) into global maps"
  (setf (gethash (sf::program-object-name object) sf::classes) object))

(defcmethod hook-into-maps ((object sf::function))
  "Hook a function into global maps"
  (setf (gethash (sf::program-object-name object) sf::functions) object))

(defcmethod sf::flush-object ((object sf::program-object))
  "Flush an object out to the database file"
  (save-object object)
  (sf::remove-from-maps object))

(defcmethod sf::remove-from-maps ((object sf::class))
  "Remove the class from the map"
  (remhash (sf::program-object-name object) sf::classes))

(defcmethod sf::remove-from-maps ((object sf::function))
  "Remove the class from the map"
  (remhash (sf::program-object-name object) sf::functions))

(defun sf::ensure-class (name)
  "Return the class named NAME, creating it or recovering it from the
database"
  (or
   (if (sf::class-p name)
       name)
   (gethash name sf::classes)
   (read-object name 'class)
   (hook-into-maps (make-instance 'sf::class :name name))))

(defun sf::ensure-function (name)
  "Ensure that function NAME exists, reading it from the object store
if necessary"
  (or
   (if (sf::function-p name)
       name)
   (gethash name sf::functions)
   (read-object name 'function)
   (hook-into-maps (make-instance 'sf::function :name name))))

(defun sf::find-class-member-function (class function)
  "Find a member function in a class"
  (let ((class (sf::ensure-class class)))
    (loop for x in (sf::class-member-functions class)
	  if (or
	      (eq function x)
	      (equal (sf::member-function-name x) function))
	  return x)))

(defun sf::ensure-member-function (class function)
  "Find the member function, creating it if necessary"
  (let ((class (sf::ensure-class class)))
    (or
     (sf::find-class-member-function class function)
     (make-instance 'sf::member-function :name function :class
		    (sf::class-name class)))))

(defcmethod initialize-instance ((object sf::member-function))
  "Initialize a member function"
  (if (sf::member-function-class object)
      (let ((class (sf::ensure-class (sf::member-function-class
				      object))))
	(sf::class-add-member-function class object))))

(defun sf::class-add-member-function (class function)
  "Add function to class if it's not there already"
  (if (not (sf::find-class-member-function class function))
      (push function (sf::class-member-functions class))))

(defun sf::flush-all-functions ()
  "Flush all functions to data store"
  (interactive)
  (maphash #'(lambda (key value)
	       (sf::flush-object value))
	   sf::functions)
  (save-buffer (sf::find-object-buffer)))


(defun sf::flush-all-classes ()
  "Flush all classes to data store"
  (interactive)
  (maphash #'(lambda (key value)
	       (sf::flush-object value))
	   sf::classes)
  (save-buffer (sf::find-object-buffer)))

(defcmethod print-object ((obj sf::program-object))
  "Print a program object"
  (if sf::in-store-object
      (store-object obj)
    (print-unreadable-object (format "%s %s" (cclass-name (cclass-of obj))
				     (sf::program-object-name obj)))))
  