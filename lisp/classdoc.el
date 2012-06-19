;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; classdoc.el -- c++ class documentation functions
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser
;; Created On      : Fri Jun 28 10:43:44 1996
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defstruct class
;;   (defvar default-class-documentation-alist
;;   (defvar class-documentation-order-list '(name "AUTHOR" "VISIBILITY"
;;   (defun document-class ()
;;   (defmodemethod document-class-internal c++-mode ()
;;   (defun insert-class-documentation (class)
;;   (defun class-insert-or-update-field (field list class)
;;   (defun get-buffer-match (arg)
;;   (defmodemethod find-class-in-buffer c++-mode ()
;;   (defun find-class-functions (class start end)
;;   (defun get-class-extents ()
;;   (defun parse-c++-superclasses (arg)
;;   (defun get-superclasses-matching (match list)
;;   (defun class-insert-name (class value)
;;   (defun class-supers-value (class value)
;;   (defun member-functions-value (class value)
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: classdoc.el,v $
;; $Revision: 1.10 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'cl)
(require 'modefn)

(eval-when (eval compile)
  (require 'ptmacros))

(defstruct class
  name
  superclasses				; a list of strings
  member-functions			; alist of ((type  elements)
					; (type elements)
  member-variables
  )

(defvar default-class-documentation-alist
      '((name
	 ((update-function
	   class-insert-name)
	  (lines special)))
	("AUTHOR"
	 ((update-function doc-insert-author)	 
	  (lines single)))
	("VISIBILITY"
	 ((lines single)
	  (value "PUBLIC | PRIVATE")))
	("PUBLIC SUPERCLASSES"
	 ((update-function
	   class-supers-value)
	  (lines single)))
	("RESPONSIBILITY"
	 ((value "|><|")
	  (lines multi)))
	("COLLABORATORS"
	 ((value "|><|")
	  (lines multi)))
	("DESCRIPTION"
	 ((value "|><|")
	  (lines multi)))
	("CONSTRUCTION"
	 ((value "|><|")
	  (lines multi)))
	("OPERATIONS"
	 ((value "|><|")
	  (update-function member-functions-value)
	  (lines multi)))
	("NOTES"
	 ((value "|><|")
	  (lines multi)))
	("SEE ALSO"
	 ((value "|><|")
	  (lines multi)))
	("CAVEATS AND BUGS"
	 ((value "|><|")
	  (lines multi)))
	("FORMAT VERSION"
	 ((value "0.0")
	  (lines single)))
	)
      )
;;   "property list for fields.  This list can be managed by functions set-list-propberty and get-list-property.  List consists of property name - value pairs for various field names.  Currently used properties include:
;; 
;; 'value -- value of the field
;; 'inclusion-function -- a function that will be called to test whether
;;                       the field should be included in the
;;                       documentation
;; 'update-function -- a function that will be called with the function
;;                     (returned by (make-function)) and a value, and
;;                     should return appropriate value to insert."
;; )

(defvar class-documentation-order-list '(name "AUTHOR" "VISIBILITY"
					    "PUBLIC SUPERCLASSES"
					    "RESPONSIBILITY"
					    "COLLABORATORS"
					    "DESCRIPTION"
					    "CONSTRUCTION"
					    "OPERATIONS" "NOTES"
					    "SEE ALSO"
					    "CAVEATS AND BUGS" "FORMAT VERSION"))

;;;###autoload(autoload 'mconcat "classdoc" nil nil 'macro)

(eval-when (compile load eval)
  (defmacro mconcat (&rest args)
    (apply 'concat (mapcar 'eval args)))

  (defconst c-token-regexp "[a-zA-Z_][a-zA-Z_0-9]*"
    "Regular expression that matches a C or C++ token")

  (defconst class-function-regexp
    (mconcat
     "\\("   c-token-regexp "\\)"
     "(")
    "Regular expression that matches a C++ member function in a class
definition."))


;;;###autoload
(defun document-class ()
  "Put in documentation template for C++ class"
  (interactive)
  (document-class-internal))

(defmodemethod document-class-internal c++-mode ()
  "Document a C++ class"
  (let ((class (save-excursion (find-class-in-buffer))))
    (if class
	(insert-class-documentation class))))

(defun insert-class-documentation (class)
  "Insert the class documentation"
  (let ((start (set-marker (make-marker) (point)))
	(header-prefix (header-prefix-string)))
    (insert comment-start)
    (fill-line-with-preceding-character)
    (insert "\n")
    (mapcar (lambda (field)
	      (class-insert-or-update-field field
					    default-class-documentation-alist
					    class))
	    class-documentation-order-list)
    (insert comment-end)
    (goto-char start)
    (set-marker start nil)
    (forward-line 1)
    (end-of-line)))


(defun class-insert-or-update-field (field list class)
  "Insert a field into the buffer, possibly updating the fields value
from existing information.

FIELD -- name of field to insert
LIST -- property list of fields
CLASS -- a function created by (make-function)
"
  (let ((start)
	(value-end)
	(value-start)
	(inclusion (get-list-property list field 'inclusion-function))
	(linetype (get-list-property list field 'lines))
	(fn (get-list-property list field 'update-function))
	(value (get-list-property list field 'value)))
    (declare (special header-prefix))
    (if (include-fieldp inclusion field list class)
	(progn
	  (setq start (point))
	  (insert (if (stringp field)
		      field
		    ""))
	  (setq value-start
		(insert-value linetype
			      (if fn
				  (funcall fn class value)
				value)))
	  (insert-box start (point) header-prefix)))))


(defun get-buffer-match (arg)
  (if (and
       (match-beginning arg)
       (match-end arg))
      (buffer-substring (match-beginning arg)
			(match-end arg))
    ""))
  

(defmodemethod find-class-in-buffer c++-mode ()
  "Find the next class in the buffer and parse it, returning a CLASS
object representing that class"
  (let (class)
    (if (re-search-forward
	 (mconcat
	  "class[ \t]*"
	  "\\("	c-token-regexp	"\\)"
	  "\\("	"[ \t]*:[ \t]*" "\\(.*\\)" "\\)?")
	 nil 't)
	(progn
	  (setq class (make-class :name (get-buffer-match 1)
				  :superclasses (parse-c++-superclasses
						 (get-buffer-match 3))))
	  (multiple-value-bind (start end)
	      (get-class-extents)
	    (find-class-functions class start end))
	  class))))

(defun find-class-functions (class start end)
  "find all member functions of a class"
  (let (list)
    (goto-char start)
    (while (search-forward-regexp
	    (mconcat
	     "\\(" class-function-regexp "\\)") end 't)
      (let (( match (get-buffer-match 2)))
	(if (not (string= match (class-name class)))
	    (prepend-to-list list (cons "" (get-buffer-match 2)))))
    (setf (class-member-functions class) list))))

(defun get-class-extents ()
  "Return the extents of a class"
  (save-excursion
  (let (start)
    (if (search-forward "{" nil 't)
      (progn
	(backward-char 1)
	(setq start (point))
	(forward-sexp)
	(values start (point)))))))
  


(defun parse-c++-superclasses (arg)
  "Parse the C++ superclasses"
  (let (list)
    (in-temp-buffer
     (insert arg)
     (beginning-of-buffer)
     (while (re-search-forward "[ \t]*,[ \t]*" nil 't)
       (replace-match "\n"))
     (end-of-buffer)
     (insert "\n")
     (beginning-of-buffer)
     (while (< (point) (point-max))
       (if (looking-at
	    (mconcat
	     "\\(public\\|private\\|protected\\)[ \t]+"
	     "\\(" c-token-regexp "\\)"))
	   (prepend-to-list list
			    (cons (get-buffer-match 1)
				  (get-buffer-match 2))))
       (forward-line 1)))
    list))



(defun get-superclasses-matching (match list)
  (mapcar '(lambda (arg)
	     (if (equal match (car arg))
		 (cdr arg)))
	  list))

(defun class-insert-name (class value)
  (concat "class " (class-name class) " -- "
	  (if value
	      value
	    "")))


(defun class-supers-value (class value)
  (mapconcat '(lambda (arg)
		      arg)
		  (get-superclasses-matching
		   "public" (class-superclasses class))
		  ", "))

(defun member-functions-value (class value)
    (mapconcat '(lambda (arg) (concat (cdr arg) "() -- "))
			  (class-member-functions class)
			  "\n"))

(provide 'classdoc)
