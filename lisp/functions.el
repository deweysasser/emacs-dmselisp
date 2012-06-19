;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions.el -- stuff to deal with functions
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser
;; Created On      : Tue Apr 25 10:13:08 1995
;; Status          : $State: Exp $
;; Name            : $Name: HEAD $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defvar default-documentation-alist
;;   (defvar documentation-order-list '(name "CLASS" "AUTHOR" "VISIBILITY" "SYNOPSIS" "DESCRIPTION" "ARGUMENTS" "RETURNS" "NOTES" "SEE ALSO" "CAVEATS AND BUGS" "TESTS AND COVERAGE" "FORMAT VERSION"))
;;   (defvar c++-documentation-order-list '(name "CLASS" "AUTHOR" "VISIBILITY" "SYNOPSIS" "DESCRIPTION" "ARGUMENTS" "RETURNS" "EXCEPTIONS" "NOTES" "SEE ALSO" "CAVEATS AND BUGS" "TESTS AND COVERAGE" "FORMAT VERSION"))
;;   (defvar allow-internal-comments 't
;;   (defconst c-token vb-token
;;   (defvar functions-hit-threshold 5
;;   (defstruct function
;;   (defstruct argument
;;   (defstruct (array-argument (:include argument))
;;   (defmacro compile-concat (&rest args)
;;   (defmacro match-buffer-substring-no-properties (matchnum)
;;   (defmacro safe-match-buffer-substring-no-properties (matchnum)
;;   (defmacro match-substring (string matchnum)
;;   (defun default-to (default value)
;;   (defun function-staticp (theFunc)
;;   (defun top-find-function-by-parts ()
;;   (defmodemethod find-function-by-parts-internal-comments default ()
;;   (defmodemethod find-function-by-parts-internal-comments perl-mode ()
;;   (defmodealias find-function-by-parts-internal-comments perl-mode
;;   (defmodemethod  find-function-by-parts c-mode ()
;;   (defun c-mode-macro-find-function-by-parts ()
;;   (defun split-string-on-sub (string sub &optional trim-ws)
;;   (defun c-mode-real-function-find-function-by-parts ()
;;   (defun make-c-argument-list (arg)
;;   (defmodealias  find-function-by-parts c-mode find-function-by-parts  c++-mode)
;;   (defmodemethod find-function-by-parts perl-mode ()
;;   (defun find-next-brace ()
;;   (defun list-perl-function-args ()
;;   (defun find-next-perl-arg ()
;;   (defun enter-arg-hash (arg)
;;   (defun split-perl-args (regexp line)
;;   (defmodemethod functions-search-for-header ms-visual-test-mode ()
;;   (defmodemethod functions-search-for-header perl-mode ()
;;   (defun set-property (field key value)
;;   (defun copy-proplist (theList)
;;   (defun set-list-property-fun (list field key value)
;;   (defmacro set-list-property (list field key value)
;;   (defun get-property (field key)
;;   (defun get-list-property (list field key)
;;   (defun document-function (delete-old)
;;   (defun document-c-function (delete-old)
;;   (defun include-fieldp (inc field list func)
;;   (defun insert-or-update-field (field list func)
;;   (defun insert-value (linetype value)
;;   (defun contains-description (value)
;;   (defun multiple-newlinesp (string)
;;   (defun should-indent-value (linetype)
;;   (defun doc-insert-author (function value)
;;   (defun doc-insert-name (function value)
;;   (defun get-function-return-type-string (function value)
;;   (defun remove-keywords (string)
;;   (defmodemethod get-function-args default (function value)
;;   (defmodemethod get-function-args perl-mode (function value)
;;   (defun make-arg-description-alist (value)
;;   (defmodemethod make-retval-alist default (value)
;;   (defmodemethod make-retval-alist c-mode (value)
;;   (defmodealias
;;   (defmodemethod make-retval-alist perl-mode (value)
;;   (defun make-retval-alist-internal (value token)
;;   (defun get-header-tests (function value)
;;   (defun get-header-tests-on (field list func)
;;   (defun get-class (function value)
;;   (defun get-class-test (field list func)
;;   (defmodemethod  functions-search-for-header c-mode ()
;;   (defmodemethod  functions-search-for-header c++-mode ()
;;   (defmodemethod functions-find-previous-header-end c-mode ()
;;   (defmodemethod functions-find-previous-header-end default ()
;;   (defun find-default-field-values (theList &optional del-old)
;;   (defun parse-old-function-header (here there)
;;   (defun find-existing-value (field theList there)
;;   (defun remove-header-start (value)
;;   (defun remove-header-start-skip (char)
;;   (defun header-prefix-string ()
;;   (defun get-function-visibility (function value)
;;   (defmodemethod include-returns-p c++-mode (field list func)
;;   (defmodemethod include-returns-p default (field list func)
;;   (defun function-class-constructor-p (func)
;;   (defun function-class-destructor-p (func)
;;   (defmodemethod documentation-order-list  default ()
;;   (defmodemethod documentation-order-list c++-mode ()
;;   (defun get-field-documentation (field)
;;   (defun help-for-field ()
;;   (defun display-field-documentation (field)
;;   (defun display-documentation (string)
;;   (defmodegeneric kill-all-comments ()
;;   (defmodemethod kill-all-comments default ()
;;   (defmodemethod kill-all-comments c++-mode ()
;;   (defmodemethod find-function-by-parts ms-visual-test-mode ()
;;   (defvar msvt-type-abbrev-table nil
;;   (defmodemethod skip-comment default ()
;;   (defun parse-single-argument (arg)
;; 
;;  OPTIONS
;;    Update Tests:  no
;; 
;; $RCSfile: functions.el,v $
;; $Revision: 1.27 $
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ideas for improvements:
;;
;; perhaps add a "type" of function to be able to distinguish "inline"
;; and "macros" from regular functions

(if (string-lessp emacs-version "19.30")
    (require 'backquote))
(require 'cl)
(require 'protos)
(require 'alists)
(require 'header)
(require 'c-support)
(require 'modefn)

(require 'ptmacros)
;(setq function-find-alist '((c-mode . new-find-c-function-by-parts)
;			      (c++-mode . new-find-c-function-by-parts)))


(defvar default-documentation-alist
  '(("VISIBILITY"
     ((value "PUBLIC | PRIVATE")
      (documentation
       "If this function is internal to your package, it's PRIVATE")
      (update-function get-function-visibility)
      (lines single)))
    ("FORMAT VERSION"
     ((lines single)
      (value "0.0")))
    ("CLASS"
     ((inclusion-function get-class-test)
      (update-function get-class)
      (lines single)))
    ("EXCEPTIONS"
     ((value "NONE")
      (documentation
       "Any exceptions thrown in or below this function.")
      (lines variable)))
    ("TESTS AND COVERAGE"
     ((inclusion-function get-header-tests-on)
      (update-function get-header-tests)
      (lines multi)))
    ("CAVEATS AND BUGS"
     ((value "|><|")
      (documentation
       "Any private documentation.")
      (lines multi)))
    ("RETURNS"
     ((update-function get-function-return-type-string)
      (inclusion-function include-returns-p)
      (documentation "Return value of the function, and what it means")
      (fdc-header "mdkwrt")
      (lines variable)))
    ("ARGUMENTS"
     ((update-function get-function-args)
      (documentation "what the function takes")
      (fdc-header "mdkwa")
      (lines variable)))
    ("DESCRIPTION"
     ((value "|>Description of function<|")
      (documentation
       "Describe the function.  What does it do, and how if applicable")
      (fdc-header "mdkwdt")
      (lines multi)))
    ("SYNOPSIS"
     ((lines multi)
      (documentation
       "Quick summary of the function suitable for a quick ref card")
      (fdc-header "mdkwsy")))
    (name
     ((update-function doc-insert-name)
      (fdc-header "mdkwfn")
      (lines special)))
    ("NOTES"
     ((value "|><|")
      (lines multi)
      (documentation
       "Any public notes on the function?  This goes into the command ref")
      (fdc-header "mdkwn")
      (default-value "|><|")))
    ("SEE ALSO"
     ((value "|><|")
      (lines multi)
      (documentation
       "Cross References?  What should the user also look into?")
      (default-value "|><|")))
    ("AUTHOR"
     ((update-function doc-insert-author)
      (documentation "who wrote it")
      (lines single)))
    )
  "property list for fields.  This list can be managed by functions set-list-property and get-list-property.  List consists of property name - value pairs for various field names.  Currently used properties include:

'value -- value of the field
'inclusion-funcion -- a function that will be called to test whether
                      the field should be included in the
                      documentation
'update-function -- a function that will be called with the function
                    (returned by (make-function)) and a value, and
                    should return appropriate value to insert."
)

;; this is buffer local so that you can change the author in one
;; buffer when documenting legacy code.  There should be a buffer
;; local "overrides" variable when then falls back on this.

(make-variable-buffer-local 'default-documentation-alist)

(defvar documentation-order-list '(name "CLASS" "AUTHOR" "VISIBILITY" "SYNOPSIS" "DESCRIPTION" "ARGUMENTS" "RETURNS" "NOTES" "SEE ALSO" "CAVEATS AND BUGS" "TESTS AND COVERAGE" "FORMAT VERSION"))

(defvar c++-documentation-order-list '(name "CLASS" "AUTHOR" "VISIBILITY" "SYNOPSIS" "DESCRIPTION" "ARGUMENTS" "RETURNS" "EXCEPTIONS" "NOTES" "SEE ALSO" "CAVEATS AND BUGS" "TESTS AND COVERAGE" "FORMAT VERSION"))

(defvar allow-internal-comments 't
  "Set this variable to 't if you would like to allow comments in
argument lists, etc.  Leaving it nil allows a slight performance
benefit.")

(make-variable-buffer-local 'allow-internal-comments)

(eval-when (load compile eval)
  (defconst vb-token "[_a-zA-Z][_a-zA-Z0-9]*"
    "A visual basic token"))

(defconst c-token vb-token
  "a C language token")

(defvar functions-hit-threshold 5
  "Number of fields in documentation header that must be found for the
old header to be erased.")

(defstruct function
  "Structure which holds information about a function"
  name
  class
  return-type
  args)

(defstruct argument
  "Structure which holds information about an argument"
  name
  type
  description
  )

(defstruct (array-argument (:include argument))
  sizes)

  
(defmacro compile-concat (&rest args)
  (apply 'concat (mapcar 'eval args)))

;;;###autoload
(defmacro match-buffer-substring-no-properties (matchnum)
  "Extract the substring of a buffer from MATCHNUM"
  `(buffer-substring-no-properties (match-beginning ,matchnum)
		    (match-end ,matchnum)))

(defmacro safe-match-buffer-substring-no-properties (matchnum)
  "Extract the substring of a buffer from MATCHNUM, but only if there
is a match there." 
  `(if (match-beginning ,matchnum)
       (match-buffer-substring-no-properties ,matchnum)))

(defmacro match-substring (string matchnum)
  "Extract the matched part from a string"
  `(substring ,string (match-beginning ,matchnum)
	      (match-end ,matchnum)))


(defun default-to (default value)
  "Return DEFAULT unless VALUE is not nil"
  (if value
      value
    default))

(eval-when '(compile)
  (require 'ptmacros))



(defun function-staticp (theFunc)
  "Return 't if function is static, nil otherwise"
  (if (and (function-return-type theFunc)
       (string-match "^static " (function-return-type theFunc)))
      't
    nil))


(defun top-find-function-by-parts ()
  "Handle internal comments"
  ;; first, skip to the function
  (while (or
	  (> (skip-syntax-forward " ") 0)
	  (if (looking-at comment-start-skip)
	      (progn
		(skip-comment)
		't))
	  (if (eq (char-after (point)) 10)
	      (progn
		(forward-char 1)
		't)))
    ;; do nothing
    )
  (if allow-internal-comments
      (find-function-by-parts-internal-comments)
    (find-function-by-parts)))


(defmodemethod find-function-by-parts-internal-comments default ()
  "Find function by parts after removing comments in temp buffer"
  (let ((here (point)))
  (if (search-forward ")")
      (progn
	(let ((stuff (buffer-substring-no-properties here
				       (match-end 0))))
	  (in-temp-buffer-same-mode
	   (insert stuff)
	   (goto-char (point-min))
	   (save-excursion
	     (while (re-search-forward "/\\*.*\\*/" nil 't)
	       (replace-match "")))
	   (save-excursion
	     (while (re-search-forward "//.*$" nil 't)
	       (replace-match "")))
	   (find-function-by-parts)))))))

(defmodemethod find-function-by-parts-internal-comments perl-mode ()
  (find-function-by-parts))

(defmodealias find-function-by-parts-internal-comments perl-mode
  find-function-by-parts-internal-comments ms-visual-test-mode)

(defmodemethod  find-function-by-parts c-mode ()
  "Return a function which describes the next function in the buffer.
It assumes that it is called before the function that it is supposed
to return."
  (if (looking-at "^#")
      (c-mode-macro-find-function-by-parts)
    (c-mode-real-function-find-function-by-parts)))
		  
(defun c-mode-macro-find-function-by-parts ()
  "Find a function representing a macro in 'C' and 'C++'"
  (if (looking-at "#[ \t]*define[ \t]*\\([_a-zA-Z][_a-zA-Z0-9]*\\)[ \t]*(\\(.*\\))")
      (make-function :name (match-buffer-substring-no-properties 1)
		     :args (mapcar
			    (function (lambda (x)
					(make-argument :name x)))
			    (split-string-on-sub
				      (match-buffer-substring-no-properties 2)
				      "," 't)))))
		     

(defun split-string-on-sub (string sub &optional trim-ws)
  "Split a string on a substring and return a list of the parts.  Trim
whitespace form the parts if TRIM-WS is true."
  (let ((list))
    (while (string-match sub string)
      (setq list (cons 
		  (let ((str
			 (substring string 0 (match-beginning 0))))
		    (if trim-ws
			(trim-whitespace str)
		      str))
		  list))
      (setq string (substring string (match-end 0))))
    (reverse (cons (if trim-ws (trim-whitespace string)
		     string)
		   list))))
  
(defun c-mode-real-function-find-function-by-parts ()
  (let ((theFunc (make-function)))
    (if (search-forward-regexp
	 (concat
	  ;; lots of white space
	  "[ \t\n]*"
	  ;; return type, and whitespace
	  "\\(\\(\\([_a-zA-Z][_a-zA-Z0-9]*\\)&?[ \t\n]+\\)*\\*?\\)"
	  ;; maybe a class
	  "\\(\\([_a-zA-Z][_a-zA-Z0-9]*\\)::\\)?"
	  ;; function name
	  "\\(~?[_a-zA-Z][_a-zA-Z0-9]*\\)[ \t\n]*"
					;			      "("
	  )
	 nil 't)
	(progn
	  (if (match-beginning 1)
	      (setf (function-return-type theFunc)
		    (trim-all-whitespace
		     (buffer-substring-no-properties
		      (match-beginning 1)
		      (match-end 1)))))

	  (if (match-beginning 4)
	      (setf (function-class theFunc)
		    (trim-all-whitespace
		     (buffer-substring-no-properties
		      (match-beginning 5)
		      (match-end 5)))))
	  (if (match-beginning 6)
	      (setf (function-name theFunc)
		    (trim-all-whitespace
		     (buffer-substring-no-properties
		      (match-beginning 6)
		      (match-end 6))))
	    (error "could not find function name"))
	  (setf (function-args theFunc) (mapcar 'make-c-argument-list
						(reverse (list-c-function-args
						  (buffer-substring-no-properties
						   (1+ (point))
						   (save-excursion
						     (forward-sexp)
						     (1- (point))))))))
						   
					       
	  theFunc))))

(defun make-c-argument-list (arg)
  "make a list of arguments from the string"
  (make-argument :name (argument-get-name arg)
		 :type (argument-get-type arg)))

(defmodealias  find-function-by-parts c-mode find-function-by-parts  c++-mode)


(defmodemethod find-function-by-parts perl-mode ()
  (let ((theFunc (make-function)))
    (if (search-forward-regexp
	 ;; "sub " + name + "{"
	 "sub[ \t\n]+\\([a-zA-Z0-9_]*\\)[ \t\n]*{" nil 't)
	(progn
	  (setf (function-name theFunc) (buffer-substring-no-properties
					 (match-beginning 1)
					 (match-end 1)))
	  (setf (function-return-type theFunc) "something perlish")

	  (setf (function-args theFunc) (reverse (list-perl-function-args)))))
    theFunc))

(defun find-next-brace ()
  (save-excursion
    (if (search-forward "{" nil 't)
	(point)
      (point-max))))
   
(defun list-perl-function-args ()
  "Make a list of all arguments"
  ;; cases are as follows:
  ;; can assign @_ to a single array
  ;; can assign $_[] to pieces of arrays

  (let ((limit (find-next-brace))
	(list)
	(number)
	(scratch)
	(table (make-hash-table)))
    (while (find-next-perl-arg))
    (setq number 0)
    (while (setq scratch (gethash number table))
      (setq list (cons scratch list))
      (setq number (1+ number)))
    list))
      
(defun find-next-perl-arg ()
  "Find the next perl argument, returning a list of argument place,
name and type.  Type may be nil or \"LIST\".  It also modifies the
hash table of args."
  (let ((scratch)
	(varname "\\(\\$\\|\\@\\)[a-zA-Z_0-9]*"))
;;	(varname "\\$[a-zA-Z_]*"))
    (declare (special limit number table))
    (cond ((search-forward-regexp
	    (concat
	     ;; local ( 
	     "^[ \t]*local[ \t]*([ \t]*"
	     ;; $varname
	     "\\("
	     varname
	     "\\)"
	     ;; ) = @[0]
	     "[ \t]*)[ \t]*=[ \t]*\\(\\$\\|\\@\\)_\\[\\([0-9]*\\)\\]"
	     ) limit 't)
	 (setq number (buffer-substring-no-properties (match-beginning 4)
					(match-end 4)))
	 (setf (gethash (read number)  table)
	       (buffer-substring-no-properties (match-beginning 1)
				 (match-end 1))))
	((search-forward-regexp
	    (concat
	     ;; local ( 
	     "^[ \t]*local[ \t]*([ \t]*"
	     ;; $varname ) =
	     "\\("
	     varname
	     "\\)"
	     ;; ) = @[0]
	     "[ \t]*)[ \t]*=[ \t]*\\(\\$\\|\\@\\)_\\[\\([0-9]*\\)\\]"
	     ) limit 't)
	 (setf (gethash 1 table)
	       (buffer-substring-no-properties (match-beginning 1)
				 (match-end 1))))
	((search-forward-regexp
	    (concat
	     ;; local ( 
	     "^[ \t]*local[ \t]*([ \t]*"
	     ;; $varname, $varname
	     "\\("
	     varname "\\([ \t]*,[ \t]*" varname "\\)*"
	     "\\)"
	     ;;  ) = @_
	     "[ \t]*)[ \t]*=[ \t]*\\@_"
	     ) limit 't)
	 (setq scratch (buffer-substring-no-properties (match-beginning 1)
				 (match-end 1)))
	 (setq number 0)
	 (loop for arg in (split-perl-args "[ \t]*,[ \t]*" scratch)
	       count (enter-arg-hash arg))))))

(defun enter-arg-hash (arg)
  (declare (special number table))
  (setf (gethash number table) arg)
  (setq number (1+ number)))

(defun split-perl-args (regexp line)
  (let ((accum))
  (while (string-match regexp line)
    (setq accum (cons (substring line 0 (match-beginning 0)) accum ))
    (setq line (substring line (match-end 0) (length line))))
  (setq accum (cons line accum))
  (reverse accum)))
  

(defmodemethod functions-search-for-header ms-visual-test-mode ()
"Search backwards for function documentation header"
;;; this is just like "perl-mode", and the code should be folded
;;; together 
  (let ((start (save-excursion
	 (search-backward (progn
			    (in-temp-buffer-same-mode
			     (make-divisor)
			     (buffer-substring-no-properties (point-min) (point-max))))
			  nil 't)))
	(brace
	 (save-excursion (search-backward "end" nil 't))))
    (cond ((not start)
	   nil)
	  ((not brace)
	   (goto-char start)
	   't)
	  ((< brace start)
	   (goto-char start)
	   't)
	  ('t
	   nil))))
  
(defmodemethod functions-search-for-header perl-mode ()
"Search backwards for function documentation header"
  (let ((start (save-excursion
	 (search-backward (progn
			    (in-temp-buffer
			     (perl-mode)
			     (make-divisor)
			     (buffer-substring-no-properties (point-min) (point-max))))
			  nil 't)))
	(brace
	 (save-excursion (search-backward "}" nil 't))))
    (cond ((not start)
	   nil)
	  ((not brace)
	   (goto-char start)
	   't)
	  ((< brace start)
	   (goto-char start)
	   't)
	  ('t
	   nil))))



(defun set-property (field key value)
  "set property of FIELD with KEY to VALUE"
  (setq default-documentation-alist
	(make-association default-documentation-alist
			  field
			  (make-association (get-association
					     default-documentation-alist field)
					    key value))))

(defun copy-proplist (theList)
  (mapcar (lambda (listel)
	    (cons (car listel)
		  (cons
		   (copy-alist
		    (cadr listel))
		   nil)))
	  theList))
 
(defun set-list-property-fun (list field key value)
  (make-association list
		    field
		    (make-association (get-association
				       list field)
				      key value)))

(defmacro set-list-property (list field key value)
  (` (setq (, list) (set-list-property-fun (, list) (, field) (, key)
					   (, value)))))


(defun get-property (field key)
  (get-association (get-association default-documentation-alist  field) key))

(defun get-list-property (list field key)
  (get-association (get-association list  field) key))

(declaim (inline get-property get-list-property set-list-property-fun))

;;;###autoload
(defun document-function (delete-old)
  "Insert a comment block containing the module title, author, etc.  To
   use, place the cursor on or before the line starting the function definition.

   Functions with these headers can be automatically placed into the table
   of contents by the update-table-of-contents command.

   The synopsis can be automatically constructed by the
   update-function-synopsis (\\[update-function-synopsis]) command."
  (interactive "P")
  (document-c-function delete-old))

;;;###autoload
(defun document-c-function (delete-old)
  "Insert a comment block containing the module title, author, etc.  To
   use, place the cursor on or before the line starting the function definition.

   Functions with these headers can be automatically placed into the table
   of contents by the update-table-of-contents command.

   The synopsis can be automatically constructed by the
   update-function-synopsis (\\[update-function-synopsis]) command."
  (interactive "P")
  (let (
	(start (make-marker))
	(theFunc (save-excursion (top-find-function-by-parts)))
	(theList (copy-proplist default-documentation-alist))
	(header-prefix (header-prefix-string)))
    (if theFunc
	(progn
	  (set-marker start (progn (beginning-of-line) (point)))
	  (setq theList (find-default-field-values theList (not delete-old)))
	  (insert comment-start)
	  (fill-line-with-preceding-character)
	  (insert "\n")
	  (mapcar (lambda (field)
		    (insert-or-update-field field theList theFunc))
		  (documentation-order-list))
	  (if (string= comment-end "")
	      nil
	    (insert comment-end "\n"))
	  (goto-char start)
	  (set-marker start nil)
	  (forward-line 1)
	  (end-of-line))
      (message "No function found"))))

(defun include-fieldp (inc field list func)
  (if (not inc)
      't
    (funcall inc field list func)))

(declaim (inline include-fieldp))


(defun insert-or-update-field (field list func)
  "Insert a field into the buffer, possibly updating the fields value
from existing information.

FIELD -- name of field to insert
LIST -- property list of fields
FUNC -- a function created by (make-function)
"
  (let ((start)
	(value-end)
	(value-start)
	(inclusion (get-list-property list field 'inclusion-function))
	(linetype (get-list-property list field 'lines))
	(fn (get-list-property list field 'update-function))
	(value (get-list-property list field 'value)))
    (declare (special header-prefix))
    (if (include-fieldp inclusion field list func)
	(progn
	  (setq start (point))
	  (insert (if (stringp field)
		      field
		    ""))
	  (setq value-start
		(insert-value linetype
			      (if fn
				  (funcall fn func value)
				value)))
	  (insert-box start (point) header-prefix)))))
      

  
(defun insert-value (linetype value)
  (let ((value-start (point)))
    (if value
	(progn
	  (cond ((eq linetype 'single)
		 (insert ":    " value "\n")
		 (if (= (char-after (- (point) 2)) 10)
		     (delete-char -1))
		 (setq value-start (point)))
		((eq linetype 'multi)
		 (insert "\n")
		 (setq value-start (point))
		 (insert value "\n")
	         (if (= (char-after (- (point) 2)) 10)
		     (delete-char -1))
		 (insert-box value-start (point) "  "))
		((eq linetype 'special)
		 (insert value "\n"))
		((eq linetype 'variable)
		 (if (or (multiple-newlinesp value)
			 (contains-description value))
		     (progn
		       (insert "\n")
		       (setq value-start (point))
		       (insert value "\n")
		       (if (= (char-after (- (point) 2)) 10)
			   (delete-char -1))
		       (insert-box value-start (point) "  "))
		   (insert ":    " value "\n")
		   (setq value-start (point))))))
      (insert "\n"))
    value-start))

(defun contains-description (value)
  (string-match "--" value))

(defun multiple-newlinesp (string)
  (string-match "\n" string))
  
(defun should-indent-value (linetype)
  (eq linetype 'multi))

(defun doc-insert-author (function value)
  (if value
      value
    (user-full-name-and-email)))

(defun doc-insert-name (function value)
  (if (not value)
      (cond
       ((function-class-constructor-p function)
	(setq value (concat "constructor for "
			    (function-class function))))
       ((function-class-destructor-p function)
	(setq value (concat "destructor for "
			    (function-class function))))))
  (concat (function-name function) "() -- "
	  (if value
	      value
	    "")))

(defun get-function-return-type-string (function value)
  (flet ((isblank (type)
		  (or
		   (not type)
		   (string= type ""))))
  (let ((type (function-return-type function))
	)
    ;; trim off the "(type) -- "
    (if (and value
	     (not (string= value "NOTHING")))
	(progn
	  (string-match "(.*) -- ?\\(\\(.\\|\n\\)*\\)" value)
	  (setq value (substring value (match-beginning 1)
				 (match-end 1))))
      (setq value nil))

    (cond ((string= type "void")
	   "NOTHING")
	  ((and (isblank type)
		(eq major-mode
		    'c-mode))
	   "(int) -- ")
	  ((and (isblank type)
		(eq major-mode
		    'c++-mode))
	   "NOTHING")
	  ('t 
	    (concat "(" (remove-keywords (trim-all-whitespace type)) ") -- "
		   (if (stringp value)
		       value
		     "")))))))




  
(defun remove-keywords (string)
  "Remove static from beginning of a type"
  (if (string-match "^static " string)
      (substring string (match-end 0) nil)
    string))


(defmodemethod get-function-args default (function value)
  (let ((args (function-args function))
	(namelist (make-arg-description-alist value)))
    (if (and args
	     (not (string= (argument-name (car args)) "void")))
	(progn
	  (apply
	   'concat
	   (mapcar (lambda (arg)
		     (concat
		      (trim-all-whitespace
		       (concat
			(argument-name  arg)
			(if (argument-type arg)
			    (concat
			     " ("
			     (argument-type  arg)
			     ")" 
			     )
			  "")
			" -- "
			(if (and namelist
				 (assoc (argument-name arg) namelist))
			    (cadr (assoc (argument-name arg) namelist))
			  "" )))
		      "\n"))
		   args)))
      "NONE")))


(defmodemethod get-function-args perl-mode (function value)
  (let ((args (function-args function))
	(namelist (make-arg-description-alist value)))
    (if (and args
	     (not (string= (argument-get-name (car args)) "void")))
	(progn
	  (apply
	   'concat
	   (mapcar (lambda (arg)
		     (concat
		      (trim-all-whitespace
		       (concat
			arg
			" -- "
			(if (and namelist
				 (assoc arg namelist))
			    (cadr (assoc arg namelist))
			  "" )))
		      "\n"))
		   args)))
      "NONE")))

(eval-when '(compile)
  (require 'ptmacros))


(defun make-arg-description-alist (value)
  "Return an alist of (name . value) for argument names.
Takes the previous field.  Does its work in a temporary buffer"
  (if value
      (in-temp-buffer-same-mode
       (make-retval-alist value))))

(defmodemethod make-retval-alist default (value)
  (make-retval-alist-internal value "[_a-zA-Z][_a-zA-Z0-9]*"))

(defmodemethod make-retval-alist c-mode (value)
  (make-retval-alist-internal value "[_a-zA-Z][_a-zA-Z0-9]*"))

(defmodealias
  make-retval-alist c-mode
  make-retval-alist c++-mode )

(defmodemethod make-retval-alist perl-mode (value)
  (make-retval-alist-internal value "\\$[_a-zA-Z][_a-zA-Z0-9]*"))


(defun make-retval-alist-internal (value token)
  "Return an alist of (name . value) for argument names.
Takes the previous field and regexp for a token"
  (let ((theList)
	(start)
	(name)
	(val))
    (insert value)
    (goto-char (point-min))
    (while
	(re-search-forward
	 (concat "^\\("
		 token
		 "\\)[ \t]*\\((.*)\\)?[ \t]* --[ \t]*")
		 nil 't)
      (setq name (buffer-substring-no-properties (match-beginning 1)
				   (match-end 1)))
      (setq start (point))
      (if (re-search-forward (concat
			      "^\\("
			      token
			      "\\)"
			      )nil 't)
	  nil
	(goto-char (point-max)))	;goto to end if no next arg
      (beginning-of-line)
      (backward-char 1)			;before the newline
      (setq val (buffer-substring-no-properties start (point)))
      (setq theList (make-association theList name val)))
    theList))
    
    





    
(defun get-header-tests (function value)
  "Put test coverage checkboxes into headers"
  (let ((theList header-tests-list))
    (if header-tests-on
	(progn
	  (apply 'concat (mapcar (lambda (element)
				   (concat element ": __  "))
				 theList))))))

(defun get-header-tests-on (field list func)
  header-tests-on)
   
    

(defun get-class (function value)
  (if (function-class function)
      (function-class function)
    "NONE"))

(defun get-class-test (field list func)
  (eq major-mode 'c++-mode))

;;(defun get-documentation-defaults (default-list)
;;  "Set the values of default documentation list to what they really
;;are."
;;  (let (
;;	(theList (copy-alist default-list)))
;;    (find-function-doc-header)		;leaves us after "--" in doc
;;					;header
;;    (set-list-property theList 'name 'value (buffer-substring-no-properties
;;					     (point)
;;					     (progn
;;					       (end-of-line)
;;					       (point))))
;;    ))
	

(defmodemethod  functions-search-for-header c-mode ()
"Search backwards for function documentation header"
  (let ((start (save-excursion
	 (search-backward (trim-whitespace comment-start) nil 't)))
	(brace
	 (save-excursion (search-backward "}" nil 't))))
    (cond ((not start)
	   nil)
	  ((not brace)
	   (goto-char start)
	   't)
	  ((< brace start)
	   (goto-char start)
	   't)
	  ('t
	   nil))))

(defmodemethod  functions-search-for-header c++-mode ()
"Search backwards for function documentation header"
  (let ((start (save-excursion
	 (search-backward (progn
			    (in-temp-buffer
			     (c++-mode)
			     (make-divisor)
			     (buffer-substring-no-properties (point-min) (point-max))))
			  nil 't)))
	(brace
	 (save-excursion (search-backward "}" nil 't))))
    (cond ((not start)
	   nil)
	  ((not brace)
	   (goto-char start)
	   't)
	  ((< brace start)
	   (goto-char start)
	   't)
	  ('t
	   nil))))


(defmodemethod functions-find-previous-header-end c-mode ()
  "Find where the function starts, return that point."
  (save-excursion
    (skip-syntax-backward " ")
    (if (looking-at "\n")
	(forward-char 1))
    (point)))

(defmodemethod functions-find-previous-header-end default ()
  "Find where the function starts, return that point."
  (save-excursion
    (skip-syntax-backward " ")
    (point)))

(defun find-default-field-values (theList &optional del-old)
  "Gets a field property list of default values, and should set those
values appropriately from the previous documentation.  If 2nd arg is
not nil, function deletes the old documentation header."
  (let ((header-start (header-prefix-string))
	(here (save-excursion (functions-search-for-header) (point)))
	(there (save-excursion (functions-find-previous-header-end)))
	(results)
	(temp))
    (if (not here)
	nil
      (setq results (parse-old-function-header here there))
      (if (and here there
	       ;; check to see if we found enough fields to think that
	       ;; this is actually a function header
	       (> (length (delq nil results)) functions-hit-threshold)
	       del-old)
	  ;;	(delete-region here (+ there 2))))
	  (delete-region here there))))
    theList)


(defun parse-old-function-header (here there)
  (declare (special theList header-start temp))
  (if (< here there)
      (save-excursion
	(goto-char here)
	(if (search-forward-regexp (concat (regexp-quote header-start)
					   "\\([_a-zA-Z][_a-zA-Z0-9]*\\)"
					   "\\(()\\)? --[ \t]*\\([^\n]*\\)")
				   there 't)
	    (progn
	      (setq temp (buffer-substring-no-properties (match-beginning 3)
							 (match-end 3)))
	      (set-list-property theList 'name 'value temp)))
	(mapcar (lambda (field)		; save results for
					; later checking
		  (save-excursion
		    (find-existing-value field theList there)))
		(documentation-order-list)))))

(defun find-existing-value (field theList there)
  "Finds default value for FIELD, puts it into THELIST.
THERE is a limit on search distance."
  (declare (special header-start))
  (if (not (stringp field))
      nil
    (if					;first, if it's single line
	(re-search-forward (concat (regexp-quote header-start)
				   (regexp-quote field)
				   "[ \t]*:[ \t]*\\([^\n]*\\)") there 't)
	(progn
	  (set-list-property theList field 'value (buffer-substring-no-properties
						   (match-beginning 1)
						   (match-end 1)))
	  't)
      (progn
	;; Here's the hard part.  We've got to find the end
	(if;; but only if we're successful
	    (re-search-forward (concat "^" (regexp-quote header-start)
				       (regexp-quote field)
				       "[ \t]*\n") there 't)
	    (let ((start (point))
		  (end))
	      (if (re-search-forward (concat "^" (regexp-quote
						  header-start)
					     "?[^ \t\n]") there 't)
		  ;; compensate for header & space & newline
		  (backward-char (+ (length header-start) 1))
		(goto-char there)
		(backward-char (+ (length (trim-whitespace comment-end)))))
	      (set-list-property theList field 'value
				 (remove-header-start
				  (buffer-substring-no-properties start (point))))
	      't
	      ))))))

;;(defun remove-header-start (value)
;;  "remove the header start string from beginning of each line in
;;VALUE."
;;  (let ((buf (current-buffer))
;;	(new-buf (generate-new-buffer " *temp*"))
;;	(temp)
;;	(string))
;;    (set-buffer new-buf)
;;    (set-window-buffer (selected-window) (current-buffer))
;;    (insert value)
;;    (goto-char (point-min))
;;    (re-search-forward (concat (regexp-quote header-start)
;;			       "[ \t]*[^ \t\n]") nil 't)
;;    
;;    (setq string (buffer-substring-no-properties (save-excursion
;;				     (beginning-of-line)
;;				     (point))
;;				   (1- (point))))
;;    (goto-char (point-min))
;;    (while (search-forward string there 't)
;;      (replace-match ""))
;;    (setq temp (buffer-substring-no-properties (point-min)
;;				 (point-max)))
;;    (set-buffer buf)
;;    (set-window-buffer (selected-window) (current-buffer))
;;    (kill-buffer new-buf)
;;    temp))

(defun remove-header-start (value)
  "remove the header start string from beginning of each line in VALUE."
  (declare (special there header-start))
  (in-temp-buffer
   (let ((temp)
	 (string))
     (if (and value
	      (> (length value) 0))
	 (block 'remove
	   (insert value)
	   (goto-char (point-min))
	   (if (re-search-forward (concat (regexp-quote header-start)
					  "[ \t]*[^ \t\n]") nil 't)
	       nil
	     (return-from 'remove nil))
    
	   (setq string (buffer-substring-no-properties (save-excursion
					    (beginning-of-line)
					    (search-forward header-start)
					    (point))
					  (1- (point))))
	   (goto-char (point-min))
	   ;; here string contains the whitespace after the header-start
	   (setq string
		 (concat "\\("
			 (regexp-quote header-start)
			 "?\\)\\("	;the ? is a hack -- make space optional
			 string		;should only have whitespace
			 "\\)?"))
		 
	   (while (search-forward-regexp string there 't)
	     (replace-match ""))
	   (setq temp (buffer-substring-no-properties (point-min)
					(point-max)))
	   temp)))))

(defun remove-header-start-skip (char)
  (and (not (eq char ?\n))
       (whitespacep char)))

;;;###autoload
(defun header-prefix-string ()
  "Returns a mode specific prefix string for use in headers.
Is sensitive to the various language dependent comment conventions."
  (let ((comment-end-p (and comment-end
			    (not (string-equal comment-end "")))))
    (cond
     ((and comment-start (= (length comment-start) 1))
      (concat comment-start comment-start " "))
     ;; Special case, three letter comment starts where the first and
     ;; second letters are the same. (i.e. c++ and ada)
     ((and comment-start (= (length comment-start) 3)
	   (equal (aref comment-start 1) (aref comment-start 0)))
      comment-start)
     ;; Other three letter comment starts -> grab the middle character
     ((and comment-start (= (length comment-start) 3))
      (concat " " (list (aref comment-start 1)) " "))
     ;;
     ((and comment-start (not comment-end-p))
      ;; Note: no comment end implies that the full comment start must be
      ;; used on each line.
      comment-start)
     (t					; I have no idea what is a good block
					; start character and will use lisp
					; as a default.
      ";; "))
    ))

(defun get-function-visibility (function value)
  (if (function-staticp function)
      "PRIVATE"
    value))

(defmodemethod include-returns-p c++-mode (field list func)
  "Return t if FIELD should be included, nil otherwise.
LIST is list of defaults, FUNC is the function."
  (cond
   ((function-class-constructor-p func)
    nil)
   ((function-class-destructor-p func)
    nil)
   ('t
    't)))

(defmodemethod include-returns-p default (field list func)
  't)


(defun function-class-constructor-p (func)
  "Return 't if FUNC is a class constructor"
  (string= (function-class func)
	       (function-name func)))

(defun function-class-destructor-p (func)
  "Return 't if FUNC is a class destructor"
  (string= (concat "~" (function-class func))
	    (function-name func)))

(defmodemethod documentation-order-list  default ()
  "Return the default-documentation-alist for most modes."
  documentation-order-list)

(defmodemethod documentation-order-list c++-mode ()
  "There's a special list for c++ mode"
  c++-documentation-order-list)

(defun get-field-documentation (field)
  "Return the documentation string for a function field"
  (get-list-property default-documentation-alist
		     field
		     'documentation))

;;;###autoload
(defun help-for-field ()
  "Give some quick help on what the current documentation field is for
and how to use it"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let (field
	  (eol (save-excursion
		 (end-of-line)
		 (point)))
	  (header-prefix-string (header-prefix-string))
	  )
      (if
	  (re-search-forward (concat
			      (regexp-quote
			       header-prefix-string)
			      "\\([a-zA-Z0-9_-]+\\)")
			     eol
			     't)
	  (display-field-documentation (buffer-substring-no-properties
					(match-beginning 1)
					(match-end 1)))
	
	(if
	    (re-search-backward (concat
				 (regexp-quote
				  header-prefix-string)
				 "\\([a-zA-Z0-9_-]+\\)")
				nil
				't)
	    (display-field-documentation (buffer-substring-no-properties
					  (match-beginning 1)
					  (match-end 1)))
	
	  (message "Can't find field"))))))

(defun display-field-documentation (field)
  "Display help for a chosen field.  Prompts for the field"
  (interactive (list (completing-read
		      "Which Field? "
		      default-documentation-alist
		      nil
		      't)))
  (let ((docs (get-list-property default-documentation-alist field
				 'documentation)))
    (display-documentation
     (if docs
	 (format "documentation for function header field %s\n\n%s"
		 field
		 docs)
       (format "No documentation for function header field %s"
	       field)))))

(defun display-documentation (string)
  "Display help on a particular field in the *Help* window."
  (with-output-to-temp-buffer
      "*Help*"
    (princ string)
    (print-help-return-message)))

;;;###autoload (autoload 'kill-all-comments "functions")
(defmodegeneric kill-all-comments ()
  "Kills all comments in the current buffer")

(defmodemethod kill-all-comments default ()
  "Kill all comments according to comment-start and comment-end"
  (save-excursion
    (beginning-of-buffer)
    (let ((comment-start-real (trim-whitespace comment-start))
	  (comment-end-real
	   (if (string= comment-end "")
	       "\n"
	     (trim-whitespace comment-end))))
      (while (search-forward comment-start-real nil 't)
	(let ((start (- (point) (length comment-start-real))))
	  (if (search-forward comment-end-real nil 't)
	      (if (string= comment-end-real "\n")
		  (delete-region start (1- (point)))
		(delete-region start (point)))
		(error "End of comment not found")))))))

(defmodemethod kill-all-comments c++-mode ()
  "Kill all comments /* */"
  (call-next-mode-method)
  (let ((comment-start "/*")
	(comment-end "*/"))
    (call-default-mode-method)))


(defmodemethod find-function-by-parts ms-visual-test-mode ()
  "Find the function in Visual Test mode"
  (let ((case-fold-search 't))
    (skip-syntax-forward " >")
    (if (looking-at
	 (compile-concat
	  ;; leading ws, sub or function prefix
	  "[ \t]*\\(sub\\|function\\)[ \t]+\\("
	  ;; name of function
	  vb-token
	  ;; argument list
	  "\\)[ \t]*\\(([^\n]*)\\)"
	  ;; possible return type
	  "\\([ \t]+as[ \t]+\\("
	  vb-token
	  "\\)\\)?"
	  ))
	(let ((type-str (match-buffer-substring-no-properties 1))
	      (name-str (match-buffer-substring-no-properties 2))
	      (arg-str (match-buffer-substring-no-properties 3))
	      (ret-str (safe-match-buffer-substring-no-properties 5)))
	  (make-function :name name-str
			 :args (loop
				for arg in (list-c-function-args arg-str)
				collect (parse-single-argument arg) into parsed-args
				finally return (if (car parsed-args)
						  (reverse parsed-args)
						 nil))
			 :return-type (default-to
					(if (string-match type-str "sub")
					 "void"
					 "long")
					ret-str)
			 )))))

(defvar msvt-type-abbrev-table nil
  "Table to hold the postix abbreviation types")

(if msvt-type-abbrev-table
    ()
  (setq msvt-type-abbrev-table (make-hash-table :test 'equal))
  (setf (gethash "$" msvt-type-abbrev-table) "string")
  (setf (gethash "&" msvt-type-abbrev-table) "long")
  (setf (gethash "!" msvt-type-abbrev-table) "single")
  (setf (gethash "#" msvt-type-abbrev-table) "double")
  (setf (gethash "%" msvt-type-abbrev-table) "integer")
  )


;; TODO:  reconcile this with the defun* for skip-comment
(defmodemethod skip-comment default ()
  "Skip a comment going forward"
  (if (search-forward-regexp (concat (regexp-quote comment-start)
			  ".*"
			  (regexp-quote comment-end))
		  nil
		  't)
      (goto-char (match-end 0))))

(defun parse-single-argument (arg)
  "Parse an argument and return a type ARGUMENT appropriately filled out"
  (cond
   ((string-match (compile-concat
		   "\\("
		   vb-token
		   "\\)[ \t]*as[ \t]\\("
		   vb-token
		   "\\)")
		  arg
		  )
    (make-argument :name (match-substring arg 1)
		   :type (match-substring arg 2)))
   ((string-match (compile-concat
		   "\\("
		   vb-token
		   "\\)\\(.\\)")
		  arg)
    (make-argument :name (match-substring arg 1)
		   :type (gethash (match-substring arg 2) msvt-type-abbrev-table)))))

  
(provide 'functions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                                   ;;;;
;;;;                             P r o f i l e                         ;;;;
;;;;                                                                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Origional

;; Function Name                                Call Count  Elapsed Time  Average Time
;; ===========================================  ==========  ============  ============
;; include-fieldp                               166         0.0399999999  0.0002409638
;; insert-or-update-field                       166         0.9600000000  0.0057831325
;; insert-value                                 152         0.1999999999  0.0013157894
;; find-existing-value                          111         1.1799999999  0.0106306306
;; remove-header-start                          54          0.8399999999  0.0155555555
;; multiple-newlinesp                           34          0.0           0.0
;; header-prefix-string                         28          0.0300000000  0.0010714285
;; make-c-argument-list                         22          0.0299999999  0.0013636363
;; function-class-destructor-p                  14          0.0           0.0
;; function-class-constructor-p                 14          0.0           0.0
;; contains-description                         14          0.0299999999  0.0021428571
;; get-function-visibility                      12          0.0           0.0
;; find-default-field-values                    12          1.4599999999  0.1216666666
;; get-class-test                               12          0.0           0.0
;; get-header-tests-on                          12          0.0           0.0
;; make-arg-description-alist                   12          0.0299999999  0.0024999999
;; get-function-return-type-string              12          0.1299999999  0.0108333333
;; doc-insert-name                              12          0.0299999999  0.0024999999
;; doc-insert-author                            12          0.0           0.0
;; document-c-function                          12          2.9099999999  0.2424999999
;; document-function                            12          2.9399999999  0.2449999999
;; copy-proplist                                12          0.0299999999  0.0024999999
;; c-mode-real-function-find-function-by-parts  12          0.1799999999  0.0149999999
;; top-find-function-by-parts                   12          0.3000000000  0.0250000000
;; function-staticp                             12          0.0           0.0
;; remove-keywords                              11          0.0           0.0
;; get-class                                    10          0.0           0.0
;; make-retval-alist-internal                   8           0.0299999999  0.0037499999
;; parse-single-argument                        0           0             0.0
;; display-documentation                        0           0             0.0
;; display-field-documentation                  0           0             0.0
;; help-for-field                               0           0             0.0
;; get-field-documentation                      0           0             0.0
;; remove-header-start-skip                     0           0             0.0
;; get-header-tests                             0           0             0.0
;; should-indent-value                          0           0             0.0
;; get-list-property                            0           0             0.0
;; get-property                                 0           0             0.0
;; set-list-property-fun                        0           0             0.0
;; set-property                                 0           0             0.0
;; split-perl-args                              0           0             0.0
;; enter-arg-hash                               0           0             0.0
;; find-next-perl-arg                           0           0             0.0
;; list-perl-function-args                      0           0             0.0
;; find-next-brace                              0           0             0.0
;; split-string-on-sub                          0           0             0.0
;; c-mode-macro-find-function-by-parts          0           0             0.0
;; default-to                                   0           0             0.0


;; Function Name                                       Call Count  Elapsed Time  Average Time
;; ==================================================  ==========  ============  ============
;; document-c-function                                 15          4.2099999999  0.2806666666
;; document-function                                   15          4.2099999999  0.2806666666
;; default$get-function-args                           14          1.6999999999  0.1214285714
;; make-arg-description-alist                          15          1.5299999999  0.1019999999
;; find-default-field-values                           15          1.0500000000  0.0700000000
;; parse-old-function-header                           15          0.7300000000  0.0486666666
;; perl-mode$functions-search-for-header               1           0.0299999999  0.0299999999
;; top-find-function-by-parts                          15          0.3499999999  0.0233333333
;; default$find-function-by-parts-internal-comments    14          0.3199999999  0.0228571428
;; c++-mode$functions-search-for-header                11          0.1900000000  0.0172727272
;; split-string-on-sub                                 2           0.0299999999  0.0149999999
;; c-mode-macro-find-function-by-parts                 2           0.0299999999  0.0149999999
;; insert-or-update-field                              206         2.6899999999  0.0130582524
;; remove-header-start                                 54          0.5400000000  0.0100000000
;; c-mode$functions-search-for-header                  3           0.0299999999  0.0099999999
;; c-mode$find-function-by-parts                       14          0.0900000000  0.0064285714
;; find-existing-value                                 124         0.6700000000  0.0054032258
;; c-mode-real-function-find-function-by-parts         12          0.0600000000  0.0050000000
;; get-function-return-type-string                     15          0.0300000000  0.0020000000
;; get-function-visibility                             15          0.0299999999  0.0019999999
;; function-staticp                                    15          0.0299999999  0.0019999999
;; insert-value                                        187         0.2999999999  0.0016042780
;; make-c-argument-list                                22          0.0299999999  0.0013636363
;; header-prefix-string                                34          0.0299999999  0.0008823529
;; c++-mode$kill-all-comments                          2           0.0           0.0
;; default$kill-all-comments                           5           0.0           0.0
;; c++-mode$documentation-order-list                   18          0.0           0.0
;; default$documentation-order-list                    6           0.0           0.0
;; function-class-destructor-p                         18          0.0           0.0
;; function-class-constructor-p                        18          0.0           0.0
;; default$include-returns-p                           4           0.0           0.0
;; c++-mode$include-returns-p                          11          0.0           0.0
;; default$functions-find-previous-header-end          12          0.0           0.0
;; c-mode$functions-find-previous-header-end           3           0.0           0.0
;; get-class-test                                      15          0.0           0.0
;; get-class                                           11          0.0           0.0
;; get-header-tests-on                                 15          0.0           0.0
;; make-retval-alist-internal                          8           0.0           0.0
;; c-mode$make-retval-alist                            8           0.0           0.0
;; perl-mode$get-function-args                         1           0.0           0.0
;; remove-keywords                                     12          0.0           0.0
;; doc-insert-name                                     15          0.0           0.0
;; doc-insert-author                                   15          0.0           0.0
;; multiple-newlinesp                                  41          0.0           0.0
;; contains-description                                18          0.0           0.0
;; copy-proplist                                       15          0.0           0.0
;; find-next-perl-arg                                  3           0.0           0.0
;; list-perl-function-args                             1           0.0           0.0
;; find-next-brace                                     1           0.0           0.0
;; perl-mode$find-function-by-parts                    1           0.0           0.0
;; perl-mode$find-function-by-parts-internal-comments  1           0.0           0.0
