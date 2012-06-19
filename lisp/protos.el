;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; protos.el -- more automatic prototypes stuff
;; 
;; Author          : Dewey M. Sasser
;; Created On      : Sun Jun 26 00:24:42 1994
;; Status          : $State: Exp $
;; Name            : $Name: HEAD $
;; Keywords        : 
;; PURPOSE
;; 	At the time of file creation, this is a test to check the
;;      feasibility of getting more information from a c function.
;;      Namely, a list of arguments.
;; 
;; NOTES
;;      This file is not currently used.  It has been superseeded by
;;      functions.el.
;;
;; TABLE OF CONTENTS
;;   (defstruct function
;;   (defun find-c-function-by-parts ()
;;   (defun list-c-function-args (function &optional list)
;;   (defmacro increment (number)
;;   (defmacro decrement (number)
;;   (defun insert-function-arguments (fun-desc &optional theFunction)
;;   (defun insert-function-arguments-internal (fun-desc &optional theFunction flag)
;;   (defun insert-function-return-type (type &optional theFunction)
;;   (defun argument-get-name (type)
;;   (defun argument-get-type (type)
;;   (defun argument-type-void-p (arg)
;;   (defun function-get-args (func)
;;   (defun function-get-return-type (func)
;;   (defun function-get-name (func)
;;   (defun c-modep ()
;;   (defun header-insert-return-values ()
;;   (defun find-function-return-values ()
;;   (defun find-function-build-alist (end list)
;; 
;; $RCSfile: protos.el,v $
;; $Revision: 1.10 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'cl)

(defstruct function
  "Structure which holds information about a function"
  name
  class
  return-type
  args)

(defun find-c-function-by-parts ()
  "Returns a list of the following type
(fname ret_type (arguments ...))"
(interactive)
(let ((theFunction (make-function))
      (function (find-next-c-function))
      (fname)
      (ret_type)
      (fargs)
      (work))
  (if (not
       (and function (string-match "\\([_a-zA-Z][_a-zA-Z0-9]*\\)[ \t]*(" function)))
      ()
    (setq work (match-beginning 1))
    (setf (function-name theFunction)
	  (substring function (match-beginning 1) (match-end 1)))
    (setq ret_type (substring function 0 work))
    (setq work (match-end 0))
    (setq ret_type (trim-whitespace
		    (if (string= (find-first-word ret_type)
				 "static")
			(progn
			  (string-match "[ \t\n]*static[ \t\n]+\\(.*\\)"
					ret_type)
			  (substring ret_type (match-beginning 1)
				     (match-end 1)))
		      ret_type)))
    (setf (function-return-type theFunction) (if (string-match "\\(.*\\)[ \t]+[^:]*::" ret_type)
		       (substring ret_type (match-beginning 1)
				  (match-end 1))
		     ret_type))
    (if (= (aref function (- (length function) 2)) 41) ; 41 = l paren
	(setq function (substring function 0 (- (length function) 2))))
    (setq function (substring function work (length function)))
    (setf (function-args theFunction)
	  (reverse (list-c-function-args (substring function 0
						    (min
						     (length function)
						     (string-match ")[ \t\n]*:" function))))))
    theFunction)))


(defun list-c-function-args (function &optional list)
  "Return a list of function arguments.
This function assumes that the STRING it is passed starts with the
opening paren of the argument list.  It tacks it's argument onto
the end of LIST."
  (if (and (> (length function) 1)
	   (string-match "^)?\\([^,;]*\\)[;,]?" function))
      (list-c-function-args (substring function  (match-end 0)
				       (length function))
			    (cons (trim-whitespace
				   (substring function (match-beginning 1)
					      (match-end 1)))
				  list))
    list))
				       

(defmacro increment (number)
  "Increment a number"
  (list 'setq number (list '1+ number)))

(defmacro decrement (number)
  "Decrement a number"
  (list 'setq number (list '1- number)))


(defun insert-function-arguments (fun-desc &optional theFunction)
  "Insert function arguments into function documentation.
Takes a function description as returned by find-c-function-by-parts."
  (insert header-prefix-string " ARGUMENTS\n")
  (if (function-args fun-desc)
      (insert-function-arguments-internal (function-args fun-desc) theFunction)
    (backward-delete-char  1)
    (insert ": NONE\n"))
  (insert header-prefix-string " RETURNS")
  (insert-function-return-type (function-return-type fun-desc) theFunction))

(defun insert-function-arguments-internal (fun-desc &optional theFunction flag)
  (if (not fun-desc)
      nil
    (insert header-prefix-string
		  "   "
		  (argument-get-name (car fun-desc))
		  " ("
		  (argument-get-type (car fun-desc))
		  ") -- "
		  (function-find-arg-description
		   theFunction (argument-get-name (car fun-desc)))
		  "\n")
      (insert-function-arguments-internal (cdr fun-desc) theFunction)))

(defun insert-function-return-type (type &optional theFunction)
  (if (string= type "void")
      " NOTHING\n"
    (insert "\n"
	    header-prefix-string "   (" type ") -- "
	    (if (and theFunction (function-desc-returns theFunction))
		(function-desc-returns theFunction)
	      "\n"))))

	  
(defun argument-get-name (type)
  "Get the name of an argument."
  (if (string-match "\\([_a-zA-Z][_a-zA-Z0-9]*\\)[][0-9]*\\(=.*\\)?$" type)
      (substring type (match-beginning 1)
		 (match-end 1))
    (if (string-match "..." type)
	""
      "Couldn't recognize name")))

(defun argument-get-type (type)
  "Get the name of an argument."
  (if (string-match "\\([_a-zA-Z][_a-zA-Z0-9]*\\)[][0-9]*\\(=.*\\)?$" type)
      (trim-whitespace
       (concat
	(substring type 0
		   (match-beginning 1))
	(substring type (match-end 1)
		   (length type))))
    (if (string-match "..." type)
	"variable arguments"
      "Couldn't recognize type")))


(defun argument-type-void-p (arg)
  "Return true if arguments type is void."
  (string-match "^void$" arg nil ))


(defun function-get-args (func)
  "Get arg list of a function"
  (function-args func))

(defun function-get-return-type (func)
  "Gets the return type of a function.
Argument is produced by find-c-function-by-parts."
  (function-return-type func))

(defun function-get-name (func)
  "Gets the return name of a function.
Argument is produced by find-c-function-by-parts."
  (function-name func))

(defun c-modep ()
  "Check for compatibility with c-mode."
  (or (eq major-mode 'c-mode)
      (eq major-mode 'c++-mode)))

(defun header-insert-return-values ()
  "Insert all values returned by the following function."
  (interactive)
  (let ((values (find-function-return-values))
	(header-prefix (header-prefix-string)))
    (while values
      (insert "\n" header-prefix "\t" (car (car values)) " -- ")
      (setq values (cdr values)))))
    

(defun find-function-return-values ()
  "Returns alist of function return values.
Current, the alist properties of the alist are not used."
  (let ((end nil))
      (save-excursion
	(if (not
	     (search-forward "{" nil 't))
	    ()
	  (backward-char 1)
	  (save-excursion
	    (forward-sexp)
	    (setq end (point)))
	  (find-function-build-alist end nil)))))

(defun find-function-build-alist (end list)
  "Return an alist of function return values."
  (let ((value))
  (if (not
       (re-search-forward "\\breturn\\b \\(.*\\);" end 't))
      (reverse list)
    (setq value (buffer-substring (match-beginning 1) (match-end 1)))
    (if (assoc value list)
	nil
      (setq list (cons (cons value nil) list)))
    (find-function-build-alist end list))))
      
      
      


(provide 'protos)
