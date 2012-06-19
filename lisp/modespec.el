;;; modespec.el --- mode specialized functions

;; Copyright (C) 1996 Dewey M. Sasser
     
;; Author: Dewey M. Sasser <dewey@newvision.com>
;; Created: 30 Oct 1996
;; $Version$
;; Keywords: advice major-mode

;;; Commentary:
;; This package is designed to make it easier for lisp programmers to
;; make customizable programs.  The basic primise is that the
;; programmer defines a number of functions that can be specialized on
;; the major mode of the current buffer

;;; Internals:
;;  When a function is made mode specific, a property is added to its
;;  plist.  This propertys car is the default value and it's cdr is an
;;  alist consisting of (mode . function) pairs.  Any function that
;;  can be called by apply is valid as a function 
;;
;;  

(if (string-lessp emacs-version "19.30")
    (require 'backquote))


(defmacro def-mode-spec-function (function-name args options
						       &rest default-body)
  "Define a function to be mode specific.
FUNCTION-NAME is the name of the function (as would be specified to
defun).
ARGS is the argument list,
OPTIONS is a list whose first element is the name of the symbol that
will contain the definition of function.  If nil, an anonymous
function is used.  If non nil, this name can be used with
debug-on-entry and will be printed in debugging stack traces.
DEFAULT-BODY is the body of the default implementation.
"
  `(define-mode-specific-function ',function-name ',args ',options
     (function (lambda args default-body))))


(defun define-mode-specific-function (function-name args options
						    lambda-expr)
  "Do necessary book keeping to arrange for FUNCTION-NAME to be a mode
specific function taking ARGS."
  (if (is-mode-specific-function function-name)
      (modespec-add-or-change-default function-name args options
				      default-body)
    (if (symbol-function function-name)
	;; It alreay has a definition, but that definition is *not* a
	;; mode-specific function
	(error "Function %s has a normal definition" function-name)
      (modespec-setup-base-function function-name args options lambda-expr)
      (modespec-add-or-change-default function-name args options
				      default-body))))

(defun modespec-setup-base-function (function-name args options
						   lambda-expr)
  "Set up the function so that it will call its mode specific method"
  (fset function-name (function (lambda (


;;; modespec.el ends here
