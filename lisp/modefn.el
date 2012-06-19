;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modefn.el -- define mode specific functions
;; 
;; Author          : Dewey M. Sasser (dewey@newvision.com)
;; Created On      : Tue Jun 06 14:52:50 1995
;; Status          : $State: Exp $
;; Name            : $Name: HEAD $
;; Keywords        : 
;; PURPOSE
;;
;; 	Implements functions whose methods can change according to the
;; 	mode of the current buffer.  Calling these functions is
;; 	completely transparent to the calling function.
;;      
;;      The purpose of this functionality is to be able to write
;;      functions that behave differently in different modes, without
;;      having to put in specific knowledge of the functions existence
;;      to the calling function.
;;
;; BUGS & Contact
;;
;;      Please send all bug reports, feature requests, etc to me at
;;      dewey@newvision.com.  If you extend this package in a useful
;;      way, please send me your extensions as well.
;;
;; Copyright
;;
;;      This package is copyright 1996 by Dewey M. Sasser.  All rights
;;      are reserved.  A License is grated to use, modify and
;;      distribute this code, provided that this copyright message is
;;      maintained and any modifications are specifically attributed
;;      to the author of the modifications and not to the original
;;      author.  You may not sell this package for profit.
;;      
;;      Please note that this package is not yet under the GNU General
;;      Public License, though it may be in the future.
;;
;; Warranty
;;
;;      None.  This package as provided as is in the hope that it will
;;      be useful, but without any warranty, express or implied, even
;;      that of fitness for a particular purpose.
;;
;;      That being said, please report any bugs or suggestions to
;;      Dewey Sasser <dewey@newvision.com>.
;;
;; Improvements
;;      Right now the package works by mangling names of functions and
;;      putting them into the normal symbol table, after installing
;;      the "generic" function which can produce those mangled names
;;      on the basis of the current mode.
;;
;;      In the future, it would probably be better to place the mode
;;      specific functions in a different place so that they don't
;;      clutter up the normal symbol table.  I've used a dollar sign
;;      ('$') in the mangled names, which doesn't seem too popular in
;;      lisp symbols, but it is a vulnerability
;;
;; Commentary
;;      Put this package somewhere in your lisp path and byte-compile
;;      it.  Any code that uses this package should (require
;;      'modefn).  Then, simply define mode specific functions and use
;;      those functions as normal.  For example:
;;      
;;	(defun document-function ()
;;	  (interactive)
;;	  (let ((args (parse-function-arguments)))
;;	    (do-something-with
;;	     args)))
;;	
;;	(defmodemethod parse-function-arguments c-mode ()
;;	  )
;;	
;;	(defmodemethod parse-function-arguments perl-mode ()
;;	  )
;;
;;      Note that the "mode" argument of defmodemethod is the same
;;      symbol as that found in the major-mode variable.
;;
;; Notification
;;      Please let me know if you are using this package.  I'll try to
;;      give you notices of improvements, etc.
;; 
;; TABLE OF CONTENTS
;;   (defmacro defmodealias (name mode &rest aliases)
;;   (defun modefn::defmodealias-internal (mode1 name1 aliases)
;;   (defmacro defmodegeneric (name args &rest body)
;;   (defmacro defmodemethod (name mode args &rest body)
;;   (defmacro modefn::define-new-function (mode name args &rest body)
;;   (defun modefn::modefn::define-generic-function-conditional (mode name args body)
;;   (defun modefn::function-no-docs (def)
;;   (defun modefn::define-generic-function (name doc)
;;   (defun modefn::get-function-documentation (name)
;;   (defun modefn::redefine-generic-to-default-method (name def)
;;   (defmacro modefn::define-mode-specific-function (mode name args &rest body)
;;   (defmacro modefn::define-mode-function (mode name args body)
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: modefn.el,v $
;; $Revision: 1.20 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (string-lessp emacs-version "19.30")
    (require 'backquote))

(require 'bytecomp)

(defmacro defmodealias (name mode &rest aliases)
  "*Make a mode specific function have two names.
NAME MODE [NAME MODE]*"
  ` (modefn::defmodealias-internal ', mode ', name ', aliases))


(defun modefn::defmodealias-internal (mode1 name1 aliases)
  "Make a mode specific function have two names -- glue layer"
  (let ((real-name1 (concat (symbol-name mode1) "$" (symbol-name name1)))
	(real-name2))
    (while (and aliases
		(car aliases)
		(car (cdr aliases)))
      (let* ((name (car aliases))
	     (mode (cadr aliases))
	    (fndef))
	(setq real-name2 (concat (symbol-name mode )
				"$"
				(symbol-name name)))
	(fset (intern real-name2)
	      ` (lambda (&rest args) (apply ', (intern real-name1) args)))
	(modefn::modefn::define-generic-function-conditional mode name nil nil)
	(setq aliases (cddr aliases))))
    (intern real-name2)))


(defmacro defmodegeneric (name args &rest body)
  "*defmodegeneric NAME ARGS DOCUMENTATION
Define a generic mode specialized function"
  ` (modefn::modefn::define-generic-function-conditional nil ', name ', args ', body))

(defmacro defmodemethod (name mode args &rest body)
  "*args: NAME MODE ARGS &rest BODY
Define a function that is mode specific.  If MODE is \"default\", the
function defined is the default if no more specific mode method is
defined" 
  
  ` (modefn::define-new-function , mode , name , args ,@ body))


(defmacro modefn::define-new-function (mode name args &rest body)
  "Define the global function for the defined mode specific function."
  `
   (progn
     (modefn::modefn::define-generic-function-conditional ', mode ', name
       ', args ', body)
     (modefn::define-mode-specific-function , mode , name , args ,@ body)))

(defun modefn::modefn::define-generic-function-conditional (mode name args body)
  (let ((doc (if (stringp (car body))
		 (car body)
	       "See mode specific documentation"))
	(oldfn))
    (if (fboundp name) ;;; one exists
	(progn
	  (setq oldfn (symbol-function name)); save old definition
	  (let ((doc2 (documentation (symbol-function name) 't)))
	    (if doc2
		(setq doc doc2)))))
    (modefn::define-generic-function name doc)
    (if (and oldfn
	 (not (equal (modefn::function-no-docs oldfn)
		  (modefn::function-no-docs (symbol-function name)))) ;if old def is not the new def
	 (not (equal "default" mode)))
	 (modefn::redefine-generic-to-default-method name oldfn)))
  name)


(defun modefn::function-no-docs (def)
  (if (listp def)
      (let ((args (cadr def))
	  (body (if (stringp (caddr def))
		    (cdddr def)
		  (cddr def))))
      ` (lambda , args , body))
    def))





(defun modefn::define-generic-function (name doc)
    (let ((defaultname (intern (concat "default$"
				       (symbol-name  name))))
	  (sname (symbol-name name)))
      (fset name
	    (byte-compile-sexp
	    ` (lambda (&rest args)
		 , doc
		 (let ((fname (intern (concat
				       (symbol-name major-mode)
				       "$"
				       , sname))))
		   (if (fboundp fname)
		       (apply fname args)
		     (if (fboundp ', defaultname)
			 (apply ', defaultname args)
		       (error "No function %s applicable to mode %s\n" ', name major-mode)))))))))

  

(defun modefn::get-function-documentation (name)
  "Return documentation for a function"
  (documentation name))
;;  (let ((body (symbol-function name)))
;;    (if (stringp (car body))
;;	(car body)
;;      nil)))

(defun modefn::redefine-generic-to-default-method (name def)
  "Just that"
  (let ((fundef def)
	(real-name (concat "default$" (symbol-name name))))
    (fset (intern real-name)
	  fundef)))

(defmacro flet-alias (bindings &rest body)
  (let ((real-bindings (mapcar*
			'(lambda (cell)
			   `,(car cell) (&rest args) (apply ',(cadr cell) args))
			bindings)))
  `(flet ,real-bindings
     ,@body)))



(defmacro modefn::define-mode-specific-function (mode name args &rest body)
  "Just that"
  (let* ((real-name (concat (symbol-name mode) "$" (symbol-name name)))
	 (default-name (concat "default" "$" (symbol-name name)))
	 (default-name-sym (intern default-name))
	 (sym (intern real-name)))
    ` (defun , sym  , args
	 (flet-alias ((call-next-mode-method , default-name-sym)
		      (call-default-mode-method , default-name-sym))
		     ,@ body))))

		      

(defmacro modefn::define-mode-function (mode name args body)
  "Defines a mode function"
  (let ((newname (intern (concat
			  (symbol-name mode)
			  "$"
			  (symbol-name name)))))
    `
     (defun , newname , args
      ,@ body)))

(defun modefn::autoload-defmodemethod-handler (form file)
  "Return autoload for FORM"
  (let* ((name (nth 1 form))
	 (docs (nth 4 form)))
    (unless (stringp docs)
      (setq docs nil))
    `(autoload ',name ,file ,docs)))

(if (fboundp 'autoload-add-form-handler)
    (autoload-add-form-handler 'defmodemethod
			       #'modefn::autoload-defmodemethod-handler))

(provide 'modefn)
