;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; patches.el -- Version 19 stuff for Version 18
;; Author          : Dewey M. Sasser <dewey@athena.mit.edu>
;; Created On      : Sun Oct 31 21:07:41 1993
;; Status          : $State: Exp $
;; Name            : $Name: HEAD $
;; PURPOSE
;; 	This will fill the holes in my commonly used packages where
;;      functions and variables are missing in Version 18.
;;
;;      Documentation string take from OEMACS (emacs version
;;      MSDOS/Windows GNU Emacs 19.19.0 of Sun Aug 22 1993 on hostigos
;;      (ms-dos) 
;; 
;; TABLE OF CONTENTS
;;   (defun skip-syntax-backward (syntax &optional lim)
;;   (defun skip-syntax-forward (syntax &optional lim)
;;   (defun char-in-string (char string)
;;   (defun run-hooks (&rest hooklist)
;;   (defconst run-hooks 'run-hooks
;;   (defun add-hook (hook function &optional append)
;;   (defun remove-hook (hook function)
;; 
;; $RCSfile: patches.el,v $
;; $Revision: 1.6 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Elmakedoc:  Automatic Prototyping Destinations
;  functions: patches.doc
;  variables: patches.doc


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; skip-syntax-backward -- skip backwards chars with certain syntax
;; AUTHOR:      Dewey M. Sasser <dewey@athena.mit.edu>
;; SYNOPSIS
;;   
;;   (defun skip-syntax-backward (syntax &optional lim)
;;   
;; DESCRIPTION
;;   Skips backwards over characters whos syntax in in the string
;;   "syntax". Leaves the point after first character not in syntax.
;; NOTES
;;   |><|
;; CAVEATS AND BUGS
;;   Doesn't do any error checking of it's own

(defun skip-syntax-backward (syntax &optional lim)
  "Move point backward across chars in specified syntax classes.\n\
SYNTAX is a string of syntax code characters.
Stop on reaching a char whose syntax is not in SYNTAX, or at position LIM.
If SYNTAX starts with ^, skip characters whose syntax is NOT in SYNTAX.
This function returns the distance traveled, either zero or negative.

arguments: (syntax &optional lim)"
  (interactive "sEnter Syntax to skip: ")
  (if lim
      ()
    (setq lim 1))
  (while (and
	  (char-in-string (char-syntax (preceding-char)) syntax)
	  (> (point) lim))
    (backward-char)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; skip-syntax-forward -- skip forward chars with certain syntax
;; AUTHOR:      Dewey M. Sasser <dewey@athena.mit.edu>
;; SYNOPSIS
;;   
;;   
;;   (defun skip-syntax-forward (syntax &optional lim)
;;   
;; DESCRIPTION
;;   Skips forwards over characters whos syntax in in the string
;;   "syntax". Leaves the point after first character not in syntax.
;; NOTES
;;   |><|
;; CAVEATS AND BUGS
;;   Doesn't do any error checking of it's own


(defun skip-syntax-forward (syntax &optional lim)
  "Move point forward across chars in specified syntax classes.\n\
SYNTAX is a string of syntax code characters.
Stop before a char whose syntax is not in SYNTAX, or at position LIM.
If SYNTAX starts with ^, skip characters whose syntax is NOT in SYNTAX.
This function returns the distance traveled, either zero or positive.

arguments: (syntax &optional lim)"
  (interactive "sEnter Syntax to skip: ")
  (if lim
      ()
    (setq lim (point-max)))
  (while (and
	  (char-in-string (char-syntax (following-char)) syntax)
	  (< (point) lim))
    (forward-char)))               
  


(defun char-in-string (char string)
  "Return 't if char is found in string."
  (let ((len (length string))
	(count 0)
	(return nil))
    (while (< count len)
      (if (char-equal char (aref string count))
	  (setq return 't))
      (setq count (1+ count)))
    return))
	     
    
	  

(defun run-hooks (&rest hooklist)
  "Takes hook names and runs each one in turn.  Major mode functions use this.
Each argument should be a symbol, a hook variable.
These symbols are processed in the order specified.
If a hook symbol has a non-nil value, that value may be a function
or a list of functions to be called to run the hook.
If the value is a function, it is called with no arguments.
If it is a list, the elements are called, in order, with no arguments."
  (while hooklist
    (let ((sym (car hooklist)))
      (and (boundp sym)
	   (symbol-value sym)
	   (let ((value (symbol-value sym)))
	     (if (and (listp value) (not (eq (car value) 'lambda)))
		 (mapcar 'funcall value)
	       (funcall value)))))
    (setq hooklist (cdr hooklist))))

;; Tell C code how to call this function.
(defconst run-hooks 'run-hooks
  "Variable by which C primitives find the function `run-hooks'.
Don't change it.")

(defun add-hook (hook function &optional append)
  "Add to the value of HOOK the function FUNCTION.
FUNCTION is not added if already present.
FUNCTION is added (if necessary) at the beginning of the hook list
unless the optional argument APPEND is non-nil, in which case
FUNCTION is added at the end.

HOOK should be a symbol, and FUNCTION may be any valid function.  If
HOOK is void, it is first set to nil.  If HOOK's value is a single
function, it is changed to a list of functions."
  (or (boundp hook) (set hook nil))
  ;; If the hook value is a single function, turn it into a list.
  (let ((old (symbol-value hook)))
    (if (or (not (listp old)) (eq (car old) 'lambda))
	(set hook (list old))))
  (or (if (consp function)
	  ;; Clever way to tell whether a given lambda-expression
	  ;; is equal to anything in the hook.
	  (let ((tail (assoc (cdr function) (symbol-value hook))))
	    (equal function tail))
	(memq function (symbol-value hook)))
      (set hook 
	   (if append
	       (nconc (symbol-value hook) (list function))
	     (cons function (symbol-value hook))))))

(defun remove-hook (hook function)
  "Remove from the value of HOOK the function FUNCTION.
HOOK should be a symbol, and FUNCTION may be any valid function.  If
FUNCTION isn't the value of HOOK, or, if FUNCTION doesn't appear in the
list of hooks to run in HOOK, then nothing is done.  See `add-hook'."
  (if (or (not (boundp hook))		;unbound symbol, or
	  (null (symbol-value hook))	;value is nil, or
	  (null function))		;function is nil, then
      nil				;Do nothing.
    (let ((hook-value (symbol-value hook)))
      (if (consp hook-value)
	  (setq hook-value (delete function hook-value))
	(if (eq hook-value function)
	    (setq hook-value nil)))
      (set hook hook-value))))

