;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; canrun.el -- Find if a certain feature can run
;; 
;; Author          : Dewey M. Sasser
;; Created On      : Mon Feb 26 09:27:33 1996
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defun system-name-only ()
;;   (defmacro on-computer (computer-name &rest body)
;;   (defun system-domain ()
;;   (defmacro in-domain (domain-name &rest body)
;;   (defvar cando::can-run-list nil
;;   (defvar cando::cache nil
;;   (defun setcando-function (name value)
;;   (defmacro setcando (name &rest body)
;;   (defun cando (name)
;;   (defun cando::some (func seq)
;;   (defun cando::every (func seq)
;;   (defun reset-cando (name)
;;   (defun cando::reset-all-cando ()
;;   (defmacro if-cando (condition &rest body)
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: cando.el,v $
;; $Revision: 1.4 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)

(defun system-name-only ()
  "Return only the machine name, not the domain"
  (downcase
   (let* ((dot (string-match "\\." system-name)))
     (if dot
	 (substring system-name 0 dot)
       system-name))))
    

(defmacro on-computer (computer-name &rest body)
  "*Execute BODY of macro only if the current system's name is COMPUTER-NAME.
COMPUTER-NAME should be allow lower case and it will match only the
hostname part of the computer's name."
  `(let ((name (system-name-only)))
     (if
	 (or
	  (and (listp ',computer-name)
	       (member name ',computer-name))
	  (eq ',computer-name 't)
	  (equal ',computer-name name))
	 (progn
	   ,@body))))

(defun system-domain ()
  "Return the current domain of the system"
  (let ((name (system-name)))
    (string-match "\\([a-zA-Z]+\\)\.\\(.*\\)" name)
  (substring name (match-beginning 2)
	     (match-end 2))))

(defmacro in-domain (domain-name &rest body)
  "*Execute BODY of macro only if the current system's name is COMPUTER-NAME.
COMPUTER-NAME should be allow lower case and it will match only the
hostname part of the computer's name."
  `(let ((name (system-domain)))
     (if
	 (or
	  (and (listp ',domain-name)
	       (member name ',domain-name))
	  (eq ',domain-name 't)
	  (equal ',domain-name name))
	 (progn
	   ,@body))))

(put 'on-computer 'lisp-indent-function 1)

(defvar cando::can-run-list nil
  "List of things that can run.  This list is made up of other lists
  where... ")

(defvar cando::cache nil
  "Cache of answers")

(defun setcando-function (name value)
  (setq cando::cache nil)
  (setq cando::can-run-list (cons (cons name value)
				  cando::can-run-list)))


(defmacro setcando (name &rest body)
  "*Define conditions under which name can be done.
NAME must be a symbol which is the cando symbol.  Rest is a list of
forms, each of which must evaluate to 't, for (cando 'NAME) to result
in 't.

Conditions are *not* evaluated immediately, but only when the cando is
tested.  It is therefore possible to refer to conditions, such as
other cando's, that have not yet been satisfied.

Beware, however, of circular dependencies."
  `(setcando-function ,name ',body))

(defun cando (name)
  "*Return 't if the current system can do NAME.
NAME can be a symbol, in which case conditions are evaluated for that
symbol, or name can be a list, possibly starting with `and' or `or'.
If name is a list that does not begin with `and' or `or', it implies `and'."

  (if (listp name)
      (let ((first (first name)))
	(cond
	 ((eq first 'not)
	  (not (apply 'cando (cdr name))))
	 ((eq first 'and)
	  (cando::every 'cando (cdr name)))
	 ((eq first 'or)
	  (cando::some 'cando (cdr name)))
	 ('t
	  (cando::every 'cando name))))
    (let ((found (assq name cando::can-run-list)))
      (if found
	  (cando::every 'eval (cdr found))))))


(defun cando::some (func seq)
  "Return 't if any element evaluates to 't"
  (let (result)
    (while (and
	    seq
	    (not result))
      (setq result (funcall func (car seq)))
      (setq seq (cdr seq)))
    result))

(defun cando::every (func seq)
  "Return t if every element is t"
  (catch 'ready
    (while seq
      (if (funcall func (car seq))
	  (setq seq (cdr seq))
	(throw 'ready nil)))
    't))



(defun reset-cando (name)
  "Reset a cando"
  (setcando name nil))

(defun cando::reset-all-cando ()
  (setq cando::can-run-list nil))

(defmacro if-cando (condition &rest body)
  "*Execute body if condition is true.  See documentation for function `cando' for details."
  `(if (cando ,condition)
       (progn
	 ,@body)))

(put 'if-cando 'lisp-indent-function 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;                  Definitions for some common packags                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 


(setcando 'hilit19
	  (string-lessp "19" emacs-version)
	  (cando 'faces))

(setcando 'time
	  (cando 'wakeup))

(setcando 'window-system
	  window-system)

(setcando 'screen-colors
	  (cando 'faces))

(setcando 'os2-pm-fonts
	  (cando 'faces)
	  (cando 'os2))

(setcando 'type-break
	  (cando 'wakeup))

(setcando 'mailcrypt
	  (cando 'mail)
	  (cando 'pgp))

(setcando 'font-lock
	  (cando 'faces))

(setcando 'os2
	  (eq system-type 'emx))

(setcando 'nt
	  (eq system-type 'windows-nt))