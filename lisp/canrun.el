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
;;   (defvar cando::can-run-list nil
;;   (defvar cando::cache nil
;;   (defun setcando-function (name value)
;;   (defmacro setcando (name &rest body)
;;   (defun cando (name)
;;   (defun cando::every (func seq)
;;   (defun reset-cando (name)
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: canrun.el,v $
;; $Revision: 1.3 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (string-lessp emacs-version "19.30")
    (require 'backquote))

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
  "Define conditions under which name can be done"
  `(setcando-function ,name ',body))

(defun cando (name)
  (let ((found (assq name cando::can-run-list)))
    (if found
	(cando::every 'eval (cdr found)))))


(setcando 'hilit19
	  (cando 'faces))

(setcando 'cl
	  (cando 'whatever)
	  (string-lessp "19" emacs-version))

(setcando 'whatever 't)

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

