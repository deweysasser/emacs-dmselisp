;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert-hooks.el -- functions that manipulate hooks
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser
;; Created On      : Fri Mar 01 21:25:43 1996
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defun insert-hooks (function)
;;   (defun uninsert-hooks (function)
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: insert-hooks.el,v $
;; $Revision: 1.2 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun insert-hooks (function)
  "Hang hooks on a function.  Anything using this function should
probably use advice instead."
  (let ((old-func (symbol-function function))
	(before-hook (intern (concat
			      (symbol-name function)
			      "-before-hooks")))
	(after-hook (intern (concat
			     (symbol-name function)
			     "-after-hooks"))))
    
    (fset function
	  `(lambda (&rest args)
	     (interactive)
	     (run-hooks ',before-hook)
	     (prog1
		 (apply ,old-func args)
	       (run-hooks ',after-hook))))))


(defun uninsert-hooks (function)
  (let ((old-func
	 (cadadr (cadddr (symbol-function function)))))
    (fset function old-func)))



	 
(provide 'insert-hooks)