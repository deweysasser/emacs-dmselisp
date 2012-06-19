;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c++-tools.el -- tools for programming in c++
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser <dewey@newvision.com>
;; Created On      : Wed Dec 11 17:12:38 1996
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defun make-multiple-c++-accessors (arg)
;;   (defun make-c++-accessors ()
;;   (defun get-arg-name (arg)
;;   (defun insert-get-accessor (type basename)
;;   (defun insert-set-accessor (type basename)
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: c++-tools.el,v $
;; $Revision: 1.2 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-multiple-c++-accessors (arg)
  "make many accessors, formatting and deleting old lines"
  (interactive "p")
  (insert "\npublic:   // accessors\n")
  (while (< 0 arg)
    (make-c++-accessors)
    (kill-line 1)
    (setq arg (1- arg)))
  (beginning-of-defun)
  (indent-c-exp))
    

;;;###autoload
(defun make-c++-accessors ()
  "Make accessors for the current line's data type"
  (interactive)
  (let* (line arg)
    (setq line (buffer-substring-no-properties (save-excursion (beginning-of-line) (point))
					       (save-excursion (end-of-line) (point))))
    (if (string-match "[ \t]*\\(.*\\);" line)
	(progn
	  (setq line (substring line (match-beginning 1) (match-end 1)))
	  (setq arg (make-c-argument-list line))
	  (if arg
	      (progn
		(insert-get-accessor (argument-type arg) (get-arg-name arg))
		(insert-set-accessor (argument-type arg) (get-arg-name arg))
		))))))
    

(defun get-arg-name (arg)
  "Get the name of the argument with \"its\" removed"
  (let ((where (string-match "^its" (argument-name arg))))
    (if where
	(substring (argument-name arg) (match-end 0))
      (argument-name arg))))

(defun insert-get-accessor (type basename)
  "Insert the GetXXX accessor"
  (insert "\n" type " Get" basename "() { return its" basename ";};");
  )

(defun insert-set-accessor (type basename)
  "Insert the GetXXX accessor"
  (insert "\nint Set" basename "(const " type " the" basename
	  ")\n  { its" basename " = the" basename "; return 1;};\n")
  )

(provide 'c++-tools)