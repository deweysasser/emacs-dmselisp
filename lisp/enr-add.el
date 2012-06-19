;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; enr-add.el -- Additions to enriched mode
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser <dewey@newvision.com>
;; Created On      : Fri Dec 20 15:14:08 1996
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defun enriched-insert-toc ()
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: enr-add.el,v $
;; $Revision: 1.2 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun enriched-insert-toc ()
  "Update the table of contents with Sections"
  (interactive)
  (let ((toc))
    (save-excursion
      (while (re-search-forward "^Section .*$" nil 't)
	(setq toc (cons (buffer-substring-no-properties (match-beginning 0)
							(match-end 0))
			toc))
	))
    (mapcar '(lambda (x) (insert x "\n"))  (reverse toc))))