;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convert.el -- convert alchmey xpm output to sun readable
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser
;; Created On      : Thu Aug  4 10:42:49 1994
;; Status          : $State: Exp $
;; Name            : $Name: HEAD $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defun convert-xpm ()
;;   (defun xpm-insert-beginning ()
;;   (defun strchr (string char)
;;   (defun xpm-insert-end ()
;;   (defun unixify-lines ()
;;   (defun xpm-comment-line ()
;;   (defun xpm-quote-each-line ()
;; 
;; $RCSfile: convert.el,v $
;; $Revision: 1.6 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defun convert-xpm ()
  "Converts from the xpm format that Image Alchemy(tm) outputs to that
usable by xvt."
  (interactive)
  (c-mode)
  (unixify-lines)
  (beginning-of-buffer)
  (xpm-comment-line)
  (xpm-insert-beginning)
  (xpm-quote-each-line)
  (xpm-insert-end)
) 


(defun xpm-insert-beginning ()
  "Inserts the beginning part of an xpm, I.E., the
char *<filename>[] = {
where <filemame> is the name of the buffers file, with no extension"
  (let ((fname (file-name-nondirectory (buffer-file-name)))
	whereext)
    (setq whereext (strchr fname ?.))
    (setq fname (substring fname 0 whereext))
    (insert "char *" fname "[]={\n")
    (forward-line -1)))

(defun strchr (string char)
  "See _The C Programming Language_, by K&R"
  (let ((len (length string))
	(index 0))
    (while (and
	    (< index  len)
	    (not
	     (= (aref string index) char)))
      (setq index (1+ index)))
    (if (> len index)
	index
      nil)))

    



(defun xpm-insert-end ()
  "Insert the appropriate end, namely \"};\""
  (end-of-line)
  (insert "\n};"))



(defun unixify-lines ()
  "Convert End of Line marker from DOS to UNIX"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (replace-string "" "")))
   

(defun xpm-comment-line ()
  "Comment out the current line"
  (beginning-of-line)
  (if (looking-at (concat
		   (regexp-quote comment-start)
		   ".*"
		   (regexp-quote comment-end)))
      (forward-line)
    (insert comment-start)
    (end-of-line)
    (insert comment-end)
    (forward-char 1)))



(defun xpm-quote-each-line ()
  "make each line past the current point be wrapped in quotes ( \")
and separated by commas."
  (while (not
	  (= (forward-line) 1))
    (beginning-of-line)
    (insert "\"")
    (end-of-line)
    (insert "\","))
  (beginning-of-line)
  (if (looking-at "\"\",")
      (progn
	(kill-line 1)
	(delete-char -2)		;take out previous comma
	(forward-line 1)
	(insert "\n"))))
	
    