;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fundoc.el -- extract documentation from source code
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser
;; Created On      : Wed Jun 07 13:19:03 1995
;; Status          : $State: Exp $
;; Name            : $Name: HEAD $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;;
;; NOTES
;;      This file is not currently used.  A perl script was written to
;;      take its place.
;; 
;; TABLE OF CONTENTS
;;   (defun fdc-docify-dired-file ()
;;   (defun fdc-docify-directory ()
;;   (defun fdc-docify-buffer ()
;;   (defun private-function (theDocs)
;;   (defun find-end-of-function ()
;;   (defmodegeneric find-next-function ()
;;   (defmodemethod  find-next-function c-mode ()
;;   (defun get-doc-buffer-name (&optional buf)
;;   (defun format-for-ms-word (string)
;;   (defun one-line-paragraphs ()
;;   (defun format-arguments-for-ms-word (string &optional insert)
;;   (defun write-function-summary (func docs)
;;   (defun fdc-include-function (func docs)
;;   (defun add-optional-newline (string)
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: fundoc.el,v $
;; $Revision: 1.9 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'functions)


(defun fdc-docify-dired-file ()
  "Docify the current function in dired."
  (interactive)
  (dired-find-file)
  (beginning-of-buffer)
  (update-all-synopsis)
  (beginning-of-buffer)
  (fdc-docify-buffer)
  (save-buffer)
  (kill-buffer (current-buffer))
  (save-buffer)
  (kill-buffer (current-buffer)))

(defun fdc-docify-directory ()
  "Docify all .c files in current dired buffer."
  (interactive)
  (while (search-forward ".c" nil 't)
    (fdc-docify-dired-file)))



(defun fdc-docify-buffer ()
  "Create a buffer which consists of documentation extracted into
Dave's Word format.  All documentation must have synopses before
running this operation.  The function update-all-synopsis
(\\[update-all-synopsis]) will do this."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let* ((filename (get-doc-buffer-name
		      (current-buffer)))
	   (new-buf (generate-new-buffer filename))
	   (theFunc)
	   (theDocs))
      (while
	  (find-next-function)
	(setq theDocs (find-default-field-values (copy-proplist
						  default-documentation-alist)))
	(if (and
	     (get-list-property theDocs 'name 'value)
	     (not (private-function theDocs)))
	    (progn
	      (setq theFunc (find-function-by-parts))
	      (message "Processing function %s()..." (function-name theFunc))
	      (in-buffer new-buf
			 (write-function-summary theFunc theDocs))))
	(find-end-of-function))
      (set-buffer new-buf)
      (setf (buffer-file-name) filename)
      (set-window-buffer (selected-window) new-buf)
      (message "finished."))))

(defun private-function (theDocs)
  "Return 't if theDocs is documentation for a private function"
  (let ((vis (get-list-property theDocs "VISIBILITY" 'value)))
    (or (string= vis "PRIVATE")
	(string= vis "Private")
	(string= vis "private"))))
  
(defun find-end-of-function ()
  (search-forward "{" nil 't)
  (backward-char 1)
  (forward-sexp))

(defmodegeneric find-next-function ()
  "Position point immediately before next function.")

(defmodemethod  find-next-function c-mode ()
  "Position point immediately before next function.  This function
works only in ANSI C, and assumes that there is no comment between
first line of function and the body."
  (if (re-search-forward ")[ \t\n]*{" nil 't)
      (prog1
	(re-search-backward (concat
			   "\\("
			   (regexp-quote
			    (trim-whitespace comment-end))
			   "\\|}\\)")
			   nil 't)
	(forward-char 1))))

;;(defun find-next-function ()
;;  "Position point immediately before next function"
;;  (if (search-forward "{" nil 't)
;;      (re-search-backward "^[ \t]*$" nil 't)))
;;



(defun get-doc-buffer-name (&optional buf)
  "Return (string) name appropriate for doc buffer for the given
buffer"
  (let ((string))
  (if (not buf)
      (setq buf (current-buffer)))

  (cond
   ((stringp buf)			;yea, should overload, but I
					;haven't written CLOS yet
    (setq string buf))

   ((bufferp buf)
    (setq string (file-name-nondirectory (buffer-file-name buf))))

   ('t
    (error "Argument %s must be string or buffer" buf)))

  (if (string-match "\\.[^\\.]*$" string)
      (progn
	(setq string (substring string 0
				(match-beginning 0)))))
  (concat string ".doc")))
	

(defun format-for-ms-word (string)
  "Format the string so that paragraphs are all one line."
  (if string
      (in-temp-buffer
       (insert string)
       (one-line-paragraphs)
       (buffer-substring (point-min) (point-max)))
    ""))

(defun one-line-paragraphs ()
  "Make paragraphs separated by a single line into single lines each."
       (goto-char (point-min))
       (while (search-forward-regexp "^[ \t]*$" nil 't)
	 (replace-match "*&dms"))

       (goto-char (point-min))
;       (while (search-forward-regexp "\\(  +\\)\\|\\( ?[\t\n]+ ?\\)" nil 't)
       (while (search-forward-regexp "\n[ \t]*" nil 't)
	 (replace-match " "))

       (goto-char (point-min))
       (while (search-forward-regexp (concat (regexp-quote "*&dms")
					     "[ \t]*")
				     nil 't)
	 (replace-match "\n")))
  

;;(defun format-for-ms-word (string)
;;  "Format the string so that paragraphs are all one line."
;;  (if string
;;      (trim-all-whitespace string)
;;    ""))
    
(defun format-arguments-for-ms-word (string &optional insert)
  "Format the argument list for ms-word"
  (if string
      (let ((keyword (get-property "ARGUMENTS" 'fdc-header)))
	(if (or
	     (not insert)
	     (not keyword))
	    (setq keyword ""))
	(if (string= string "NONE")
	    (concat keyword "NONE\n")
	  (in-temp-buffer
	   (insert string)
	   (goto-char (point-min))
	   (while (search-forward "--" nil 't)
	     (replace-match "\t")
	     (beginning-of-line)
	     (insert "\n" keyword))
	   (one-line-paragraphs)
	   (goto-char (point-min))
	   (if (looking-at "^[ \t]*$")
	       (delete-char 1))
	   (add-optional-newline
	    (buffer-substring (point-min) (point-max))))))
	""))
  
   
(defun write-function-summary (func docs)
  "Write out the function summary in fdc format"
  (if (fdc-include-function func docs)
      (insert
   (get-list-property docs 'name 'fdc-header)
   (function-name func) "()\n"
   (get-list-property  docs 'name 'value) "\n"
   
   (get-list-property docs "SYNOPSIS" 'fdc-header)
   (trim-all-whitespace (get-list-property docs "SYNOPSIS" 'value)) "\n"

   "mdkwrbDESCRIPTION\n"
   (get-list-property docs "DESCRIPTION" 'fdc-header)
   (format-for-ms-word (get-list-property docs "DESCRIPTION" 'value))

   "mdkwrbARGUMENTS\n"
   (format-arguments-for-ms-word
    (get-list-property docs "ARGUMENTS" 'value) 't)
   
   "mdkwrbRETURNS\n"
   (get-list-property docs "RETURNS" 'fdc-header)
   (format-arguments-for-ms-word (get-list-property docs "RETURNS" 'value))
   
   "mdkwrbNOTES\n"
   (get-list-property docs "NOTES" 'fdc-header)
   (format-for-ms-word (get-list-property docs "NOTES" 'value)) "\n"

   "\n\n"
   )))

(defun fdc-include-function (func docs)
  "Return 't if the function should be included for documentation"
  't)

(defun add-optional-newline (string)
  "If the last character is STRING is not a newline, add one"
  (if (not (eq (aref string (1- (length string)))
	       ?\n))
      (concat string "\n")
    string))

(provide 'fundoc)