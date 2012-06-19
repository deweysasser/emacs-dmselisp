;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c-stuff.el -- Dewey's support for C programming
;; Author          : Dewey M. Sasser <dewey@jenna.blake7.nvs.com>
;; Created On      : Sun Mar 27 19:48:10 1994
;; Last Modified By: Dewey M. Sasser <dsasser@cerulean.com>
;; Last Modified On: Tue May 30 21:32:58 2000
;; Update Count    : 115
;; Status          : $State: Exp $
;; Name            : $Name: HEAD $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defvar include-copyright-notice nil
;;   (defvar header-copyright-notice nil
;;   (defun* skip-comment (arg)
;;   (defmodemethod insert-header-wrapper-internal default (buf-name)
;;   (defmodemethod insert-header-wrapper-internal c++-mode (buf-name)
;;   (defun insert-header-wrapper ()
;;   (defun buffer-get-c-define-name ()
;;   (defun h-file-wrapper ()
;;   (defun c-mode-insert-copyright ()
;;   (defmodemethod insert-ID-String-internal default ()
;;   (defun insert-ID-String ()
;; 
;; $RCSfile: c-stuff.el,v $
;; $Revision: 1.15 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)
(require 'modefn)

; This hack is so that it is not an error if fcreate is not around
(if (not (featurep 'fcreate))		;if it is not loaded
    (load "fcreate" 't))		;load file created hooks

(require 'alists)			;this is an error if not existant

(defvar include-copyright-notice nil
  "*Set this to nil if copyright notice should be inhibited.")

(defvar header-copyright-notice nil
  "*A string containing a copyright disclaimer to be inserted into all headers.
   This string needs no leading blanks and may contain any number of lines.
   May be nil.")



(defun* skip-comment (arg)
  "Skip ARG number of comments"
  (let ()
    (while arg
      (skip-syntax-forward " >")
      (if (looking-at comment-start )
	  (if
	      (if (or (not comment-end)
		      (string= comment-end ""))
		  (progn
		    (end-of-line)
		    nil)
		(not (search-forward comment-end nil 't)))
	      (error "Mismatched comments"))
	(return-from skip-comment))
      (setq arg (1- arg)))))


;;;###autoload
(defmodemethod insert-header-wrapper-internal default (buf-name)
  (insert "\n#ifndef " buf-name "\n#define " buf-name "\n\n"
	  "#ifdef __cplusplus\nextern \"C\" {\n#endif\n")
  (goto-char (point-max))
  (insert"\n\n\n#ifdef __cplusplus\n}\n#endif\n"
	 "#endif " comment-start " " buf-name " " comment-end))

;;;###autoload
(defmodemethod insert-header-wrapper-internal c++-mode (buf-name)
  (if (looking-at "/\\*")
      (search-forward "*/" nil 't))
  (while (looking-at "//")
    (forward-line 1))
  (insert "\n#ifndef " buf-name "\n#define " buf-name "\n\n")
  (end-of-buffer)
  (insert "\n\n\n#endif " comment-start " " buf-name " " comment-end))


;;;###autoload
(defun insert-header-wrapper ()
  "Place and #ifndef type multiple include protection around file.
Example:
#ifndef THISFILE_H
#define THISFILE_H
/* stuff */
#endif  /* THIFILE_H */

This skips the first comment in the file to allow for a comment header."
  (interactive)
  (let (
	(buf-name (buffer-get-c-define-name)))
    (save-excursion
      (beginning-of-buffer)
      (let ((comment-start (trim-whitespace comment-start))
	    (comment-end (trim-whitespace comment-end)))
;;	(skip-comment 1))
	(if (looking-at comment-start)
	    (search-forward comment-end nil 't)))
      (insert-header-wrapper-internal buf-name))))



;;;###autoload
(defun buffer-get-c-define-name ()
  "Return buffer name suitable for a c token, in upper case.
Example:  buffer: temp.h  return: TEMP_H"
  (let* ((temp (upcase (file-name-nondirectory (buffer-file-name))))
	(val 1))
    (while (setq val (string-match "\\.\\|-" temp val))
      (aset temp val ?_))
    temp))
;;;###autoload
(defun h-file-wrapper ()
  "If this is a .h file, call insert-header-wrapper."
  (if (and
       (or
	(eq major-mode 'c-mode)
	(eq major-mode 'c++-mode))
	(string-match "\\.h$\\|\\.hpp$"
		    (file-name-nondirectory
		     (buffer-file-name))))
      (progn
	(insert-header-wrapper))))
  

;;;###autoload
(defun c-mode-insert-copyright ()
  "Inserts a copyright string in a static char * at the beginning file.
This function uses the same copyright message as inserted in the header
in the headers package."
  (interactive)
  (if include-copyright-notice
  (save-excursion
    (goto-char (point-max))
    (let ((start)
	  (end))
      (insert "\n\nstatic char CopyrightMessage[]=\n")
      (setq start (point))
      (insert "\""
	      header-copyright-notice
	      "\";")
      (setq end (1+ (point)))
      (message "End is at %d" end)
      (goto-char start)
      (while (search-forward "\n" end t)
	(replace-match "\\n" nil t))
      (goto-char end)
      (end-of-line)
      (insert "\n")))))

;;(if (and
;;     (featurep 'fcreate)		; file creation hooks
;;     (not (featurep 'c-stuff)))		; c-stuff not loaded before
;;    (progn
;;      (append-to-list created-file-alist (cons "\\.h$" 'insert-header-wrapper))
;;      (append-to-list created-file-alist
;;		      (cons "\\.c$" 'c-mode-insert-copyright))))

(defmodemethod insert-ID-String-internal default ()
  "Insert an RCS ID string"
  (let ((fn (buffer-get-c-define-name)))
    (insert
     "static char "
     fn
     "_ID_STRING[]=\"$Id: c-stuff.el,v 1.15 2000/06/19 15:45:41 dewey Exp $\";\n")))

;;;###autoload
(defun insert-ID-String ()
  "Insert an RCS ID string"
  (interactive)
  (insert-ID-String-internal))



(provide 'c-stuff)
