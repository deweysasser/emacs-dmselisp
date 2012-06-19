;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spellsup.el -- Spelling support
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser <dewey@newvision.com>
;; Created On      : Wed Feb 26 14:37:24 1997
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defun ispell-comments ()
;;   (defun ispell-strings ()
;;   (defun ispell-things (start-thing end-thing)
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: spellsup.el,v $
;; $Revision: 1.2 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)

;;;###autoload
(defun ispell-comments ()
  "Run Ispell region on all comments in the buffer"
  (interactive)
  (ispell-things comment-start comment-end))

;;;###autoload
(defun ispell-strings ()
  "Run Ispell region on all strings in the current buffer."
  (interactive)
  (flet ((search ()
		 (let (result)
		   (while (and
			   (not (eobp))
			   (setq result (re-search-forward "\\s\"" nil 't))
			   (eql ?\\
				(char-syntax (char-after (-
							  (point) 2))))))
		   result)))
    (save-excursion
      (goto-char (point-min))
      (while (search)
	(let ((start (point)))
	  (search)
	  (ispell-region start (point)))))))


(defun ispell-things (start-thing end-thing)
  "Run Ispell Region on all \"Things\" delimited by START-THING and
END-THING in the buffer"
  (save-excursion
    (goto-char (point-min))
    (while (search-forward  start-thing nil 't)
      (let ((start (point)))
	(if (string= end-thing "")
	    (end-of-line)
	  (search-forward end-thing nil 't))
	(ispell-region start (point))))))


"blah"