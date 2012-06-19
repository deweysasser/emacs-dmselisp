;;; DIREDSUP.EL --- Additional commands for dired

;; Copyright (C) 1998 Dewey M. Sasser

;; Author: Dewey M. Sasser <dewey@newvision.com>
;; Maintainer: Dewey M. Sasser <dewey@newvision.com>
;; Created: 18 Dec 1997
;; Version: 1.10
;; Keywords:

 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to <dewey@newvision.com>) or
;; from the Free Software Foundation, Inc., 675 Mass Ave, Cambridge,
;; MA 02139, USA.

;; LCD Archive Entry:
;; diredsup|Dewey M. Sasser|<dewey@newvision.com>
;; |Additional commands for dired
;; |$Date: 2000/06/19 15:45:46 $|$Revision: 1.13 $|~/packages/diredsup.el

;;; Commentary:

;;; Change log:
;; $Log: diredsup.el,v $
;; Revision 1.13  2000/06/19 15:45:46  dewey
;; Removed ^M chars
;;
;; Revision 1.12  1998/02/04 20:00:35  dewey
;; Modified diredsup to add `dired-do-this' & released it.
;;
;; Revision 1.11  1998/01/22 20:15:32  dewey
;; Added known bug
;;
;; Previous Revisions:
;; o Changed Change Log section
;; o Added docs for aborting dired-do-this
;; o Modified dired-do-this to take a prefix argument which means repeat.
;; o Added bug submission function
;; o Added autoload
;; o Added docstrings
;; o Added dired-do-this
;;

;;; Known Bugs:
;;
;;  dired-do-this cannot be exited with M-x exit-recursive-edit,
;;  because those characters are recorded as part of the macro.  The
;;  general solution to this problem is not worth doing.  As a work
;;  around, use the key binding for exit-recursive-edit (By default
;;  M-C-c.) 

;;; Code:
(eval-when-compile
  (require 'cl))

(defconst diredsup-version (substring "$Revision: 1.13 $" 11 -2)
  "$Id: diredsup.el,v 1.13 2000/06/19 15:45:46 dewey Exp $

Report bugs using diredsup-submit-bug")

(defconst diredsup-maintainer "diredsup-maintainer@newvision.com"
  "Maintainer for diredsup, of course")

;;;###autoload
(defmacro while-editing (file &rest body)
  "Execute body while visiting file"
  (let ((current-buffer (gensym "while-visiting-")))
    `(let ((,current-buffer (current-buffer)))
       (unwind-protect
	   (progn
	     (find-file ,file)
	     ,@body)
	 (set-buffer ,current-buffer)
	 (set-window-buffer (selected-window) ,current-buffer)))))

(put 'while-editing 'lisp-indent-function 1)

(defmacro while-defining-kbd-macro (&rest body)
  "Arrange to be defining a macro during this body"
  `(unwind-protect
      (progn
	(start-kbd-macro nil)
	,@body)
    (end-kbd-macro)))
    

;;;###autoload
(defun dired-do-command (command)
  "Apply COMMAND to all files marked in dired"
  (interactive "CCommand: ")
  (let ((files (dired-get-marked-files)))
    (mapcar #'(lambda (x)
		(while-editing x
		  (call-interactively command)))
	    files)))


;;;###autoload
(defun dired-do-this (prefix)
  "Do the recorded \"thing\" on all files.
Use \\[exit-recursive-edit] to end edits on the first file and process
the other files.  Use \\[abort-recursive-edit] to abort the operation."
  (interactive "P")
  (let ((files (dired-get-marked-files)))
    (if (not (and prefix last-kbd-macro))
	(progn
	  (while-editing (first files)
	    (while-defining-kbd-macro
	     (recursive-edit)))
	  (pop files)))
    (mapcar #'(lambda (x)
		(while-editing x
		  (message "Processing %s" x)
		  (execute-kbd-macro last-kbd-macro)))
	    files)))

(defun diredsup-submit-bug ()
  "Submit a diredsup bug"
  (interactive)
  (if (yes-or-no-p "Submit a bug on diredsup? ")
      (progn
	(require 'reporter)
	(reporter-submit-bug-report diredsup-maintainer
				    (concat "diredsup"
					    " version "
					    diredsup-version)
				    '(last-kbd-macro)))))

(provide 'diredsup)
		  
;;; DIREDSUP.EL ends here

