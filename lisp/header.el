;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; header.el --- Support for creation and automatic update of file headers
;; Author          : Lynn Slater
;; Created On      : Tue Aug  4 17:06:46 1987
;; Last Modified By: Dewey M. Sasser <dsasser@cerulean.com>
;; Last Modified On: Tue May 30 21:32:50 2000
;; Update Count    : 214
;; Status          : OK
;; PURPOSE
;; 	This file was origionally written by Lynn Slater.  It was an
;;      excellent package as it was, but I just can't seem to resist
;;      hacking on anything.  I improved the (make-divisor) I think,
;;      because it doen't include whitespace, but still allows comment
;;      beginning ending in spaces.  I'm going to send this to Lynn
;;      when I get a chance.  The rest of the stuff is mostly his
;;      code.  See the file "changelog" for details.
;;
;; TABLE OF CONTENTS
;;   (defvar command-line-hooks '())
;;   (defvar include-copyright-notice nil
;;   (defvar inhibit-email-address nil
;;   (defvar header-author-string nil
;;   (defvar header-max 2000
;;   (defvar header-copyright-notice nil
;;   (defvar header-status-string nil
;;   (defvar make-header-hooks '(
;;   (defun current-line ()
;;   (defun user-full-name-and-email ()
;;   (defun header-beginning ()
;;   (defun fill-line-with-preceding-character (&optional column character)
;;   (defun where-beginning-of-line ()
;;   (defun header-ending ()
;;   (defun header-blank ()
;;   (defun header-keywords ()
;;   (defun header-file-name ()
;;   (defun header-author ()
;;   (defun header-creation-date ()
;;   (defun header-modification-author ()
;;   (defun header-modification-date ()
;;   (defun header-update-count ()
;;   (defun header-status ()
;;   (defun header-history ()
;;   (defun header-purpose ()
;;   (defun header-toc ()
;;   (defun header-rcs ()
;;   (defun header-sccs ()
;;   (defun header-AFS ()
;;   (defun header-copyright ()
;;   (defun header-shell ()
;;   (defun header-mode-line ()
;;   (defun header-end-line ()
;;   (defvar file-header-update-alist ()
;;   (defun register-file-header-action (regexp function-to-call)
;;   (defun true-mode-name ()
;;   (defun make-header ()
;;   (defun make-revision ()
;;   (defun make-divisor (&optional end-col)
;;   (defun make-box-comment (&optional end-col)
;;   (defun current-d-m-y-string ()
;;   (defun update-file-header ()
;;   (defun delete-and-forget-line ()
;;   (defun update-write-count ()
;;   (defun update-last-modifier ()
;;   (defun update-last-modified-date ()
;;   (defun update-file-name ()
;;   (defun headerable-file-p (file)
;;   (defun uniqueify-list (list)
;;   (defvar header-required-mode nil
;;   (defun touch-headers ()
;;   (defun make-headers ()
;;   (defun set-header-mode ()
;;   (defun set-header-required-mode ()
;;   (defun widen-and-run-function (function)
;;   (defun widen-update-toc ()
;;   (defun widen-update-test-fields ()
;;   (defun copyright-notice-on ()
;;   (defun copyright-notice-off ()
;; 
;; $Locker:  $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copyright (C) 1988 Lynn Randolph Slater, Jr.
;; Copyright (C) 1989 Free Software Foundation, Inc.
;; This file is compatable with GNU Emacs but is not part of the official
;; distribution (yet).
;;
;; This file is distributed in the hope that it will be useful,
;; but without any warranty.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.
;;
;; Everyone is granted permission to copy, modify and redistribute
;; this file, but only under the conditions described in the
;; document "GNU Emacs copying permission notice".   An exact copy
;; of the document is supposed to have been given to you along with
;; this file so that you can know how you may redistribute it all.
;; It should be in a file named COPYING.  Among other things, the
;; copyright notice and this notice must be preserved on all copies.

;; HISTORY
;; 27-Oct-1993		Dewey M. Sasser
;;    Last Modified: Wed Oct 27 21:17:09 1993 #151 (Dewey M. Sasser)
;;    see file Changelog for history

;; This file adds support for the creation and automatic maintenence of file
;; headers such as the one above.
;;  User Commands:
;;   M-x make-header
;;   M-x make-revision
;;   M-x make-divisor
;;   M-x make-box-comment
;; Customizer commands
;;   register-file-header-action
;; Customizer variables
;;   header-copyright-notice
;;   make-header-hooks
;;
;; This file is particularly useful with the file-declarations package also
;;   by Lynn Slater.
;; Make this file header.el, byte-compile it in your path
;;
;; Read the first 20% of this file to learn how to customize.

;; History
;; 25-Sep-1989		Lynn Slater
;;    Last Modified: Mon Sep 25 15:12:16 1989 #119 (Lynn Slater)
;;    added -default-mode ahd headerable-file-p
;; 10-Sep-1989		Lynn Slater
;;    Last Modified: Wed Sep  6 17:36:00 1989 #110 (Lynn Slater)
;;    Seperated out header-mode-line and header-end. Headers are now really
;;    easy to modify.
;;    Added instructions for mode-specific headers.
;; 8-Aug-1989		Lynn Slater
;;    Last Modified: Thu Aug  3 08:04:06 1989 #88 (Lynn Slater)
;;    Changed structure to allow site/user customized headers
;; 24-Jun-1989		Lynn Slater
;;    Last Modified: Thu Jun 22 12:52:24 1989 #84 (Lynn Slater)
;;    restructured file, made the order of header actions not be significant.
;; 22-Jun-1989		Lynn Slater
;;    Last Modified: Thu Jun 22 11:40:53 1989 #82 (Lynn Slater)
;;    Made file header actions easier to declare
;;    Made sccs and rcs support be user settable.
;;    Added c-style support
;; 25-Jan-1989		Lynn Slater
;;    Last Modified: Wed Jan 25 12:03:23 1989 #78 (Lynn Slater)
;;    Added make-doc command
;; 25-Jan-1989		Lynn Slater
;;    Last Modified: Tue Sep  6 07:57:22 1988 #77 (Lynn Slater)
;;    made the make-revision command include the last-modified data
;; 31-Aug-1988		Lynn Slater
;;    Made the make-revision work in most modes
;;    Added the update-file-name command
;; 1-Mar-1988		Lynn Slater
;;   made the headers be as sensitive as possible to the proper
;;   comment chars.
;; 1-Mar-1988		Lynn Slater
;;   Made the mode be declared in each header
;; 26-Feb-1988		Lynn Slater
;;   added the make-revision call
(provide 'header)


;Elmakedoc:  Automatic Prototyping Destinations
;  functions: header.doc
;  variables: header.doc



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file has two major divisions: header creation and automatic header
;; maintenance.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Added by Dewey Sasser, Wed Oct 27 10:59:53 1993
(defvar command-line-hooks '())

(defvar include-copyright-notice nil
  "*Set this to nil if copyright notice should be inhibited.")

(defvar inhibit-email-address nil
  "*This is set non-nil if the package SHOULD NOT put in email addresses.")

;; Added by Dewey Sasser, Sun Oct 31 09:17:38 1993
;; This is inserted if it is non nil, giving the user the ability to
;; document functions without claiming credit.
(defvar header-author-string nil
  "*Inserted as the author if it is non nil, otherwise, the current \

user's name and email address is used.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User/Site Customizable Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar header-max 2000
  "*Is the number of characters at the start of a buffer that will be
   searched for header info to automatically update.")

(defvar header-copyright-notice nil
  "*A string containing a copyright disclaimer to be inserted into all headers.
   This string needs no leading blanks and may contain any number of lines.
   May be nil.")

(defvar header-status-string nil
  "*A string to be inserted as the status.\
If it doesn't exist, the status is
 \"Experimental\".  This is used by make-header on \\[make-header].")


(defvar make-header-hooks '(
			    header-beginning
			    header-file-name
			    header-copyright
			    ;;header-sccs
			    ;;header-AFS
			    header-author
			    header-creation-date
			    ;;header-modification-author
			    ;;header-modification-date
			    ;;header-update-count
			    header-status
			    header-keywords
			    ;; Re-enable the following lines if you wish
			    ;;header-blank
			    ;;header-history
			    header-purpose
			    header-blank
			    header-toc
			    header-blank
			    header-options
			    header-blank
			    header-rcs
			    header-ending
			    )

  "A list of functions which will insert the various header elements.
   Each function is started on a new line and is expected to end in a new line.
   Each function may insert any number of lines, but each line, including
   the first, must be started with the value of the header-prefix-string
   variable. (This variable holds the same value as would be returned by
   calling 'header-prefix-string but is faster to access.)
   Each function may set the following dynamically bound values:
     header-prefix-string -- mode specific comment sequence
     return-to        -- character position to which point will be moved
                         after all header functions are processed. Any
                         header function may set this, but only the last
                         set will take effect.

   It is reasonable to locally set these hooks according to certain
   modes. For example, a table of contents may only apply to code development
   modes and 'header-shell should only apply to shell scripts. See the
   instructions in header.el to do this.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode specific headers:
;;  Not all headers should look alike. Suppose that you have a unix script
;;  mode and want it to have a shell specifier line that all other headers
;;  do not have. To do this, Place the following lines
;;     (make-local-variable 'make-header-hooks)
;;     (setq make-header-hooks (cons 'header-shell
;;                                    (default-value 'make-header-hooks)))
;;  either in a hook called when the mode is invoked or in the code that
;;  establishes the mode.
;;
;;  Note that the header building blocks are automatically sensitive to the
;;  different comment characters in different modes.
;;
;; Mode specific update actions:
;;  Suppose something needs to be automatically maintained only in certian
;;  modes. An example is the .TH macro in man pages.  You can create mode
;;  specific update actions by placing lines such as the following in the
;;  mode creation function of in the mode hook.
;;    (make-local-variable 'file-header-update-alist)
;;    (register-file-header-action
;;      "^\.TH[ \t]+[^\" \t]+[ \t]+[^\" \t]+[ \t]+\"\\([^\"]*\\)\""
;;     'update-last-modified-date-macro)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define the individual header elements.  THESE ARE THE BUILDING BLOCKS
;; used to construct a site specific header.  You may add your own
;; functions either in this file or in your .emacs file.  The variable
;; make-header-hooks specifies the functions that will actually be called.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'testsup)

(defun current-line ()
  "Return the current line."
  (count-lines 1 (point)))
;;;###autoload
(defun user-full-name-and-email ()
  "Returns the full name and email address of the current user"
  (if inhibit-email-address
      (user-full-name)      
    (concat (user-full-name) " <" user-mail-address ">")))


(defun header-beginning ()
  "Puts the beginning comment character into the header."
    (insert comment-start)
    (fill-line-with-preceding-character)
    (insert "\n"))

;;;###autoload
(defun fill-line-with-preceding-character (&optional column character)
  "Fills a line with a character preceding the point.\
If column is not provided, fill-column is used.
If character is not provided, last non white space character is used."
  (interactive)
  (let ((here (point)))
	(skip-syntax-backward " ")		;go back to comment character
	(delete-region (point) here))
  (if character
      ()
    (setq character (preceding-char)))	;if we don't have char, get one
  (if column
      ()
    (setq column fill-column))
  (insert-char character (- column (current-column))))



(defun where-beginning-of-line ()
  "Return point of beginning of line."
  (save-excursion
    (beginning-of-line)
    (point)))


(defun header-ending ()
  "Puts the ending comment characters into the header."
  (let (eol)
    (declare (special header-prefix-string))
  (insert header-prefix-string)
  (fill-line-with-preceding-character)
  (save-excursion
    (insert comment-end)
    (setq eol (point)))
  (fixup-whitespace)
  (delete-char 1)			;delete the one space
  (goto-char eol)			;goto after the comment end
  (insert "\n")))

(defun header-blank ()
  "Places a blank line into a file header"
  (declare (special header-prefix-string))
  (insert header-prefix-string  "\n"))

(defun header-keywords ()
  "Places a Keywords: d into a file header"
  (declare (special header-prefix-string))
  (insert header-prefix-string  "Keywords        : \n"))

(defun header-file-name ()
  "Places the buffer's file name and leaves room for a description."
  (declare (special header-prefix-string))
  (insert header-prefix-string (file-name-nondirectory
				(buffer-file-name)) " -- \n")
  (setq return-to (1- (point))))

(defun header-author ()
  "Inserts the current user's name and email address as the module's author."
  (declare (special header-prefix-string))
  (insert header-prefix-string "Author          : "
	  (if header-author-string
	      header-author-string
	    (user-full-name-and-email))
	  "\n"))

(defun header-creation-date ()
  "Places today's data as the file creation date."
  (declare (special header-prefix-string))
  (insert header-prefix-string "Created On      : "  (current-time-string) "\n"))

(defun header-modification-author ()
  "Inserts the current user's name as the one who last modified the module.
   This will be overwritten with each file save if all the
   file-header-actions in the default header.el file are registered."
  (declare (special header-prefix-string))
  (insert header-prefix-string  "Last Modified By: \n"))

(defun header-modification-date ()
  "Inserts todays date as the time of last modification.
   This will be overwritten with each file save if all the
   file-header-actions in the default header.el file are registered."
  (declare (special header-prefix-string))
  (insert header-prefix-string  "Last Modified On: \n"))

(defun header-update-count ()
  "Inserts a count of the number of times the file has been saved.  This is
  often a more useful measure of 'age' and 'modifications' than dates
  recorded in the file system.  It is a handy code metric that is a
  surprisingly good indication of file complexity and can often help
  indicate which modules have been changed so much that they need a rethink.
  It also assist recovery from source control mixups."
  (declare (special header-prefix-string))
  (insert header-prefix-string  "Update Count    : 0\n"))

(defun header-status ()
  "Inserts a status line that should be manually edited to reflect the
   general condition of the entire module."
  (declare (special header-prefix-string))
  (insert header-prefix-string
	  "Status          : "
	  (if (stringp header-status-string)
	      header-status-string
	      (concat
	       "$" "State" "$")) "\n"))

(defun header-history ()
  "Inserts HISTORY line into header for later use by make-revision.
   Without this, make history will insert after the header."
  (declare (special header-prefix-string))
  (insert header-prefix-string  "HISTORY\n"))

(defun header-purpose ()
  "Inserts a line that starts a section that should describe the purpose of
   the file/module."
  (declare (special header-prefix-string))
  (insert header-prefix-string  "PURPOSE\n"
	  header-prefix-string "	|>Description of modules purpose<|\n"))

(defun header-toc ()
  "Inserts a line that starts a section that should describe each function
   defined in the module that is significant to external users."
  (declare (special header-prefix-string))
  (insert header-prefix-string "TABLE OF CONTENTS\n"))

(defun header-rcs ()
  "Inserts lines to record rcs information."
  (declare (special header-prefix-string))
  (insert header-prefix-string
	  "$" "RCSfile" "$\n"
	  header-prefix-string
	  "$" "Revision" "$\n"
;;	  header-prefix-string
;;	  "$" "Log" "$\n")
  ))

(defun header-sccs ()
  "Inserts a line to record sccs information."
  (declare (special header-prefix-string))
  (insert header-prefix-string "SCCS Status     : %W%\t%G%\n"))

(defun header-AFS ()
  "Inserts a line to record SHAPE information."
  (declare (special header-prefix-string))
  (insert header-prefix-string "AFSID           : $__Header$\n"))

(defun header-copyright ()
  "Inserts the copyright notice stored in the variable header-copyright-notice.
   This value may be nil."
  (declare (special header-prefix-string))
  (if (and include-copyright-notice
	   header-copyright-notice)
      (let ((start (point)))
	(insert header-copyright-notice)
	(save-restriction
	  (narrow-to-region start (point))
	  (goto-char (point-min))
	  ;; I must now insert the header prefix.  I cannot just do a
	  ;; replace string because that would cause too many undo boundries.
	  (insert header-prefix-string)
	  (while (progn (skip-chars-forward "^\n") (looking-at "\n"))
	    (forward-char 1)
	    (insert header-prefix-string))
	  (goto-char (point-max)))
	(insert "\n"))))

(defun header-shell ()
  "Inserts a kernel shell specifier line. Uses the same shell named in
   explicit-shell-file-name, the ESHELL environment variable, the SHELL
    environment variable, or '/bin/sh'. (This is the same shell that the
   shell command uses."
  (insert "#!" (or (and (boundp 'explicit-shell-file-name)
			 explicit-shell-file-name)
		   (getenv "ESHELL")
		   (getenv "SHELL")
		   "/bin/sh")
	  "\n"))

(defun header-mode-line ()
  "Inserts the mode line into the buffer."
  (let* ((mode-declaration
	  (concat " -*- Mode: " (true-mode-name)
		  (if (assoc 'c-style (buffer-local-variables))
		      (concat "; C-Style: " (symbol-name c-style))
		    "")
		  " -*- "))
	 (md-length (length mode-declaration)))
    (insert (cond
	     ((and comment-start (= (length comment-start) 1))
	      ;; I will presume that the comment start character is
	      ;; the filler character.
	      (concat comment-start comment-start
		      (make-string (/ (- 77 md-length) 2)
				   (aref comment-start 0))
		      mode-declaration
		      (make-string (/ (- 78 md-length) 2)
				   (aref comment-start 0))
		      ))
	     (comment-start-p
	      ;; I will presume that spaces will fill the gaps
	      (concat comment-start
		      (make-string (/ (- 79 md-length (length comment-start))
				      2) ?\ )
		      mode-declaration))
	     (t;; there is no known comment-start. Presume lisp
	      (concat ";;"
		      (make-string (/ (- 77 md-length) 2) ?\;)
		      mode-declaration
		      (make-string (/ (- 78 md-length) 2) ?\;))))
	    "\n"))
  )

(defun header-end-line ()
  "Inserts a trailing divisor line for headers."
  (insert (cond (comment-end-p comment-end)
		((and comment-start (= (length comment-start) 1))
		 (make-string 79 (aref comment-start 0)))
		(comment-start-p comment-start)
		(t       (make-string 79 ?\;)))
	  "\n\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System Variables -- Do not modify. Instead, call the functions that modify.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar file-header-update-alist ()
  "Used by update file header to know what to do in the file. Is a list of
   sets of cons cells where the car is a regexp string and the cdr is the
   fcn to call if the string is found near the start of the file.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User function to declare header actions on a save file.
;;   See examples at the end of this file.
;; Invoke from site-init.el or in .emacs.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun register-file-header-action (regexp function-to-call)
  "Accepts REGEXP and FUNCTION-TO-CALL. Records
   FUNCTION-TO-CALL as the appropriate action to take if the REGEXP is
   found in the file header when a file is written.  The function will be
   called with the cursor located just after the matched regexp.

   Calling this fcn twice with the same arguments overwrites
   the previous FUNCTION-TO-CALL"
  (let ((ml (assoc regexp file-header-update-alist)))
    (if ml
	(setcdr ml function-to-call);; overwrite old defn
      ;; This entry is new to us. Add to the master alist
      (setq file-header-update-alist
	    (cons (cons regexp function-to-call)
		  file-header-update-alist)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Header and file division header creation code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun true-mode-name ()
  "Returns the name of the mode in such a form that the mode may be
  re-established by calling the function named by appending '-name' to
  this string.  This differs from the variable called mode-name in that
  this is guaranteed to work while the values held by the variable may
  have embedded spaces or other junk.

  THIS MODE NAME IS GUARANTEED OK TO USE IN THE MODE LINE."
  (let ((major-mode-name (symbol-name major-mode)))
    (capitalize (substring major-mode-name 0
			   (or   (string-match "-mode" major-mode-name)
				 (length major-mode-name))))))



;;;###autoload
(defun make-header ()
  "Makes a standard file header at the top of the buffer. A header is
   composed of a mode line, a body, and an end line.  The body is
   constructed by calling the functions in make-header-hooks.
   The mode line and end lines start and terminate block comments while the
   body lines just have to continue the comment. "
  (interactive)
  (beginning-of-buffer)       ;; leave the mark at the old location
  ;; Dynamically bound some handy variables
  (let ((return-to nil)       ;;; to be set by make-header-hooks functions
	(header-prefix-string (header-prefix-string)) ;;; cache the result
	(comment-start-p
	 (and comment-start (not (string-equal comment-start ""))))
	(comment-end-p (and comment-end (not (string-equal comment-end "")))))

    ;; Do the header functions
    (mapcar 'funcall make-header-hooks)

    ;; Move to wherever return-to was set
    (if return-to (goto-char return-to))
    ))

(defun make-revision ()
  "Inserts a revision marker after the history line.  Makes the history
   line if it does not already exist."
  (interactive)
  (let ((header-prefix-string (header-prefix-string))
	(logical-comment-start
	 (if (= (length comment-start) 1)
	     (concat comment-start comment-start " ")
	   comment-start)))

    ;; Look for the History line
    (beginning-of-buffer)         ;;; leave a mark where we were
    (if (re-search-forward (concat "^\\("
				   (regexp-quote (header-prefix-string))
				   "\\|"
				   (if (and comment-start
					    (not (string-equal comment-start "")))
				       (concat
					"\\|" (regexp-quote comment-start))
				     "")
				   "\\)"
				   " *History")
			   header-max t)
	(progn (end-of-line))
      (progn
	;; We did not find a history line, add one
	(goto-char (point-min))
	;; find the first line that is not part of the header
	(while (and (< (point) header-max)
		    (looking-at
		     (concat "[ \t]*\\("
			     (regexp-quote (header-prefix-string))
			     (if (and comment-start
				      (not (string-equal comment-start "")))
				 (concat
				  "\\|" (regexp-quote comment-start))
			       "")
			     (if (and comment-end
				      (not (string-equal comment-end "")))
				 (concat "\\|" (regexp-quote comment-end))
			       "")
			     "\\)")))
	  (forward-line 1))
	(insert "\n" logical-comment-start
		"HISTORY ")
	(save-excursion (insert "\n" comment-end))))

    ;; We are now on the line with the history marker

    (insert "\n"
	    header-prefix-string
	    (current-d-m-y-string)
	    "\t\t"
	    (user-full-name-and-email)
	    ;;"\t|>Ident<|\n"
	    "\t\n"
	    header-prefix-string
	    "   "
	    )
    ;; Now, add details about the history of the file before its modification
    (if (save-excursion
	  (re-search-backward "Last Modified On[\t]*: \\(.+\\)$" nil t))
	(progn
	  (insert "Last Modified: " (buffer-substring (match-beginning 1)
						      (match-end 1)))
	  (if (save-excursion
		(re-search-backward "Update Count[ \t]*: \\([0-9]+\\)$" nil t))
	      (insert " #" (buffer-substring (match-beginning 1)
					     (match-end 1))))
;;	  (if (save-excursion
;;		(re-search-backward "Last Modified By[ \t]*: \\(.+\\)$" nil t))
;;	      (insert " (" (buffer-substring (match-beginning 1)
;;					     (match-end 1))
;;		      ")"))
	  (insert " (" (user-full-name) ")")
	  (insert "\n" header-prefix-string "   ")))
    ))

;;;###autoload
(defun make-divisor (&optional end-col)
  "A divisor line is the comment start, filler, and the comment end"
  (interactive)
  (let ((eol))
  (insert comment-start)
  (fill-line-with-preceding-character end-col)
  (save-excursion
    (insert comment-end)
    (setq eol (point)))
  (fixup-whitespace)
  (delete-char 1)			;delete the one space
  (goto-char eol)			;goto after the comment end
  (insert "\n")))


;;;###autoload
(defun make-box-comment (&optional end-col)
  "Inserts a box comment that is built using mode specific comment characters."
  (interactive)
  (if (not (= 0 (current-column))) (forward-line 1))
  (insert comment-start)
  (if (= 1 (length comment-start))
      (insert comment-start))
  (if (not (char-equal (preceding-char) ? )) (insert ? ))
  (insert (make-string (max 2
			    (- (or end-col fill-column ) (length
							       comment-end)
			       2 (current-column)))
                       (aref comment-start
			     (if (= 1 (length comment-start)) 0 1))
		       ))
  (insert "\n" (header-prefix-string) )
  (save-excursion
    (insert "\n" (header-prefix-string)
	    (make-string (max 2
			      (- (or end-col fill-column) (length
								 comment-end)
				 2 (current-column)))
			 (aref comment-start
			       (if (= 1 (length comment-start)) 0 1))
			 )
	    comment-end "\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun current-d-m-y-string ()
  (let ((str (current-time-string)))
    (concat (if (equal ?\  (aref str 8))
		       (substring str 9 10)
		       (substring str 8 10))
	    "-"
	    (substring str 4 7)
	    "-"
	    (substring str 20 24))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic Header update code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun update-file-header ()
  "If the file has been modified, searches the first header-max chars in the
   buffer using the regexps in file-header-update-alist. When a match is
   found, it applies the corresponding function with the point located just
   after the match.  The functions can use (match-beginning) and
   (match-end) calls to find out the strings that causes them to be invoked."
  (interactive)
  (if (and (> (buffer-size) 100) (buffer-modified-p) (not buffer-read-only))
      (save-excursion
	(save-restriction ;; only search the header-max number of characters
	  (narrow-to-region 1 (min header-max (- (buffer-size) 1)))
	  (let ((patterns file-header-update-alist))
	    ;; do not record this call as a command in the command history
	    (setq last-command nil)
	    (while patterns
	      (goto-char (point-min))
	      (if (re-search-forward (car (car patterns)) nil t)
		  (progn
		    ;; position the cursor at the end of the match
		    (goto-char (match-end 0))
		    ;;(message "do
		    (funcall (cdr (car patterns)))))

	      (setq patterns (cdr patterns))
	      )
	    )))))

;; Place the header update function as a write file action
(if (not (memq 'update-file-header write-file-hooks))
    (setq write-file-hooks (cons 'update-file-header write-file-hooks)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Now, define the individual file header actions.  These are the building
;; blocks of automatic header maintenance.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun delete-and-forget-line ()
  ;; does not place the line in the kill-ring
  (let* ((start (point))
	 (stop  (progn (end-of-line) (point)))
	 (str   (buffer-substring start stop))
	 )
  (delete-region start stop)
  str))

;;;###autoload
(defun update-write-count ()
  (let ((num)
	(str  (delete-and-forget-line)))
    (setq num (car (read-from-string str)))
    (if (not (numberp num))
	(progn
	  (insert str)
	  (error "invalid number for update count '%s'" str))
      (progn
	;;(message "New write count=%s" num)
	(insert (format "%s" (+ 1 num)))))
    ))

;;;###autoload
(defun update-last-modifier ()
  (delete-and-forget-line)
  (insert (format "%s" (user-full-name-and-email)))
  )
;;;###autoload
(defun update-last-modified-date ()
  (delete-and-forget-line)
  (insert (format "%s" (current-time-string)))
  )
;;;###autoload
(defun update-file-name ()
  (declare (special header-prefix-string))
  (beginning-of-line)
  ;; verify that we are looking at a file name for this mode
  (if (looking-at
       (concat (regexp-quote (header-prefix-string)) " *\\(.*\\) *\\-\\-"))
      (progn
	(goto-char (match-beginning 1))
	(delete-region (match-beginning 1) (match-end 1))
	(insert (file-name-nondirectory (buffer-file-name))" -")
	)))


;;(setq file-header-update-alist nil)
;;(setq file-header-update-alist (cdr file-header-update-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stand Alone Headers
;;
;; These functions give the ability to invoke headers from the command line.
;;   This if if yor site has non-productive vi users and you want them to
;;   be able to use headers as well.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun headerable-file-p (file)
  (not (if (not (file-exists-p file))
	   (message "\"%s\" does not exist!." file)
	 (if (file-symlink-p file)
	     (message "\"%s\" is a symbolic link." file)
	   (if (file-directory-p file)
	       (message "\"%s\" is a directory." file)
	     )))))

(defun uniqueify-list (list)
  (let ((rest list))
    (while rest
      (setcdr rest (delq (car rest) (cdr rest)))
      (setq rest (cdr rest)))
    list))

;;(headerable-file-p "AFS")
;;(headerable-file-p "dbiogen.el")
;;(headerable-file-p "dbiogen.elc")

(defvar header-required-mode nil
  "The mode we will force files to be in, regardless of file suffix.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define a touch-headers command. This depends upon Lynn Slater's
;; customizations to startup.el to allow command-line-hooks.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq command-line-hooks (cons 'touch-headers command-line-hooks))

;;;###autoload
(defun touch-headers ()
  (if (or (string= argi "-touch") (string= argi "-touch-headers"))
      (let ((trim-versions-without-asking t)
	    (executing-macro "true"));; suppress "Mark Set" messages
	;; Consume all following arguments until one starts with a "-"
	(while (and command-line-args-left
		    (not (char-equal ?- (aref (car command-line-args-left) 0))))
	  (if (headerable-file-p (car command-line-args-left))
	      (progn
		(find-file (car command-line-args-left))
		(make-revision)
		(write-file nil)
		(kill-buffer (current-buffer))))
	  (setq command-line-args-left (cdr command-line-args-left))
	  ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define a make-headers command line option.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq command-line-hooks (cons 'make-headers command-line-hooks))

;;;###autoload
(defun make-headers ()
  (if (or (string= argi "-make-headers") (string= argi "-make"))
      (let ((trim-versions-without-asking t)
	    (executing-macro "true"));; suppress "Mark Set" messages
	;; Consume all following arguments until one starts with a "-"
	(while (and command-line-args-left
		    (not (char-equal ?- (aref (car command-line-args-left) 0))))

	  (if (headerable-file-p (car command-line-args-left))
	      (progn
		(find-file (car command-line-args-left))
		(if header-required-mode
		    (funcall header-required-mode))
		(make-header)
		(write-file nil)
		(message "\tMode was %s" major-mode)
		(kill-buffer (current-buffer))))
	  (setq command-line-args-left (cdr command-line-args-left))
	  ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define a -default-mode command line option.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq command-line-hooks (cons 'set-header-mode command-line-hooks))

;;;###autoload
(defun set-header-mode ()
  (if (or (string= argi "-default-mode")
	  (string= argi "-default"))
      (let ((trim-versions-without-asking t)
	    (executing-macro "true");; suppress "Mark Set" messages
	    (mode (intern (car command-line-args-left))))
	(if (memq mode (mapcar 'cdr auto-mode-alist))
	    (progn
	      (setq default-major-mode mode)
	      (message "Default mode is %s" default-major-mode)
	      (setq command-line-args-left (cdr command-line-args-left)))
	  (message "Mode \"%s\" is invalid. Try one of %s" mode
		   (uniqueify-list (mapcar 'cdr auto-mode-alist)))
	  (kill-emacs 1))
	)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define a -required-mode command line option.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq command-line-hooks (cons 'set-header-required-mode command-line-hooks))

;;;###autoload
(defun set-header-required-mode ()
  (if (or (string= argi "-required-mode")
	  (string= argi "-mode"))
      (let ((trim-versions-without-asking t)
	    (executing-macro "true");; suppress "Mark Set" messages
	    (mode (intern (car command-line-args-left))))
	(if (memq mode (mapcar 'cdr auto-mode-alist))
	    (progn
	      (setq header-required-mode mode)
	      (message "Required mode is %s" header-required-mode)
	      (setq command-line-args-left (cdr command-line-args-left)))
	  (message "Mode \"%s\" is invalid. Try one of %s" mode
		   (uniqueify-list (mapcar 'cdr auto-mode-alist)))
	  (kill-emacs 1))
	)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Things in the works or still to do.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; effort.el -- allows an "effort" to be resgistered in the mode line much
;; like the mode is. The effort then determines some header characteristics
;; such as copyright.  Typical efforts would be 'gdb 'gcc, 'g++, 'emacs, etc.
;; This would let the copyright (and c-style) be adjusted even within
;; common modes.
;;
;; need ez access to values in the header
;; need a headerp fcn
;;
;; auto make-revision if current user is not same as last modifier
;;   this would give a history of who touched what.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This allows the table of contents to be saved
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun widen-and-run-function (function)
  "Widen the buffer before executing a function."
  (save-restriction
    (widen)
    (funcall function)))

;;;###autoload
(defun widen-update-toc ()
  "Widen buffer and run update-table-of-contents"
  (widen-and-run-function 'update-table-of-contents))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Register the automatic actions to take for file headers during a save
;; See the second part of the file for explinations.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(progn
  (register-file-header-action "[ \t]Update Count[ \t]*: "  'update-write-count)
  (register-file-header-action "[ \t]Last Modified By[ \t]*: "  'update-last-modifier)
  (register-file-header-action "[ \t]Last Modified On[ \t]*: "  'update-last-modified-date)
  (register-file-header-action "TABLE OF CONTENTS" 'widen-update-toc)
  (register-file-header-action "Update Tests:[ \t]*yes" 'widen-update-test-fields)
  ;;(register-file-header-action "^.* *\\(.*\\) *\\-\\-" 'update-file-name)
  )


(defun widen-update-test-fields ()
 "Widen buffer, then update test fields"
   (widen-and-run-function 'update-function-test-fields))

;;;###autoload
(defun copyright-notice-on ()
  "Set copyright notice to be included."
  (interactive)
  (setq include-copyright-notice 't))

;;;###autoload
(defun copyright-notice-off ()
  "Set copyright notice to not be included."
  (interactive)
  (setq include-copyright-notice nil))


