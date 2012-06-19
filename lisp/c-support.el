;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c-support.el -- Partial support for team C or C++ development
;; Portions Copyright (c) 1989, Lynn R. Slater Jr.
;; Portions Copyright (c) 1996, Dewey M. Sasser
;; Author          : Lynn Slater
;; Created On      : Tue Aug  8 12:26:17 1989
;; Last Modified By: Dewey M. Sasser <dsasser@cerulean.com>
;; Last Modified On: Tue May 30 21:32:57 2000
;; Update Count    : 244
;; Status          : Alpha released
;;
;;
;; TABLE OF CONTENTS
;;   (defvar command-line-hooks '())
;;   (defvar headere-tests-insert nil
;;   (defvar header-tests-list
;;   (defvar header-insert-arguments 't
;;   (defun header-prefix-sstring ()
;;   (defun delete-function-documentation (name)
;;   (defun have-function-documentation-p ()
;;   (defun next-c-function-name ()
;;   (defun delete-synopsis ()
;;   (defun update-function-synopsis ()
;;   (defun update-all-synopsis ()
;;   (defun insert-box (start end text)
;;   (defun insert-end (start end text)
;;   (defun update-table-of-contents ()
;;   (defmodemethod get-next-function-description latex-mode ()
;;   (defmodemethod get-next-function-description emacs-lisp-mode ()
;;   (defmodemethod get-next-function-description make-mode ()
;;   (defmodemethod get-next-function-description default ()
;;   (defmodemethod get-next-function-description ms-visual-test-mode ()
;;   (defun header-goto-table-of-contents ()
;;   (defun header-goto-purpose ()
;;   (defun header-goto-end ()
;;   (defun help-for-c-templates ()
;;   (defvar c-template-map nil
;;   (defun do-update-table-of-contents ()
;;   (defun do-update-all-synopsis ()
;;   (defvar afs-main-header-string "
;;   (defvar afs-module-header-string "
;;   (defun afs-main ()
;;   (defun afs-module ()
;;   (defun c++-to-c-comments-buffer ()
;;   (defun find-insert-return-values ()
;;   (defun find-all-return-statements ()
;;   (defun make-insert-return-values-function (prefix)
;;   (defun find-insert-exceptions ()
;;   (defun find-all-exceptions ()
;;   (defun insert-header-tests (header-prefix-string)
;;
;; HISTORY
;; PURPOSE--Changes
;;      The main changes I have made to this are in the
;;      document-c-function and related functions.  See the file
;;      Changelog for details
;; PURPOSE
;;    This file provides:
;; 1) Function header templates for C++ or C. C++ is better supported. 
;; 2) Fcn synopsys maintenance.
;; 3) Table of contents maintenacne (lisp amd make file also).
;; 4) Help-for help like support available.
;; 5) C++ to C comment conversion
;; 6) Command line batch maintanance operations
;; 7) Main and module insertation of ident strings.
;; 8) Highlight, selection, and replacement of "templates" much like some
;;    of the dumb sun editors.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Warning: this file is not yet ready for insertion into just any emacs.
;; Only someone with some experience and patience should try it as I have
;; not yet seperated out local site quirks or otherwise sanitized this file.
;; It is also not as customizable as I would like. That is why this is an
;; "alpha" release and not a "beta". Improvements are welcome.
;; Save as 'c-support and byte-compile.

;Elmakedoc:  Automatic Prototyping Destinations
;  functions: c-suppor.doc
;  variables: c-suppor.doc


(require 'backquote)

(defvar command-line-hooks '())

(provide 'c-support)
(require 'header)    ;; Package from lrs@indetech.com
;(require 'redoc)


(defvar headere-tests-insert nil
  "*Non-nil if should insert function header test checkboxes")

(defvar header-tests-list
  (list "Statement" "Branch" "Domain")
  "*List of test types to insert in header")

(defvar header-insert-arguments 't
  "*Non-nil if the function arguments should be listed in c-mode or
c++-mode function documentation.")



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function headers.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun header-prefix-sstring ()
  "Returns the header prefix string stripped of trailing blanks"
  (let* ((hps (header-prefix-string))
	 (match (string-match "[ \t]+$" hps)))
    (if match (substring hps 0  match) hps)))



;;;###autoload
(defun delete-function-documentation (name)
  "delete documentation header"
  (let ((start)
	(end))
  (save-excursion
    (goto-char name)
    (search-backward "/*")
    (setq start (point))
    (search-forward "*/")
    (setq end (point))
    (delete-region start end))))
	  
;;;###autoload
(defun have-function-documentation-p ()
  "Return 't if function documentation already exists"
  (save-excursion
    (let ((theFunc (find-c-function-by-parts)))
      (search-backward-regexp (concat (function-get-name theFunc) "\\(()\\)? -- ?") nil 't))))
    

;;;###autoload
(defun next-c-function-name ()
  "Returns the name of the next c or lisp function.
  Should only be called from before a function as it cannot reliable  tell
  function calls from function definitions and certain c constructs such as
  for and while loops." 
  (save-excursion
    (cond 
     ((eq major-mode 'emacs-lisp-mode)	;emacs-lisp-mode
      (if (re-search-forward "^(defun\\s-+\\(\\(\\s_\\|\\sw\\)+\\)\\s-+(" nil t)
	  (buffer-substring (match-beginning 1) (match-end 1))
	"")
      )					;emacs-lisp-mode end
     ((eq major-mode 'c++-mode)
       (if (re-search-forward "\\([a-zA-Z_][_a-zA-Z0-9]+::\\)\\(\\(\\sw\\|\\s_\\)+\\)[ \t]*(" nil t)
	   (buffer-substring (match-beginning 2) (match-end 2))
	 ""))
     ('t				; c-mode here
       (if (re-search-forward "\\(\\(\\sw\\|\\s_\\|::\\)+\\)[ \t]*(" nil t)
	   (buffer-substring (match-beginning 1) (match-end 1))
	 "")))))
     

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function synopsys extraction
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun delete-synopsis ()
  "Deletes current synopsis, places cursor at start of next line"
  (interactive)
  (let* ((header-prefix-pat (concat "^[ \t]*" (regexp-quote (header-prefix-string))))
	 (synopsis-pat (concat header-prefix-pat "SYNOPSIS")))
    (beginning-of-line)
    ;; Move back to synopsis line
    (while (and (not (looking-at synopsis-pat)) (looking-at header-prefix-pat) (forward-line -1)))
    (if (not (looking-at synopsis-pat))
	(progn
	  (forward-line 1)
	  ;; Move forward to synopsis line
	  (while (and (not (looking-at synopsis-pat)) (looking-at header-prefix-pat) (forward-line 1)))))
    (beginning-of-line)
    (if (looking-at synopsis-pat)
	(progn
	  ;; we have the start of a synopsis
	  (forward-line 1)
	  (delete-region (point)
			 (progn
			   (while (and (looking-at (concat header-prefix-pat
							   "[\n \t]"))
				       (forward-line 1)))
			   (point)
			   ))
	  t
	  ))))
;;;###autoload
(defun update-function-synopsis ()
  "Updates the synopsis in a function header.  Grabs the function
  declaration (up to the first bracket in c++ or the first blank in c),
  converts comments, and inserts it under the synopsis line.

  Removes the old synopsis."
  (interactive)
  (if (delete-synopsis)
      (let* ((header-prefix-sstring (header-prefix-sstring))
	     (prefix (buffer-substring
		      (point)
		      (progn (back-to-indentation)
			     (if (char-equal
				  (aref header-prefix-sstring 0) 32)
				 (backward-char 1))
			     (point))))
	     (here (progn (beginning-of-line) (point)))
	     (header-prefix-pat (concat "^[ \t]*"
					(regexp-quote header-prefix-sstring)))
	     fcn-decl
	     start)
	(beginning-of-line)
	;; skip to start of defn
	(while (and (looking-at header-prefix-pat) (forward-line 1)))
	(setq start (point))
	(if (re-search-forward "\\(\\sw\\|\\s_\\|::\\|<<\\)+[ \t]*(" nil t)
	    (progn
	      (goto-char (1- (match-end 0)))
	      (forward-sexp)   ;; Skip args. In C++, we are done
	      (if (eq major-mode 'c-mode)
		  (progn
		    ;; In C, we need to find the declarations.
		    ;; Heiristic: Skip to blank or a '{'
		    (re-search-forward "\\(^[ \t]*$\\|\\{\\)")
		    (goto-char (1- (match-beginning 0)))))
	      (setq fcn-decl (buffer-substring start (point)))
	      (goto-char here)
	      (insert fcn-decl "\n\n" )
	      (insert-box here (point)
			  (concat prefix header-prefix-sstring "   "))
	      )
	  (error "No function defn here!")))))
;;;###autoload
(defun update-all-synopsis ()
  "Updates the synopses of all function headers."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward (concat (regexp-quote (header-prefix-string)) "SYNOPSIS") nil t)
      (update-function-synopsis)
      (sit-for 0)
      )
    (message "All Function synopsis updated.")
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful things for all emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun insert-box (start end text)
  "Insert a text prefix at a column in all the lines in the region.
   Called from a program, takes three arguments, START, END, and TEXT.
   The column is taken from that of START.
   The rough inverse of this function is kill-rectangle."
  (interactive "r\nsText To Insert: ")
  (save-excursion
    (let (cc)
      ;; the point-marker stuff is needed to keep the edits from changing
      ;; where end is
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (setq cc  (current-column))
      (while (< (point) end) ;; modified 2/2/88
	;; I should here check for tab chars
	(insert text)
	(forward-line 1)
	(move-to-column cc)) ;; Alternate: use move-to-column if you must.
      (move-marker end nil))))

;;;###autoload
(defun insert-end (start end text)
  "Insert a text prefix at the end in all the lines in the region.
   Called from a program, takes three arguments, START, END, and TEXT.
   The column is taken from that of START."
  (interactive "r\nsText To Insert: ")
  (save-excursion
    (let (cc)
      ;; the point-marker stuff is needed to keep the edits from changing
      ;; where end is
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (end-of-line)	
      (while (< (point) end);; modified 2/2/88
	;; I should here check for tab chars
	(insert text)
	(forward-line 1)
	(end-of-line)	
	)
      (move-marker end nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Table of contents
;;   (defun update-table-of-contents ()
;;   (defun get-next-function-description ()
;;   (defun header-goto-table-of-contents ()
;;   (defun header-goto-purpose ()
;;   (defun header-goto-end ()
;;   (defun help-for-c-templates ()
;;   (defvar c-template-map nil
;;   (defun do-update-table-of-contents ()
;;   (defun do-update-all-synopsis ()
;;   (defvar afs-main-header-string "
;;   (defvar afs-module-header-string "
;;   (defun afs-main ()
;;   (defun afs-module ()
;;   (defun c++-to-c-comments-buffer ()
;; well as C and C++.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun update-table-of-contents ()
  "Updates the table of contents in the file header.
   Removes the old table of contents."
  ;; perhaps should use insert-and-inherit in this function to make
  ;; sure that inserted characters inherit the text properties (for
  ;; example, if they're going in to a comment).  This appears not to
  ;; be necessary when using font-lock.
  (interactive)
  (if (header-goto-table-of-contents)	;leave pt after "CONTENTS"
      (save-excursion
	(let ((insert-point (point))
	      (header-prefix-string (header-prefix-string))
	      str)
	  (while (setq str (get-next-function-description))
	    (save-excursion
	      (goto-char insert-point)
	      (insert "\n" header-prefix-string "  " str)
	      (setq insert-point (point))))
	  ;; Now, get rid of the old table of contents
	  (goto-char insert-point)
	  (insert "\n" header-prefix-string "  ")
	  (setq str (regexp-quote (buffer-substring (1+ insert-point) (point))))
	  (beginning-of-line)
	  (let ((start (point)))
	    (while (looking-at str)
	      (forward-line 1))
	    (delete-region start (point)))
	  (message "Table of contents updated.")
	  ))
    ;(message "There is no table of contents!")
    ))



(defmodemethod get-next-function-description latex-mode ()
    ;;All chapters, sections, subsections, subsubsections, etc.
  (if (re-search-forward "^\\\\\\(\\(sub\\)*\\)section{\\([^}]*\\)}" nil t)
	(concat
	 (cond
	  ((or (not (match-beginning 1)) (not (match-end 1)))
	   "")
	  ((string= "subsub" (buffer-substring (match-beginning 1) (match-end 1)))
	   "     ")
	  ((string= "sub" (buffer-substring (match-beginning 1) (match-end 1)))
	   "  ")
	  (t
	   ""))	  
	 (buffer-substring (match-beginning 3) (match-end 3)))))

(defmodemethod get-next-function-description emacs-lisp-mode ()
    ;; All defuns, defvars, defmacros go to table of contents
    (if (re-search-forward "^(def.*$" nil t)
	(buffer-substring (match-beginning 0) (match-end 0))))

(defmodemethod get-next-function-description make-mode ()
    ;; In make mode, a "function" is a target. However, the only ones we
    ;; notice are those with not-null actions.
    (if (re-search-forward "^\\([^# \t\=\n]* *:\\).*\n\t" nil t)
	(buffer-substring (match-beginning 1) (match-end 1))))

(defmodemethod get-next-function-description default ()
  ;; Presume C or C++. Only notice those fcns marked with a standard
  ;; fcn header. 
  (let ((qhps (regexp-quote (header-prefix-string))))
    (if (re-search-forward (concat
			    qhps
			    "\\(class[ \t]+[a-zA-Z_][a-zA-Z_0-9]*\\( -- .*\\)?$\\|"
			    "[a-zA-Z_][a-zA-Z_0-9]*\\(()\\)?\\([ \t]*\\)?--\\([ \t]*\\)?.*\\)[ \t]*$")
			   nil t)
	(buffer-substring (match-beginning 1 ) (match-end 1)))))

(defmodemethod get-next-function-description ms-visual-test-mode ()
  "Get the function descriptions for MS-Visual Test mode.  This return
either the scenario or the normal descriptions lines."
  (let ((info
	 (block 'get-next
	   (let (what
		 where)
	     (progn
	       (save-excursion
		 (setq what (call-next-mode-method))
		 (setq where (point)))
	       (save-excursion
		 (if (re-search-forward "^[ \t]*scenario[ \t]+\".*\"" nil 't)
		     (if (or (not what) (< (point) where))
			 ;; We're before the the one the other found
			 (return-from 'get-next (cons (point) (buffer-substring
							       (match-beginning
								0)
							       (match-end 0))))
		       (return-from 'get-next (cons where what)))
		   (return-from 'get-next (cons where what)))))))))
    (if (car info)
	(goto-char (car info)))
    (cdr info)))
      

;;;###autoload
(defun header-goto-table-of-contents ()
  "Moves to the table of contents in the header"
  (interactive)
  (let ((qhps (regexp-quote (header-prefix-string)))
	(here (point)))
    (beginning-of-buffer)
    (if (re-search-forward (concat qhps "TABLE OF CONTENTS") nil t)
	(point)
      (goto-char here)
      ;(beep)
      ;(message "There is no table of contents!")
      nil)))
;;;###autoload
(defun header-goto-purpose ()
  "Moves to the purpose in the header"
  (interactive)
  (let ((qhps (regexp-quote (header-prefix-string)))
	(here (point)))
    (beginning-of-buffer)
    (if (re-search-forward (concat qhps "PURPOSE") nil t)
	(progn
	  (skip-chars-forward "/ \t\n"
			      (save-excursion (forward-line 2) (1- (point))))
	  (point))
      (goto-char here)
      (beep)
      (message "There is no purpose in the header! :->")
      nil)))
;;;###autoload
(defun header-goto-end ()
  "Moves to the end of the header box"
  (interactive)
  (beginning-of-buffer)
  (forward-line 1)
  (let ((hps (regexp-quote (header-prefix-sstring))))
    (while (looking-at hps)
      (forward-line 1))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Easy to use help.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun help-for-c-templates ()
  "You have discovered 'C-ct', the C and C++ templates facility.  
   All templates start with this sequence. From there, type a templates option:

f, C-f  Document a C function
s, C-s  Update the synopsis of a C function
   C-h  Make a file header
r, C-r  Document a revision to the file
c       Goto table of contents
   C-c  Update the table of contents 
t, C-t  Move among and edit the |> <| constructs.
b, C-b  Make a box comment
d, C-d  Make a visual dividing line
e, C-e  Goto end of header
p, C-p  Goto purpose in header
m       Insert AFS c or c++ module header code line
C-m     Insert AFS c or c++ main   header code line
        (There are no special header code liens for include files.)

New Feature: \\[remake-header] -- Insert a new header, copy details from
             old header.  May still need hand cleanup afterwards.

Please use \\[describe-key] to find out more about any of these keys."
  (interactive)
  (let ((line-prompt
	 (substitute-command-keys (concat "f s t b d e p m C-m C-h C-r c C-c. Type ? for more help: "))))
    (message line-prompt)
    (let ((char (read-char)))
      (if (or (= char ??) (= char help-ch))
	  (save-window-excursion
	    (switch-to-buffer-other-window "*Help*")
	    (erase-buffer)
	    (insert (documentation 'help-for-c-templates))
	    (goto-char (point-min))
	    (while (memq char (cons help-ch '(?? ?\C-v ?\ ?\177 ?\M-v)))
	      (if (memq char '(?\C-v ?\ ))
		  (scroll-up))
	      (if (memq char '(?\177 ?\M-v))
		  (scroll-down))
	      (message "%s%s: "
		       line-prompt
		       (if (pos-visible-in-window-p (point-max))
			   "" " or Space to scroll"))
	      (let ((cursor-in-echo-area t))
		(setq char (read-char))))))
      (let ((defn (cdr (assq (downcase char) c-template-map))))
	(if defn (call-interactively defn) (ding))))))

(defvar c-template-map nil
  "Keymap used in c or c++ mode for smart template operations.")

(let ((c-mp (make-sparse-keymap)))
  (define-key c-mp "?"    'help-for-c-templates)
  (define-key c-mp "\C-h" 'help-for-c-templates)
;  (define-key c-mp help-character 'help-for-c-templates)
  (define-key c-mp "\C-f" 'document-c-function)
  (define-key c-mp "f"    'document-c-function)
  (define-key c-mp "s"    'update-function-synopsis)
  (define-key c-mp "\C-s" 'update-function-synopsis)
  (define-key c-mp "\C-h" 'make-header)
  (define-key c-mp "\C-r" 'make-revision)
  (define-key c-mp "r"    'make-revision)
  (define-key c-mp "\C-t" 'enter-template-mode)
  (define-key c-mp "t"    'enter-template-mode)
  (define-key c-mp "b"    'make-box-comment)
  (define-key c-mp "\C-b" 'make-box-comment)
  (define-key c-mp "d"    'make-divisor)
  (define-key c-mp "\C-d" 'make-divisor)
  (define-key c-mp "\C-c" 'update-table-of-contents)
  (define-key c-mp "c"    'header-goto-table-of-contents)
  (define-key c-mp "e"    'header-goto-end)
  (define-key c-mp "\C-e" 'header-goto-end)
  (define-key c-mp "p"    'header-goto-purpose)
  (define-key c-mp "\C-p" 'header-goto-purpose)
  (define-key c-mp "\C-m" 'afs-main)
  (define-key c-mp "m"    'afs-module)
  (setq c-template-map c-mp))

;; DMS (7/97): removed c-mode.  cc-mode is now the default.  TODO:
;; investigate need for this keymap 
;;(load-library "c-mode")
;(require 'c++-mode)
;(progn
;  (define-key c-mode-map "\C-ct" c-template-map)
;  (define-key c-mode-map "\C-c\C-t" c-template-map)
;  (define-key c++-mode-map "\C-ct" c-template-map)
;  (define-key c++-mode-map "\C-c\C-t" c-template-map)
;  )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I do not like the current behavior of / or { in c++ mode
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command line header maintenance for bi diehards or batch operations.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq command-line-hooks (cons 'do-update-table-of-contents command-line-hooks))

;;;###autoload
(defun do-update-table-of-contents ()
  (if (string= (upcase argi) "-TOC") 
      (let ((trim-versions-without-asking t)
	    (executing-macro "true"));; suppress "Mark Set" messages
	;; Consume all following arguments until one starts with a "-"
	(while (and command-line-args-left
		    (not (char-equal ?- (aref (car command-line-args-left) 0))))
     
	  (if (headerable-file-p (car command-line-args-left))
	      (progn
		(find-file (car command-line-args-left))
		(update-table-of-contents)
		(write-file nil)
		(kill-buffer (current-buffer))))
	  (setq command-line-args-left (cdr command-line-args-left))
	  ))))

(setq command-line-hooks (cons 'do-update-all-synopsis command-line-hooks))

;;;###autoload
(defun do-update-all-synopsis ()
  (if (string= (upcase argi) "-SYNOPSIS") 
      (let ((trim-versions-without-asking t)
	    (executing-macro "true"));; suppress "Mark Set" messages
	;; Consume all following arguments until one starts with a "-"
	(while (and command-line-args-left
		    (not (char-equal ?- (aref (car command-line-args-left) 0))))
     
	  (if (headerable-file-p (car command-line-args-left))
	      (progn
		(find-file (car command-line-args-left))
		(update-all-synopsis)
		(write-file nil)
		(kill-buffer (current-buffer))))
	  (setq command-line-args-left (cdr command-line-args-left))
	  ))))


;;(setq command-line-hooks (cons 'list-args command-line-hooks))
;;(defun list-args ()
;;  (if (string= (upcase argi) "-LIST")
;;      (message "%s" command-line-args-left)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; QA and corperate legal want stings such as the following in all
;; non-copylefted code. (Apologies to rms, we engineers keep as much as we
;; can under copyleft.)
;;
;; Note: this code is NOT copyright to ITI. This would only happen if you
;; distributed fiels edited with these functions and without first
;; customizing the strings to something more suitable (such as a copyleft).
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar afs-main-header-string "
static char iDeNt[] = \"@(#)$__Header$, Copyright 1989 ITI\";
static char *cOpYrIgHt[] =
        {
        \"Confidential and Proprietary to Independence\",
        \"Technologies, Unpublished and Copyrighted Work\"
        };
")

(defvar afs-module-header-string "
static char iDeNt[] = \"@(#)$__Header$, Copyright 1989 ITI\";
")
(defun afs-main ()
  "Inserts the AFS header for a main c or c++ program"
  (interactive)
  (header-goto-end)
  (insert afs-main-header-string))

(defun afs-module ()
  "Inserts the AFS header for a module (a .o file) in a  c or c++ program"
  (interactive)
  (header-goto-end)
  (insert afs-module-header-string))


;;;###autoload
(defun c++-to-c-comments-buffer ()
  "Convert C++ style comments to C-style in whole buffer. \n\
This only works in c-mode. "
  (interactive)
  (let ((pt (point)))
    (if (not (eq major-mode 'c-mode))
	(error "You must be in C mode for this command")
      (beginning-of-buffer)
      (while (search-forward "//" nil t)
	(delete-char -2)		;delete back to beginning of comments
	(insert comment-start)
	(end-of-line)
	(insert comment-end)
	)
      (goto-char pt))))

;(defun end-of-line (&optional arg)
;  "Move to End of Line"
;  (interactive "P")
;  (if arg
;      (forward-line (- arg 1)))
;  (if (re-search-forward "\r\\|\n" nil 't)
;      (backward-char 1)))

;;;###autoload
(defun find-insert-return-values ()
  "Find all return statements in following function, put the values into the
current buffer."
  (interactive)
  (let* (
	 (values)
	 (start-col (current-column))
	 (header-prefix-string))
    (setq header-prefix-string
	  (concat (buffer-substring (point)
				    (progn (indent-to start-col) (point)))
		  (header-prefix-sstring)
		  "          "))
    (setq values (find-all-return-statements))
    (mapcar (make-insert-return-values-function header-prefix-string) values)))


(defun find-all-return-statements ()
  "Find all return statements, return a list of them"
  (let ((returned-list)
	(comment nil)
	(here (point))
	(end-of-function (save-excursion (end-of-defun)
					 (point))))
    (save-excursion
      (let ((return-regexp
	     (concat "^[ \t]*return[ \t]*" ; the return statement
		     "\\([^;]*\\)"         ; everything up to the end of statement
		     "[ \t]*;[ \t]*"             ; all white space
		     "\\("
		     (regexp-quote comment-start)
		     "\\([A-Z][A-Z][A-Z]?(.*):\\)?" ;some dated initials?
		     "\\(.*\\)"		;the comment
		     (regexp-quote comment-end)
		     "\\)?"		;doesn't have to have the comment
		     "$"		;go to end of line
		     )))
      (while (search-forward-regexp
	      return-regexp
	      end-of-function 't)
	(if (not (match-beginning 4))
	    nil
	  (setq comment (buffer-substring (match-beginning 4) (match-end 4))))
	(setq returned-list (cons (cons (buffer-substring (match-beginning 1)
						    (match-end 1))
					comment)
				  returned-list)))
      returned-list))))

;;;###autoload
(defun make-insert-return-values-function (prefix)
  "Make a lambda expression that inserts it's arg, prefixed by PREFIX"
  ` (lambda (arg)
       (insert "\n" , prefix (car arg) " - "
	       (if (cdr arg)
		   (cdr arg)
		 ""))))

;;;###autoload
(defun find-insert-exceptions ()
  "Finds all exceptions thrown within the code and inserts there
documentation at the current point."
  (interactive)
  (let ((exceptions (find-all-exceptions)))
    ;; we get back a list where each element is (type value comment)
    (if exceptions
	(let ((header-prefix (header-prefix-string)))
	  (flet ((exception-type (e) (car e))
		 (exception-value (e) (cadr e))
		 (exception-comment (e) (caddr e))
		 (make-exception-doc (e)
				     (insert "\n" header-prefix "  "
					     (exception-type e)
					     (if (exception-value e)
						 (concat " ("
							 (exception-value e)
							 ")")
					       "")
					     " -- "
					     (if (exception-comment e)
						 (exception-comment e)
					       ""))))
	    (mapcar 'make-exception-doc
		    exceptions))))))

(defun find-all-exceptions ()
  "Return a list of all (exceptions values comments)"
  (flet ((get-match (i)
		    (if (match-beginning i)
			(buffer-substring
			 (match-beginning i)
			 (match-end i)))))
    (let ((end-of-function (save-excursion
			     (end-of-defun)
			     (point) ))
	  (comment-regexp
	   (concat "[ \t]*"
		   (regexp-quote comment-start)
		   "[ \t]*"
		   "\\(.*\\)?"
		   "[ \t]*"
		   (regexp-quote comment-end)
		   "[ \t]*"))
	  (return-list nil))
      (save-excursion
	(while (re-search-forward
		(concat
		 "^[ \t]*throw[ \t]+\\([a-zA-Z_0-9]+\\)" ;throw + type
		 "[ \t]*([ \t]*\\(.*\\)[ \t]*)[ \t ;]*" ;ws + ( args ) +  ws
;;;		 comment-regexp		;has a group in it
		 )
		end-of-function 't)
	  ;; We've found one
	  (setq return-list
		(cons
		 (list
		  (get-match 1)		;type
		  (get-match 2)		;value
		  (get-match 3)		;comment (maybe)
		  )
		 return-list)))
	(reverse return-list)))))


	   
;; This code was changed, once I found out about backquote
;;    (append '(lambda (arg))
;; 	   (list (list 'insert "\n" prefix (list 'car 'arg) " - "
;; 		       (list 'if '(cdr arg)
;; 			     (list 'cdr 'arg)
;; 			     "")))))
;;   
	
;;;###autoload
(defun insert-header-tests (header-prefix-string)
  "Put test coverage checkboxes into headers"
  (let ((theList header-tests-list))
    (if headere-tests-insert
	(progn
	  (insert header-prefix-string " TESTS AND COVERAGE\n" header-prefix-string "   ")
	  (while theList
	    (insert
	     (car theList) ": __  ")
	    (setq theList (cdr theList)))
	  (insert "\n")))))
   
    

    
