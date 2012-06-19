;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; msvt.el -- Microsoft Visual Test mode
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser
;; Created On      : Wed Sep 11 12:20:51 1996
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defvar msvt-basic-indent 3
;;   (defvar msvt-syntax-end-indention nil
;;   (defvar msvt-syntax-indention nil
;;   (defvar msvt-keywords
;;   (defun msvt-define-end-indention (word val)
;;   (defun msvt-define-indention (word val)
;;   (define-derived-mode ms-visual-test-mode generic-code-mode "MS Visual Test"
;;   (define-key ms-visual-test-mode-map "\t" 'msvt-indent-command)
;;   (defun msvt-indent-command ()
;;   (defun msvt-get-appropriate-indention ()
;;   (defun msvt-get-end-indention-for-symbol (value)
;;   (defun msvt-get-indention-for (word this-word)
;;   (defun msvt-get-indention-for-symbol (value)
;;   (defun msvt-find-line-indention ()
;;   (defun remove-all-ws ()
;;   (defun msvt-check-if ()
;;   (defun msvt-word-at-point ()
;;   (defmodemethod forward-comment ms-visual-test-mode (arg)
;; 
;;   (defvar msvt-basic-indent 3
;;   (defvar msvt-syntax-end-indention nil
;;   (defvar msvt-syntax-indention nil
;;   (defvar msvt-keywords
;;   (defun msvt-define-end-indention (word val)
;;   (defun msvt-define-indention (word val)
;;   (define-derived-mode ms-visual-test-mode generic-code-mode "MS Visual Test"
;;   (define-key ms-visual-test-mode-map "\t" 'msvt-indent-command)
;;   (defun msvt-indent-command ()
;;   (defun msvt-get-appropriate-indention ()
;;   (defun msvt-get-end-indention-for-symbol (value)
;;   (defun msvt-get-indention-for (word this-word)
;;   (defun msvt-get-indention-for-symbol (value)
;;   (defun msvt-find-line-indention ()
;;   (defun remove-all-ws ()
;;   (defun msvt-check-if ()
;; 
;;  OPTIONS
;;    Update Tests:  yes
;;
;;  BUGS
;;    Does not recognize comments that begin with "^rem"
;; 
;; $RCSfile: msvt.el,v $
;; $Revision: 1.6 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'generic-code)
(require 'thingatpt)

;;;###autoload
(defvar msvt-basic-indent 3
  "*Basic level of indention used in MS Visual Test mode")

(defvar msvt-syntax-end-indention nil
  "*Indention to modify based on this line")

(defvar msvt-syntax-indention nil
  "*Indention entries for msvt indention.  Use msvt-syntax-indention to modify")


(defvar msvt-keywords
  '(
    "\\(\\s \\|^\\)do\\(\\s \\|$\\)"
    "\\(\\s \\|^\\)else\\(\\s \\|$\\)"
    "\\(\\s \\|^\\)end\\(\\s \\|$\\)"
    "\\(\\s \\|^\\)for\\(\\s \\|$\\)"
    "\\(\\s \\|^\\)function\\(\\s \\|$\\)"
    "\\(\\s \\|^\\)if\\(\\s \\|$\\)"
    "\\(\\s \\|^\\)loop\\(\\s \\|$\\)"
    "\\(\\s \\|^\\)scenario\\(\\s \\|$\\)"
    "\\(\\s \\|^\\)sub\\(\\s \\|$\\)"
    "\\(\\s \\|^\\)wend\\(\\s \\|$\\)"
    "\\(\\s \\|^\\)while\\(\\s \\|$\\)"
    )
  "List of key words for highlighting in ms-visual-test-mode.")

			

;;;###autoload
(defun msvt-define-end-indention (word val)
  (let ((cell (assoc word msvt-syntax-end-indention)))
    (if cell
	(setcdr cell val)
      (setq msvt-syntax-end-indention (cons (cons word val) msvt-syntax-end-indention)))))

;;;###autoload
(defun msvt-define-indention (word val)
  "Define the indention for WORD as VAL"
  (let ((cell (assoc word msvt-syntax-indention)))
    (if cell
	(setcdr cell val)
      (setq msvt-syntax-indention (cons (cons word val) msvt-syntax-indention)))))


;;;###autoload (autoload (quote ms-visual-test-mode) "msvt"     "Major mode for editing Microsoft Visual Test scripts." 't)
(define-derived-mode ms-visual-test-mode generic-code-mode "MS Visual Test"
    "Major mode for editing Microsoft Visual Test scripts.

\\{ms-visual-test-mode-map}"
\    (setq comment-start "'")
    (setq comment-end "")
    (modify-syntax-entry ?' "<")
    (modify-syntax-entry 10 ">")
    (modify-syntax-entry ?\\ "_")
    (setq comment-start-skip "\\(\\('\\|REM\\)[^\\$]\\)") ;; ^rem, not rem
    (make-local-variable 'tab-stop-list)
    (setq tab-stop-list '(2 4 6 8))
    (make-local-variable 'font-lock-defaults)
    (setq font-lock-defaults
	  '(msvt-keywords nil t nil nil
			  (font-lock-comment-start-regexp .  "'")
			  )))
	  

(define-key ms-visual-test-mode-map "\t" 'msvt-indent-command)

(defun msvt-indent-command ()
  "Indent the current line as appropriate"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((indention (msvt-get-appropriate-indention)))
      (if (not (looking-at (concat indention "\\S ")))
	  (progn
	    (remove-all-ws)
	    (insert indention))))))

(defun msvt-get-appropriate-indention ()
  "Get indention appropriate for this line"
  (let ((this-word (save-excursion
		     (beginning-of-line)
		     (skip-syntax-forward " ")
		     (msvt-word-at-point))))
    (save-excursion
      (forward-line -1)
      (while (or (looking-at (concat "[ \t]*" comment-start-skip))
		 (looking-at "^[ \t]*$"))
	(forward-line -1))
      (skip-syntax-forward " ")
      (let ((word (msvt-word-at-point)))
	(msvt-get-indention-for word this-word)))))

(defun msvt-get-end-indention-for-symbol (value)
  (let ((msvt-syntax-indention msvt-syntax-end-indention))
    (msvt-get-indention-for-symbol value)))

(defun msvt-get-indention-for (word this-word)
  "Return the appropriate indention for WORD, when THIS-WORD is the
first word of the current line"
  (let ((delta-amount 0)
	(this-word-cell (if this-word
			    (assoc (downcase this-word) msvt-syntax-end-indention)))
	(cell (if word
		  (assoc (downcase word) msvt-syntax-indention)))
	(amount (msvt-find-line-indention)))
    (if cell
	;; we have a match - use it
	(setq delta-amount (msvt-get-indention-for-symbol (cdr cell))))
    (if this-word-cell
	(setq delta-amount (msvt-get-end-indention-for-symbol (cdr
							   this-word-cell))))
    (make-string (+ amount delta-amount) ? )))

(defun msvt-get-indention-for-symbol (value)
  "get indention based on VALUE, given base amount AMOUNT"
  (cond
   ((numberp value)
    value)
   ((eq '+ value)
    msvt-basic-indent)
   ((eq '- value)
    (- msvt-basic-indent))
   ('t
     (msvt-get-indention-for-symbol (funcall value)))))
	
(defun msvt-find-line-indention ()
  "Find the indention of the current line"
  (beginning-of-line)
  (skip-syntax-forward " ")
  (current-column))
	    

(defun remove-all-ws ()
  "Remove all white space up to end of line"
  (while
      (equal (char-syntax (char-after (point)))
	     ? )
    (delete-char 1)))


(defun msvt-check-if ()
  "Return the appropriate indention for the next line by examining the
if statement on the current line."
  (save-excursion
    (let ((case-fold-search 't)
	  (end (save-excursion
		 (end-of-line)
		 (point))))
      (if (search-forward-regexp "then\\s *[^\\s \\s<]" end 't)
	  0
	'+))))


;;;; place this at end so that it can use previously defined functions
(if msvt-syntax-indention
    ()
  (msvt-define-indention "do" '+)
  (msvt-define-indention "if" 'msvt-check-if)  
  (msvt-define-indention "else" '+)
  (msvt-define-indention "for" '+)
  (msvt-define-indention "scenario" '+)  
  (msvt-define-indention "function" '+)  
  (msvt-define-indention "sub" '+)  
  (msvt-define-indention "while" '+))


(if msvt-syntax-end-indention
    ()
  (msvt-define-end-indention "wend" '-)
  (msvt-define-end-indention "loop" '-)
  (msvt-define-end-indention "end" '-))

(if (featurep 'fcreate)
    (fcreate-add-to-creation-alist 'ms-visual-test-mode 'make-header))


(defun msvt-word-at-point ()
  "Return a string which is (word-at-point), but without the
properties"
  (let ((str (word-at-point)))
    (remove-text-properties
     0
     (length str)
     (text-properties-at 0 str)
     str)
    str))
  

(defmodemethod forward-comment ms-visual-test-mode (arg)
  "Move forward across one comment, using value of comment-start skip"
  (if (looking-at comment-start-skip)
      (end-of-line)))