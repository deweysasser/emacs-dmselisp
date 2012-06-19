;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sf-token.el -- tokenizers
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser <dewey@newvision.com>
;; Created On      : Fri Feb 21 10:29:50 1997
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defun* sf::copy-readtable (&optional (read-table nil read-table-passed))
;;   (defun sf::make-sf::readtable ()
;;   (defvar *sf::readtable* (sf::copy-readtable)
;;   (defvar sf::c-tokenizer-table (sf::copy-readtable nil)
;;   (defun sf::read-char (&optional stream)
;;   (defun sf::unread-char (stream char)
;;   (defun sf::peek-char (stream)
;;   (defun sf::lookup-syntax (char read-table)
;;   (defun sf::read-arith-accum-op (stream op)
;;   (defun sf::read-equals-op (stream op)
;;   (defun sf::read-slash (stream op)
;;   (defun* sf::read (stream &optional read-table)
;;   (defun sf::syntax-function-p (syntax)
;;   (defun sf::terminating-p (syntax)
;;   (defun sf::read-string (stream char)
;;   (defun sf::complete-read-comment (stream end-of-comment)
;;   (defun sf::set-char-syntax (char syntax &optional read-table)
;;   (defun sf::set-macro-character (char function &optional
;;   (defun sf::read-colon-character (stream char)
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: sf-token.el,v $
;; $Revision: 1.2 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun* sf::copy-readtable (&optional (read-table nil read-table-passed))
  "Copy READ-TABLE.  If read-table is nil, make a standard read-table"
  (let (new)
    (if read-table
	(copy-sequence read-table)
      (if read-table-passed
	  (flet ((make-syntax (syntax list)
			      (mapcar #'(lambda (x)
					  (setf (aref new x) syntax))
				      list)))
	    (setq new (sf::make-sf::readtable))
	    (loop for i from ?a to ?z
		  do (setf (aref new i) 'constituant))
	    (loop for i from ?A to ?Z
		  do (setf (aref new i) 'constituant))
	    (loop for i from ?0 to ?9
		  do (setf (aref new i) 'constituant))	    
	    (make-syntax 'whitespace '(? ?\t ?\n))
	    new)
	(sf::make-sf::readtable)))))

(defun sf::make-sf::readtable ()
  "Make a sf::readtable"
  (let ((table (make-vector 256 'illegal)))
    (setf (aref table 255) 'sf::readtable)
    table))
      
(defvar *sf::readtable* (sf::copy-readtable)
  "The current readtable")
    

(defvar sf::c-tokenizer-table (sf::copy-readtable nil)
  "readtable used for tokenizing C code")



(defun sf::read-char (&optional stream)
  "Read a character, advancing point"
  (in-buffer stream
	     (if (eobp)
		 nil
	       (prog1
		   (char-after (point))
		 (forward-char 1)))))

(defun sf::unread-char (stream char)
  "Unread character"
  (in-buffer stream
	     (forward-char -1)))

(defun sf::peek-char (stream)
  "Peek at a character"
  (in-buffer stream
	     (char-after (1+ (point)))))

(defun sf::lookup-syntax (char read-table)
  "Lookup char syntax in the current read table"
  (if char
      (aref read-table char)))

(declaim (inline sf::read-char sf::unread-char sf::peek-char sf::lookup-syntax))

(defun sf::read-arith-accum-op (stream op)
  "Read a token that may be an accumulator"
  (unless (bufferp stream)
    (error "Stream is not a buffer:  %S" stream))
  (if (eql (sf::read-char stream) ?=)
      (concat (list op) "=")
    (sf::unread-char stream nil)
    (make-string 1 op)))

(defun sf::read-equals-op (stream op)
  "Read an = or =="
  (unless (bufferp stream)
    (error "Stream is not a buffer:  %S" stream))
  (if (eql (sf::read-char stream) ?=)
      "=="
    (sf::unread-char stream nil)
    "="))

(defun sf::read-slash (stream op)
  "Read a / "
  (unless (bufferp stream)
    (error "Stream is not a buffer:  %S" stream))
  (case (sf::read-char stream)
    (?=
     "/=")
    (?*
     (sf::complete-read-comment stream "*/"))
    (otherwise
     (sf::unread-char stream nil)
     "/")))

(defun* sf::read (stream &optional read-table)
  "Read a token from stream using *sf::readtable* as default read table"
  (let ((state 'initial)
	token
	char
	syntax
	(read-table (or read-table *sf::readtable*))
	)
    (while (not (eq state 'finished))
      (setq char (sf::read-char stream))
      (setq syntax (sf::lookup-syntax char read-table))
      (cond
       ((not char)
	(if token
	    (setq state 'finished)
	  (return-from sf::read nil)))
       ((eq syntax 'whitespace)
	(if token
	    (setq state 'finished)))
       ((eq syntax 'constituant)
	(push char token)
	)
       ((eq syntax 'illegal)
	(error "character %c has illegal syntax" char))
       ((eq syntax 'single-char-operator)
	(if token
	    (progn
	      (sf::unread-char stream char))
	  (push char token))
	(setq state 'finished))
       ((sf::syntax-function-p syntax)
	(if token
	    (progn
	      (sf::unread-char stream char)
	      (setq state 'finished))
	  (let ((contribution (funcall (cdr syntax) stream char)))
	    (if contribution
		(return-from sf::read contribution)))))
       ('t
	(error "Illegal syntax in syntax table for %c" chr))))
    (apply 'concat (mapcar #'(lambda (x)
			       (if (stringp x)
				   x
				 (char-to-string x)))
			   (reverse token)))))



(defun sf::syntax-function-p (syntax)
  "Test if syntax is that for a function"
  (consp syntax))

(defun sf::terminating-p (syntax)
  "Test if the macro terminates the token"
  (not (car syntax)))

(defun sf::read-string (stream char)
  "Read a stream that terminates with char"
  (let ((end
	 (re-search-forward (concat "[^\\\\]" (char-to-string
					       char)))))
    (prog1
	(list 'string
	      (buffer-substring-no-properties (point) (1- end)))
      (goto-char end))))

(defun sf::complete-read-comment (stream end-of-comment)
  "Complete the reading of a comment"
  (in-buffer stream
	     (let ((end (save-excursion
			  (search-forward end-of-comment))))
	       (prog1
		   (list 'comment (buffer-substring-no-properties
				   (point)
				   (- end (length end-of-comment))))
		 (goto-char end)))))

(defun sf::set-char-syntax (char syntax &optional read-table)
  "Set syntax for char in (optional) READ-TABLE.  If READ-TABLE is nil
it defaults to value of *sf::readtable*"
  (setf (aref (or read-table *sf::readtable*)
	      char)
	syntax))

(defun sf::set-macro-character (char function &optional
				     non-terminating-p read-table)
  "Set the char CHAR to be a macro character in READ-TABLE"
  (setf (aref (or read-table *sf::readtable*)
	      char)
	(cons non-terminating-p function)))

(defun sf::read-colon-character (stream char)
  "Read : or ::"
  (if (eql (sf::read-char stream) ?:)
      "::"
    (sf::unread-char stream ?:)
    ":"))



(sf::set-macro-character ?/ #'sf::read-slash nil sf::c-tokenizer-table)
(sf::set-macro-character ?+ #'sf::read-arith-accum-op nil sf::c-tokenizer-table)
(sf::set-macro-character ?- #'sf::read-arith-accum-op nil sf::c-tokenizer-table)
(sf::set-macro-character ?* #'sf::read-arith-accum-op nil sf::c-tokenizer-table)
(sf::set-macro-character ?% #'sf::read-arith-accum-op nil sf::c-tokenizer-table)
(sf::set-macro-character ?= #'sf::read-equals-op nil
			 sf::c-tokenizer-table)
(sf::set-macro-character ?: #'sf::read-colon-character nil sf::c-tokenizer-table)
(sf::set-char-syntax ?\; 'single-char-operator sf::c-tokenizer-table)
(sf::set-char-syntax ?\, 'single-char-operator sf::c-tokenizer-table)
(sf::set-char-syntax ?\( 'single-char-operator sf::c-tokenizer-table)
(sf::set-char-syntax ?\) 'single-char-operator sf::c-tokenizer-table)
(sf::set-char-syntax ?_ 'constituant sf::c-tokenizer-table)


