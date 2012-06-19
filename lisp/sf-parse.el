;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sf-parse.el -- sf parser functions
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser <dewey@newvision.com>
;; Created On      : Wed Feb 19 19:30:41 1997
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defvar sf::object-creation-functions nil
;;   (defun sf::find-program-object ()
;;   (defun sf::find-c++-class ()
;;   (defun sf::find-c-macro ()
;;   (defmacro sf::with-clear-unread-token-buffer (&rest body)
;;   (defvar sf::unread-token-buffer nil
;;   (defun sf::read-c-token ()
;;   (defun sf::unread-token (token)
;;   (defun sf::peek-token ()
;;   (defun read-c-arg-list ()
;;   (defun read-c-argument ()
;;   (defun sf::comment-p (name)
;;   (defun read-argument-default (stack)
;;   (defun read-default-from-comment (comment)
;;   (defun tokenize-string (string)
;;   (defun sf::join (sep-string list)
;;   (defun get-next-word ()
;;   (defmodemethod sf::find-function c-mode ()
;;   (defmodealias sf::find-function c-mode
;;   (defun sf::read-c-function-line ()
;;   (defun sf::get-return-tokens (list)
;;   (defmodemethod sf::translate-return-type c-mode (type)
;;   (defmodealias sf::translate-return-type c-mode
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: sf-parse.el,v $
;; $Revision: 1.2 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sf-objs)
(require 'thingatpt)

(defvar sf::object-creation-functions nil
  "List of object creation functions.")

(if sf::object-creation-functions
    nil
  (push '("#define" (c-mode c++-mode) . sf::find-c-macro)
	sf::object-creation-functions)
  (push '("class" (c++-mode) . sf::find-c++-class)
	sf::object-creation-functions)
  (push '(t  nil . sf::find-function)	
	sf::object-creation-functions))

(defun sf::find-program-object ()
  "Return an object representing the program object"
  (let ((object
	 (let* ((start (point))
		(word (get-next-word))
		(func (assoc word sf::object-creation-functions)))
	   (if (not func)
	       (progn
		 (setq func (assoc 't sf::object-creation-functions))
		 (goto-char start)
		 (funcall (cddr func)))
	     (if (or (not (cadr func ))
		     (member major-mode (cadr func)))
		 (funcall (cddr func)))))))
    object))


(defun sf::find-c++-class ()
  "Find a c++ class"
  (error "Not yet implemented"))

(defun sf::find-c-macro ()
  "Return an object representing a c macro"
  (let* ((name (sf::read-c-token))
	(arglist (read-c-arg-list))
	(macro (make-instance 'sf::macro :name name)))
    (setf (sf::macro-args macro)
	  arglist)
    macro))

(defmacro sf::with-clear-unread-token-buffer (&rest body)
  `(let ((sf::unread-token-buffer nil))
     ,@body))

(defvar sf::unread-token-buffer nil
  "Place where one unread token is stored")
  
(defun sf::read-c-token ()
  "Read a c token"
  (if sf::unread-token-buffer
      (pop sf::unread-token-buffer)
    (sf::read (current-buffer) sf::c-tokenizer-table)))

(declaim (inline sf::read-c-token))

(defun sf::unread-token (token)
  "Unread a character"
  (push token sf::unread-token-buffer))

(defun sf::peek-token ()
  "Peek at a token"
  (let ((token (sf::read-c-token)))
    (sf::unread-token token)
    token))
	

(defun read-c-arg-list ()
  "Read a 'C' argument list"
  (let ((args)
	x)
  (if (equal (sf::read-c-token) "(")
      (while (not (equal (sf::peek-token) ")"))
	(setq x (read-c-argument))
	(push x args)
	(if (equal (sf::peek-token) ",")
	    (sf::read-c-token)))
    (sf::read-c-token))
  (reverse args)))

(defun read-c-argument ()
  "Read one C argument"
  (let (stack
	name
	default
	type
	next)
  (while (not (or
	       (equal (setq next (sf::read-c-token))
		   ",")
	       (equal next ")")))
    (push next stack))
  (sf::unread-token next)
  (setq default (read-argument-default stack))
  (setq stack
	(loop for x on stack
	      if (equal (first x) "=")
	      return (cdr x)
	      finally return stack))
  (setq stack
	(loop for name in stack
	      if (not (sf::comment-p name))
	      collect name))
  (setq name (pop stack))
  (setq type (sf::join " " (reverse stack)))
  (make-instance 'sf::argument :name name
		 :type type :default default)))

(defun sf::comment-p (name)
  "Return 't if name is a comment"
  (and (listp name)
       (eq (first name) 'comment)))

(defun read-argument-default (stack)
  "Read from STACK to find default value for argument"
  (let (default)
    (if (string= (nth 1 stack) "=")
	(prog1
	    (pop stack)
	  (pop stack))
      (if  (and
	    (setq default (read-default-from-comment (first
						      stack)))
	    default)
	  (progn
	    (pop stack)
	    default)))))

(defun read-default-from-comment (comment)
  "Read the default from a comment"
  (if (and (listp comment)
	   (eq (first comment) 'comment))
      (progn
	(let ((string (cadr comment)))
	  (if (string-match "^[ \t]*=" string)
	      (substring string (match-end 0)))))))

(defun tokenize-string (string)
  "Return a list of all tokens in string"
  (let (tokens token)
    (in-temp-buffer
     (sf::with-clear-unread-token-buffer
      (insert string)
      (beginning-of-buffer)
      (while (setq token(sf::read (current-buffer)
				  sf::c-tokenizer-table))
	(push token tokens))
      (reverse tokens)))))


(defun sf::join (sep-string list)
  "concatenate list, separating elements by sep-string"
  (let ((temp-list (list (first list))))
    (loop for x on (cdr list)
	      if (and (first x) (stringp (first x)))
	      do (progn (push sep-string temp-list)
			(push (first x) temp-list)))
    (apply 'concat (reverse temp-list))))




(defun get-next-word ()
  "Return the next word"
  (skip-chars-forward " \t\n")
  (let ((start (point)))
    (skip-chars-forward "^ \t\n")
    (buffer-substring-no-properties start (point))))

(defmodemethod sf::find-function c-mode ()
  "Find the function"
  (sf::read-c-function-line))

(defmodealias sf::find-function c-mode
  sf::find-function c++-mode)

(defun sf::read-c-function-line ()
  "Read the first line of a function"
  (sf::with-clear-unread-token-buffer
   (let (stuff
	 token
	 )
     (while (not (string= (setq token (sf::read-c-token)) "("))
       (push token stuff))
     (sf::unread-token token)
     ;; at this point, name is top of stack, '::' is next (if there is
     ;; a class) followed by class name, followed by return type
     ;; (reversed)
     (let* ((name (first stuff))
	    (class (if (string= "::" (nth 2 stuff))
		       (nth 3 stuff)))
	    (args (read-c-arg-list))
	    (func (if class
		      (sf::ensure-member-function class name)
		    (sf::ensure-function name))))
       (setf (sf::function-base-args func) args)
       (setf (sf::function-base-return-type func)
	     (sf::translate-return-type
	      (sf::join " " (sf::get-return-tokens stuff))))
       func))))


(defun sf::get-return-tokens (list)
  "Take a list, which is reversed tokens from before a function, and
return the list which is the normal order tokens for the return type"
  (if (string= "::" (nth 2 stuff))
      (reverse (cddr stuff))
    (reverse (cdr stuff))))

(defmodemethod sf::translate-return-type c-mode (type)
  "Translate the return type according to the current mode.  For
example, in c-mode, return type of \"void\" would return nil"
  (if (string= type "void")
      nil
    type))

(defmodealias sf::translate-return-type c-mode
  sf::translate-return-type c++-mode)