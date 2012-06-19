;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; doc.el -- support for documenting elisp code
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser <dewey@newvision.com>
;; Created On      : Tue Feb 18 17:31:30 1997
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defsubst caddddr (f)
;;   (defvar docsnarf::snarfers nil
;;   (defvar docsnarf::emitters nil
;;   (defmacro docsnarf::add-handler (what symbol function)
;;   (defun snarf-insert-things (file things)
;;   (defun snarf-insert-commands (file)
;;   (defun snarf-insert-variables (file)
;;   (defun snarf-insert-functions (file)
;;   (defun snarf-insert-macros (file)
;;   (defun docsnarf::insert (thing)
;;   (defun docsnarf::snarf (file)
;;   (defun docsnarf::snarf-thing (thing funcs)
;;   (defun docsnarf::snarf-defun (f)
;;   (defun docsnarf::snarf-defvar (var)
;;   (defun docsnarf::snarf-macro (m)
;;   (defun is-user-visible (docs)
;;   (defun docsnarf::emit-command (x)
;;   (defun docsnarf::emit-macro (x)
;;   (defun docsnarf::emit-function (x)
;;   (defun docsnarf::emit-def-thing (x thing)
;;   (defun docsnarf::emit-defvar (x)
;;   (defun texinfo-translate-string (string)
;;   (defun process-docs (docs)
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: doc.el,v $
;; $Revision: 1.2 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsubst caddddr (f)
  (car (cdr (cdr (cdr (cdr  f))))))

(defvar docsnarf::snarfers nil
  "List of (symbol . function) for variable snarfers")

(defvar docsnarf::emitters nil
  "List of (symbol . function) to call to create string to insert")

(defmacro docsnarf::add-handler (what symbol function)
  (case what
    ('emitter
     `(push (cons ',symbol (function ,function))
	    docsnarf::emitters))
    (otherwise
     `(push (cons ',symbol (function ,function))
	    docsnarf::snarfers))))

(if docsnarf::snarfers
    nil
  (docsnarf::add-handler function defun docsnarf::snarf-defun)
  (docsnarf::add-handler function defun* docsnarf::snarf-defun)
  (docsnarf::add-handler variable defvar docsnarf::snarf-defvar)
  (docsnarf::add-handler macro defmacro docsnarf::snarf-macro)
  (docsnarf::add-handler macro defmacro* docsnarf::snarf-macro))  

(if docsnarf::emitters
    nil
  (docsnarf::add-handler emitter command docsnarf::emit-command)
  (docsnarf::add-handler emitter defvar docsnarf::emit-defvar)
  (docsnarf::add-handler emitter useful-macro docsnarf::emit-macro)
  (docsnarf::add-handler emitter useful-function docsnarf::emit-function))

(defun snarf-insert-things (file things)
  "Snarf commands from FILE"
  (mapc '(lambda (x)
	   (if (member (first x) things)
	       (docsnarf::insert x)))
	(docsnarf::snarf file)))

(defun snarf-insert-commands (file)
  "Snarf commands from FILE"
  (interactive "f")
  (snarf-insert-things file '(command)))

(defun snarf-insert-variables (file)
  "Snarf commands from FILE"
  (interactive "f")
  (snarf-insert-things file '(defvar)))

(defun snarf-insert-functions (file)
  "Snarf commands from FILE"
  (interactive "f")
  (snarf-insert-things file '(useful-function)))

(defun snarf-insert-macros (file)
  "Snarf commands from FILE"
  (interactive "f")
  (snarf-insert-things file '(useful-macro)))



(defun docsnarf::insert (thing)
  "Insert things into the current buffer"
  (let ((func (assoc (first x) docsnarf::emitters)))
    (if func
	(insert (funcall (cdr func) x))
      (error "No emitter for %s" (first x)))))


(defun docsnarf::snarf (file)
  "Snarf all things out of a file using snarf-funcs"
  (let (functions
	doc-things)
    (in-temp-buffer
     (insert-file-contents file)
     (beginning-of-buffer)
     (let (temp
	   (standard-input (current-buffer)))
       (loop for x = (condition-case e (read) (error nil))
	     if (not x)
	     return functions
	     if (and
		 (setq temp (docsnarf::snarf-thing x docsnarf::snarfers))
		 temp)
	     do (push temp doc-things))))
    doc-things))

(defun docsnarf::snarf-thing (thing funcs)
  "Find a func that can snarf thing, snarf it and return it"
  (let ((func (assoc (first thing) funcs)))
    (if func
	(funcall (cdr func) thing))))


(defun docsnarf::snarf-defun (f)
  "Snarf a function"
  (let ((name (cadr f))
	(args (caddr f))
	(docs (cadddr f))
	(interactive (caddddr f)))
    (if (stringp docs)
	nil
      (setq docs nil))
    (if (and (listp interactive)
	     (eq (car interactive) 'interactive))
	(list 'command name args docs interactive)
    (if (is-user-visible docs)
	  (list 'useful-function name args docs interactive)))))

(defun docsnarf::snarf-defvar (var)
  (let ((name (cadr var))
	(init (caddr var))
	(docs (cadddr var)))
    (if (stringp docs)
	nil
      (setq docs nil))
    (if (is-user-visible docs)
	(list 'defvar name docs init))))

(defun docsnarf::snarf-macro (m)
  "Snarf a macro"
  (let ((name (cadr m))
	  (args (caddr m))
	  (docs (cadddr m)))
    (if (stringp docs)
	nil
      (setq docs nil))
    (if (is-user-visible docs)
	  (list 'useful-macro name args docs nil))))

(defun is-user-visible (docs)
  (if docs
      (string-match "^\\*" docs)))

(defun docsnarf::emit-command (x)
  (docsnarf::emit-def-thing x "Command"))

(defun docsnarf::emit-macro (x)
  (docsnarf::emit-def-thing x "Macro"))

(defun docsnarf::emit-function (x)
  (docsnarf::emit-def-thing x "Function"))

(defun docsnarf::emit-def-thing (x thing)
  "Turn a command into a string"
  (let ((name (nth 1 x))
	(args (nth 2 x))
	(docs (nth 3 x))
	(interactive (nth 4 x))
	strings
	)
    (setq strings (reverse
		   (list "@deffn " thing " " (symbol-name name) " ")))
    (loop for y in
	  (mapcar '(lambda (x) (concat (symbol-name x) " "))
		  args)
	  do (push y strings))
    (push "\n" strings)
    (push (texinfo-translate-string (process-docs docs)) strings)
    (push "\n@end deffn\n\n" strings)    
    (apply 'concat (reverse strings))))

(defun docsnarf::emit-defvar (x)
  (let ((name (nth 1 x))
	(docs (nth 2 x))
	(init (nth 3 x)))
    (concat "@defvar " (symbol-name name) "\n"
	    (concat
	     (texinfo-translate-string (process-docs docs))
	     "\n\nThe default is "
	     (format "%s" init)
	     "\n@end defvar\n\n"))))
	
(defun texinfo-translate-string (string)
  "Translate a string to texinfo syntax"
  (in-temp-buffer
   (insert string)
   (beginning-of-buffer)
   (while (re-search-forward "\\([{}@]\\)" nil 't)
     (replace-match "@\\1"))
   (buffer-substring (point-min) (point-max))))

(defun process-docs (docs)
  (if docs
      (if (eql (aref docs 0) ?*)
	  (substring docs 1)
	docs)
    ""))