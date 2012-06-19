;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; alists.el -- functions for dealing with alists
;; Author          : Dewey M. Sasser
;; Created On      : Mon May 02 16:08:58 1994
;; Last Modified By: Dewey M. Sasser <dewey@sasser.com>
;; Last Modified On: Tue Jun 19 13:05:59 2012
;; Update Count    : 52
;; Status          : $State: Exp $
;; Name            : $Name: HEAD $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defmacro append-to-list (path &rest values)
;;   (defmacro prepend-to-list (path &rest vals)
;;   (defmacro append-to-list-unique (path &rest values)
;;   (defmacro prepend-to-list-unique (path &rest values)
;;   (defun add-to-mode-alist (arg &optional arg2)
;;   (defun make-association (list key value)
;;   (defun get-association (list key)
;; 
;; $RCSfile: alists.el,v $
;; $Revision: 1.14 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Elmakedoc:  Automatic Prototyping Destinations
;  functions: alists.doc
;  variables: alists.doc

;;;###autoload (autoload (quote prepend-to-list) "alists" "Macro to prepend a new value onto a list" nil (quote macro))

;;;###autoload (autoload (quote append-to-list) "alists" "Macro to append a new value onto a list" nil (quote macro))

(require 'backquote)
(require 'cl)

(defmacro append-to-list (path &rest values)
  "*Append some values to a path"
  (if values
      ` (setq , path (append , path (list ,@ values)))
    (error "Appending nothing to path")))

(defmacro prepend-to-list (path &rest vals)
  "*Prepend some values to a path"
  (if vals
      ` (setq , path (append (mapcar 'eval ', vals) , path))
    (error "Prepending nothing to list")))


;;;###autoload
(defmacro append-to-list-unique (path &rest values)
  "Append VALUES to LIST, then make sure list (which should be an
alist), has unique keys, by removing any leading pairs for non-unique
keys"
  `(progn
    (append-to-list ,path ,@values)
    (remove-duplicates ,path :test #'(lambda (x y)
						(eq (car x) (car
							     y))))))

;;;###autoload
(defmacro prepend-to-list-unique (path &rest values)
  "Append VALUES to LIST, then make sure list (which should be an
alist), has unique keys, by removing any subsequent pairs for
non-unique keys"
  `(progn
    (prepend-to-list ,path ,@values)
    (remove-duplicates ,path :from-end 't :test #'(lambda (x y)
						    (eq (car x) (car y))))))

;;;###autoload
(defun add-to-mode-alist (arg &optional arg2)
  "Add something to an alist."
  (let ((pair (if (consp arg)
                  arg
                (cons arg arg2))))
    (prepend-to-list auto-mode-alist pair)
    (setq auto-mode-alist (remove-duplicates auto-mode-alist :from-end 't :test '(lambda (x y) (equal (car x) (car y)))))))

;; 
;; 
;; (defmacro make-association (list key value)
;;   (` (let ((match))
;;        (setq match (assoc  (, key) (, list)))
;;        (if match
;; 	   (setcdr match (, value))
;; 	 (setq (, list) (cons   (list  (cons (, key) (, value)) (, list) )
;; 			  (, list))))))
;; 
;; (defmacro get-association (list key)
;;   (` (let ((match))
;;        (setq match (assoc (, key) (, list)))
;;        (if match
;; 	   (cdr match)))))


;;;###autoload
(defun make-association (list key value)
  "Associates in LIST the KEY with VALUE.  It overwrites an old
association, if there is one."
  (let ((match))
    (setq match (assoc key list))
    (if match
	(progn
	  (setcdr match (list value))
	  list)
      (cons (cons key (list value)) list))))




;;;###autoload
(defun get-association (list key)
  (car (cdr (assoc key list))))

(declaim (inline get-association))

(provide 'alists)
