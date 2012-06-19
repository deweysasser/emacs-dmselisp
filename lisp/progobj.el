;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; progobj.el -- Program Objects
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser <dewey@newvision.com>
;; Created On      : Wed Feb 19 15:14:17 1997
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defcclass program-object
;;   (defcclass (function (:include program-object))
;;   (defcclass argument
;;   (defcclass (array-argument (:include argument))
;;   (defcclass (class (:include program-object))
;;   (defcclass (member-function (:include function))
;;   (defcclass (member-variable (:include argument))
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: progobj.el,v $
;; $Revision: 1.2 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defcclass program-object
  name
  brief-documentation
  documentation
  options
  )

(defcclass (function (:include program-object))
  "Structure which holds information about a function"
  class
  return-type
  args)

(defcclass argument
  "Structure which holds information about an argument"
  type
  default
  )

(defcclass (array-argument (:include argument))
  sizes)

(defcclass (class (:include program-object))
  superclasses				; a list of strings
  member-functions			; alist of ((type  elements)
					; (type elements)
  member-variables
  )

(defcclass (member-function (:include function))
  member-of
  visibility)

(defcclass (member-variable (:include argument))
  visibility)

(provide 'progobj)