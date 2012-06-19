;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sf-objs.el -- super function objects
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser <dewey@newvision.com>
;; Created On      : Wed Feb 19 16:07:21 1997
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defcclass sf::program-object
;;   (defcclass (sf::function-base (:include sf::program-object))
;;   (defcclass (sf::function (:include sf::function-base))
;;   (defcclass (sf::macro (:include sf::program-object))
;;   (defcclass (sf::argument (:include sf::program-object))
;;   (defcclass (sf::array-argument (:include sf::argument))
;;   (defcclass (sf::class (:include sf::program-object))
;;   (defcclass (sf::member-function (:include sf::function-base))
;;   (defcclass (sf::member-variable (:include sf::argument))
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: sf-objs.el,v $
;; $Revision: 1.2 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cclass)

(defcclass sf::program-object
  name
  documentation
  )

(defcclass (sf::function-base (:include sf::program-object))
  "Structure which holds information about a function"
  class
  return-type
  args)

(defcclass (sf::function (:include sf::function-base))
  "Be an independent function")

(defcclass (sf::macro (:include sf::program-object))
  "C or C++ macro"
  args
  )

(defcclass (sf::argument (:include sf::program-object))
  "Structure which holds information about an argument"
  type
  default
  )

(defcclass (sf::array-argument (:include sf::argument))
  sizes)

(defcclass (sf::class (:include sf::program-object))
  superclasses				; a list of strings
  member-functions			; alist of ((type  elements)
					; (type elements)
  member-variables
  )

(defcclass (sf::member-function (:include sf::function-base))
  member-of
  visibility)

(defcclass (sf::member-variable (:include sf::argument))
  visibility)

(provide 'sf-objs)