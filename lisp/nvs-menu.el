;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nvs-menu.el -- Put things into the menubar
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser
;; Created On      : Thu Mar 07 14:55:42 1996
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defvar nvs-menu-bar-map (make-sparse-keymap "NVS Global")
;;   (defvar nvs-menu-bar-operations (make-sparse-keymap "Operations")
;;   (defvar nvs-menu-bar-synopsis (make-sparse-keymap "Function Synopsis")
;;   (defvar nvs-menu-bar-navigation (make-sparse-keymap "Navigation")
;;   (define-key nvs-menu-bar-synopsis [update] '("Update Synopsis"
;;   (define-key nvs-menu-bar-synopsis [update-all] '("Update All Synopsis"
;;   (define-key nvs-menu-bar-operations [returns] '("Find and Insert Returns" .
;;   (define-key nvs-menu-bar-operations [exceptions] '("Find and Insert Throws" .
;;   (define-key nvs-menu-bar-operations [document-function] '("Document Function"
;;   (define-key nvs-menu-bar-operations [document-class] '("Document Class"
;;   (define-key nvs-menu-bar-navigation [toc-entry] '("Goto Function" .
;;   (define-key nvs-menu-bar-navigation [goto-toc] '("Goto Contents" .
;;   (define-key nvs-menu-bar-map [ops] (cons "Documentation"
;;   (define-key nvs-menu-bar-map [synopsis] (cons "Function Synopsis"
;;   (define-key nvs-menu-bar-map [navigation] (cons "Navigation"
;;   (define-key c-mode-map [menu-bar nvs] (cons "NVS" nvs-menu-bar-map))
;;   (define-key c++-mode-map [menu-bar nvs] (cons "NVS" nvs-menu-bar-map))
;;   (define-key emacs-lisp-mode-map [menu-bar nvs] (cons "NVS" nvs-menu-bar-map))
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: nvs-menu.el,v $
;; $Revision: 1.6 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar nvs-menu-bar-map (make-sparse-keymap "NVS Global")
  "The menu bar map")

(defvar nvs-menu-bar-operations (make-sparse-keymap "Operations")
  "Operations")

(defvar nvs-menu-bar-synopsis (make-sparse-keymap "Function Synopsis")
  "Synopsis")

(defvar nvs-menu-bar-navigation (make-sparse-keymap "Navigation")
  "Navigation")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                Synopsis                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key nvs-menu-bar-synopsis [update] '("Update Synopsis"
					     . update-function-synopsis))

(define-key nvs-menu-bar-synopsis [update-all] '("Update All Synopsis"
					     . update-all-synopsis))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                        Documentation Operations                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key nvs-menu-bar-operations [returns] '("Find and Insert Returns" .
						find-insert-return-values))
(define-key nvs-menu-bar-operations [exceptions] '("Find and Insert Throws" .
						   find-insert-exceptions))
(define-key nvs-menu-bar-operations [document-function] '("Document Function"
						   . document-c-function))
(define-key nvs-menu-bar-operations [document-class] '("Document Class"
						   . document-class))

						   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                               Navigation                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key nvs-menu-bar-navigation [toc-entry] '("Goto Function" .
					    find-toc-entry))

(define-key nvs-menu-bar-navigation [goto-toc] '("Goto Contents" .
					    header-goto-table-of-contents))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                          Main Menu Operations                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						   
(define-key nvs-menu-bar-map [ops] (cons "Documentation"
					 nvs-menu-bar-operations))
(define-key nvs-menu-bar-map [synopsis] (cons "Function Synopsis"
					      nvs-menu-bar-synopsis))
(define-key nvs-menu-bar-map [navigation] (cons "Navigation"
						nvs-menu-bar-navigation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   Installation in appropriate modes                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (string-lessp emacs-version "19.30")
    (load-library "cplus-md")
  (load-library "cc-mode"))
(define-key c-mode-map [menu-bar nvs] (cons "NVS" nvs-menu-bar-map))
(define-key c++-mode-map [menu-bar nvs] (cons "NVS" nvs-menu-bar-map))
(define-key emacs-lisp-mode-map [menu-bar nvs] (cons "NVS" nvs-menu-bar-map))
