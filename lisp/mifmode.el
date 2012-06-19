;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mifmode.el -- a mode for editing MIF files
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser <dewey@newvision.com>
;; Created On      : Tue Oct 29 16:41:14 1996
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (define-derived-mode mif-mode generic-code-mode "MIF Mode"
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: mifmode.el,v $
;; $Revision: 1.2 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'generic-code)


;;;###autoload (autoload (quote mif-mode) "mifmode"     "Major mode for editing FrameMaker MIF files." 't)
(define-derived-mode mif-mode generic-code-mode "MIF Mode"
  "A mode for editing FrameMaker MIF files"
  (setq font-lock-defaults
	'(nil nil t nil nil
	      (font-lock-comment-start-regexp .  "#")))
			
  (modify-syntax-entry ?< "(>")
  (modify-syntax-entry ?> ")<")
  (modify-syntax-entry ?\n ">")
  (modify-syntax-entry ?\" ".")    
  (modify-syntax-entry ?\\ ".")    
  (modify-syntax-entry ?\# "<"))