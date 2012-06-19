;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rmailssum.el -- rmail super summary mode
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser
;; Created On      : Fri Mar 01 16:50:44 1996
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defvar ssum::data-file-name "~/.ssum")
;;   (defun ssum::ensure-buffer ()
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: rmailssum.el,v $
;; $Revision: 1.2 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ssum::data-file-name "~/.ssum")



(defun ssum::ensure-buffer ()
  "Ensure that the super sum buffer is present and current"
  
  