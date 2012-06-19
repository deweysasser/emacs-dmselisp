;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; doall.el -- 
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser
;; Created On      : Fri Mar 15 11:44:56 1996
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: doall.el,v $
;; $Revision: 1.7 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq srcdir "../lisp")

(if (not (featurep 'nvs-auto))
    (progn
      (setq load-path (cons (expand-file-name srcdir) load-path))
      (require 'nvs)
      (load-file (concat srcdir "/eltest.el"))))

(eltest::snarf-from-file '("os2patch.el"
			   "whitesp.el"
			   "block-cm.el"
			   "functions.el"
			   "classdoc.el"))


(do-and-view-failed-only 'Top)
