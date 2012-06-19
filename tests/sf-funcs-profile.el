;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sf-funcs.el -- tests for profiling sf-*.el
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser <dewey@newvision.com>
;; Created On      : Wed Feb 26 22:23:12 1997
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defTest sf-function-series
;;   (defTest sf-function-document-long
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: sf-funcs-profile.el,v $
;; $Revision: 1.2 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defTest sf-function-series
  :test-type eltest-series
  :name "SuperFunc test series"
  :directory "sf")

(defTest sf-function-document-long
  :parent sf-function-series
  :name "Document a long function"
  :test-type eltest-buffer-modifying
  :test ((sf::document-object)
	 (
	  ("longdoc.init"
	   "longdoc.result")
	  )
	 :mode c))


(do-test 'sf-function-series 10)
