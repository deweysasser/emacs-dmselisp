;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; os2patch.el -- 
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser
;; Created On      : Fri Mar 15 15:59:44 1996
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defTest os2patch-test-series
;;   (defTest os2patch-test-series-fnd
;;   (defTest os2patch-test-series-fnnd
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: os2patch.el,v $
;; $Revision: 1.2 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defTest os2patch-test-series
  :name "Tests of OS/2 patches"
  :test-type eltest-series)

(defTest os2patch-test-series-fnd
  :name "file-name-directory test"
  :parent os2patch-test-series
  :test-type eltest-black-box
  :test (file-name-directory
	 (
	  (("foo/bar") "foo/")
	  (("foo/") "foo/")
	  (("foo") nil)	  	  
	  (("g:foo") "g:/")
	  (("g:/foo") "g:/")
	  (("g:/foo/bar") "g:/foo/")
	  (("g:/foo:bar/bar") "g:/foo:bar/")
	  (("g:/foo:bar") "g:/")	  	  
	  )))

(defTest os2patch-test-series-fnnd
  :name "file-name-nondirectory test"
  :parent os2patch-test-series
  :test-type eltest-black-box
  :test (file-name-nondirectory
	 (
	  (("foo/bar") "bar")
	  (("foo/") "")
	  (("foo") "foo")
	  (("g:foo") "foo")
	  (("g:/foo") "foo")
	  (("g:/foo/bar") "bar")
	  (("g:/foo:bar/bar") "bar")
	  (("g:/foo:bar") "foo:bar")	  	  
	  )))

(do-and-view-test 'os2patch-test-series)
  