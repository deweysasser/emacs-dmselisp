;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; whitesp.el -- tests for whitespace functions
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser
;; Created On      : Fri Mar 08 13:45:56 1996
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defTest whitespace-test-series
;;   (defTest trim-leading-ws
;;   (defTest trim-trailing-ws
;;   (defTest more-trailing
;;   (defTest more-leading
;;   (defTest even-more-leading
;;   (defTest both-sides
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: whitesp.el,v $
;; $Revision: 1.5 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defTest whitespace-test-series
  :test-type eltest-series
  :name "Whitespace Tests")

(defTest trim-leading-ws
  :parent whitespace-test-series
  :name "Test of trim-leading-whitespace"
  :test-type eltest-black-box
  :test (trim-leading-whitespace
	 ((("foo") "foo")
	  (("   foo") "foo")
	  (("foo  ") "foo  ")
	  (("   foo   ")	    "foo   "))))

(defTest trim-trailing-ws
  :parent whitespace-test-series
  :test-type eltest-black-box
  :test (trim-trailing-whitespace 
	 ((("foo") "foo")
	  (("   foo") "   foo")
	  (("foo    ") "foo")
	  (("   foo   ")"   foo"))))


(defTest more-trailing
  :parent whitespace-test-series
  :test-type eltest-black-box
  :test (trailing-whitespace
	 ( ( ("foo") "")
	   (("   foo") "")
	   (("foo    ") "    ")
	   (("   foo   ") "   "))))

(defTest more-leading
  :parent whitespace-test-series
  :test-type eltest-black-box
  :test (leading-whitespace
	 ((("foo") "")
	  (("   foo") "   ")
	  (("foo    ") "")
	  (("   foo   ") "   "))))

(defTest even-more-leading
  :parent whitespace-test-series
  :test-type eltest-black-box
  :test (trim-leading-whitespace
	 (
	  (
	   (3)
	   "\"(wrong-type-argument sequencep 3)\""
	   :signal 't
	  ))))

(defTest both-sides
  :parent whitespace-test-series
  :test-type eltest-black-box
  :test (trim-whitespace
	 ((("\n") "")
	  (("foo\n") "foo")
	  (("  foo  ") "foo")
	  (("\n   foo   \n") "foo")
	  (("\n      \n") "")
	  (("      \n") "")
	  (("\n      ") "")
	  (("") "")
	  )))

(do-and-view-test 'whitespace-test-series)

