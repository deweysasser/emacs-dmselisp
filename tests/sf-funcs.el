;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sf-funcs.el -- test for sf functions
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
;;   (defTest sf-fill-line-test
;;   (defTest sf-sf::document-object
;;   (defTest sf-sf::document-object-perl-tests
;;   (defTest sf-document-c-macro
;;   (defTest sf-redocumentation-test
;;   (defTest sf-whitespace-killing-test
;;   (defTest sf-update-of-synopsis
;;   (defTest sf-operator-documentation-tests
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: sf-funcs.el,v $
;; $Revision: 1.1 $
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


(defTest sf-fill-line-test
  :parent sf-function-series
  :name "Fill line test"
  :test-type eltest-buffer-modifying
  :test ((fill-line-with-preceding-character)
	 (("fill.init"
	 "fill.result"))
	 :mode c++))

(defTest sf-sf::document-object
  :parent sf-function-series
  :name "Test of sf::document-object"
  :test-type eltest-buffer-modifying
  :test ((sf::document-object)
	 (("function.init"
	   "function.result")
	  ("func2.init"
	   "func2.result"
	   :mode c)
	  ("func2.init"
	   "func2cpp.result"
	   ))
	 :mode c++
	 :trim-surrounding-whitespace 't
	 :check-point 't
	 ))

(defTest sf-sf::document-object-perl-tests
  :parent sf-function-series
  :name "Perl mode documentation tests"
  :test-type eltest-buffer-modifying
  :test ((sf::document-object)
	 (("perlfunc.init"
	  "perlfunc.result"))
	 :mode perl
	 :check-point 't))

(defTest sf-document-c-macro
  :parent sf-function-series
  :name "C Macro documentation tests"
  :test-type eltest-buffer-modifying
  :test ((sf::document-object)
	 (("cmacro.init"
	   "cmacro.result"
	   :mode c)
	  ("cmacro.init"
	   "cmacro.cpp.result"
	   :mode c++))
	 :check-point 't))


(defTest sf-redocumentation-test
  :parent sf-function-series
  :name "Redocumentation test"
  :test-type eltest-buffer-modifying
  :test ((sf::document-object)
	 (("redoc.init"
	   "redoc.result")
	  ("redoc2.init"
	   "redoc2.result")
	  ("redoc3.init"
	   "redoc3.result")
	  ("redoc4.init"
	   "redoc4.result")
	  ("redoc5.init"
	   "redoc5.result")
	  ("redoc6.init"		;should clean up extra
					;whitespace, but this test
					;tests for its presence.  
	   "redoc6.result")
	  ("redoc7.init"
	   "redoc7.result")
	  ("redoc8.init"
	   "redoc8.result"
	   :mode c)
	  )
	 :mode c++
	 :check-point 't
	 :trim-surrounding-whitespace 't))

(defTest sf-whitespace-killing-test
  :parent sf-function-series
  :name "Whitespace killing test"
  :test-type eltest-buffer-modifying
  :test (
	 (kill-all-comments)
	 (("kac.init"
	   "kac.result"
	   :mode c)
	  ("kac.init"
	   "kac.result")
	  ("kac2.init"
	   "kac2.result")
	  )
	 :mode c++))

(defTest sf-update-of-synopsis
  :parent sf-function-series
  :name "Updating of synopsis"
  :test-type eltest-buffer-modifying
  :test (
	 (update-function-synopsis)
	 (("updsyn1.cpp.init"
	   "updsyn.cpp.result")
	  ("updsyn.cpp.init"
	   "updsyn.cpp.result"))
	 :mode c++))

(defTest sf-operator-documentation-tests
  :parent sf-function-series
  :name "Operator documentation tests"
  :test-type eltest-buffer-modifying
  :test (
	 (sf::document-object)
	 (("op1.init"
	   "op1.result"))
	 :mode c++))
  

(do-and-view-test 'sf-function-series)

;(do-test 'sf-function-series)

(do-test 'sf-function-series 10)