;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions.el -- test for functions.el
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser
;; Created On      : Tue Mar 12 11:24:08 1996
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defTest functions-test-series
;;   (defTest fill-line-test
;;   (defTest document-c-function
;;   (defTest document-function-perl-tests
;;   (defTest document-c-macro
;;   (defTest redocumentation-test
;;   (defTest whitespace-killing-test
;;   (defTest update-of-synopsis
;;   (defTest operator-documentation-tests
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: functions.el,v $
;; $Revision: 1.8 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defTest functions-test-series
  :test-type eltest-series
  :name "Functions.el test series"
  :directory "functions"
  :test (:bindings ((user-full-name "Dewey M. Sasser")
		    (user-mail-address "dewey@newvision.com")))
  )

(defTest fill-line-test
  :parent functions-test-series
  :name "Fill line test"
  :test-type eltest-buffer-modifying
  :test ((fill-line-with-preceding-character)
	 (("fill.init"
	 "fill.result"))
	 :mode c++))

(defTest document-c-function
  :parent functions-test-series
  :name "Test of document-c-function"
  :test-type eltest-buffer-modifying
  :test ((document-function nil)
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

(defTest document-function-perl-tests
  :parent functions-test-series
  :name "Perl mode documentation tests"
  :test-type eltest-buffer-modifying
  :test ((document-function nil)
	 (("perlfunc.init"
	  "perlfunc.result"))
	 :mode perl
	 :check-point 't))

(defTest document-c-macro
  :parent functions-test-series
  :name "C Macro documentation tests"
  :test-type eltest-buffer-modifying
  :test ((document-function nil)
	 (("cmacro.init"
	   "cmacro.result"
	   :mode c)
	  ("cmacro.init"
	   "cmacro.cpp.result"
	   :mode c++))
	 :check-point 't))


(defTest redocumentation-test
  :parent functions-test-series
  :name "Redocumentation test"
  :test-type eltest-buffer-modifying
  :test ((document-function nil)
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

(defTest whitespace-killing-test
  :parent functions-test-series
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

(defTest update-of-synopsis
  :parent functions-test-series
  :name "Updating of synopsis"
  :test-type eltest-buffer-modifying
  :test (
	 (update-function-synopsis)
	 (("updsyn1.cpp.init"
	   "updsyn.cpp.result")
	  ("updsyn.cpp.init"
	   "updsyn.cpp.result"))
	 :mode c++))

(defTest operator-documentation-tests
  :parent functions-test-series
  :name "Operator documentation tests"
  :test-type eltest-buffer-modifying
  :test (
	 (document-function nil)
	 (("op1.init"
	   "op1.result"))
	 :mode c++))
  

(do-and-view-test 'functions-test-series)

