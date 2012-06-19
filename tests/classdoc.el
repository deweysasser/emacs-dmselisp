;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; classdoc.el -- test for class documentation
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser <dewey@newvision.com>
;; Created On      : Fri Oct 11 14:40:18 1996
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defTest class-test-series
;;   (defTest basic-class-documentation-test
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: classdoc.el,v $
;; $Revision: 1.4 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defTest class-test-series
  :test-type eltest-series
  :name "classdoc.el test series"
  :directory "classdoc"
  :test (:bindings ((user-full-name "Dewey M. Sasser")
		    (user-mail-address "dewey@newvision.com")))
  )

(defTest basic-class-documentation-test
  :parent class-test-series
  :name "basic class documentation test"
  :test-type eltest-buffer-modifying
  :test ((document-class)
	 (("classdoc.init"
	 "classdoc.result"))
	 :mode c++
	 :check-point 't
	 ))


(do-and-view-test 'class-test-series)
