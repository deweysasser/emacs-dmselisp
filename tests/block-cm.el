;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; block-cm.el -- tests for block-cm.el functions
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser
;; Created On      : Tue Mar 12 20:22:31 1996
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defTest block-cm-tests
;;   (defTest block-comment
;;   (defTest bock-comment-line
;;   (defTest block-comment-line-2x
;;   (defTest block-comment-line-3x
;;   (defTest block-comment-ling
;;   (defTest block-comment-long-2x
;;   (defTest block-comment-long-3x
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: block-cm.el,v $
;; $Revision: 1.3 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defTest block-cm-tests
  :test-type eltest-series
  :name "block comment tests"
  :directory "block-cm")

(defTest block-comment
  :parent block-cm-tests
  :name "Block Comment"
  :test-type eltest-buffer-modifying
  :test ((call-interactively 'block-comment-region)
	 (("b1.init"
	   "b1.result")
	  ("b1.init"
	   "b2.result"
	   :bindings ((fill-column 10)))
	  ("b1.init"
	   "b3.result"
	   :bindings ((fill-column 100))))
	 :mode c
	 :bindings
	 ((fill-column 70))
	 ))

(defTest bock-comment-line
  :parent block-cm-tests
  :name "Block comment line"
  :test-type eltest-buffer-modifying
  :test ((block-comment-line 1)
	 (("l1.init"
	   "l1.result")
	  ("l1.init"
	   "l1c++.result"
	   :mode c++)
	  )
	 :mode c
	 :bindings
	 ((fill-column 70))))

(defTest block-comment-line-2x
  :parent block-cm-tests
  :name "Block comment line 2x"
  :test-type eltest-buffer-modifying
  :test ((block-comment-line 2)
	 (("l1.init"
	   "l2.result")
	  ("l1.init"
	   "l2c++.result"
	   :mode c++)
	  )
	 :mode c
	 :bindings
	 ((fill-column 70))))


(defTest block-comment-line-3x
  :parent block-cm-tests
  :name "Block comment line 3x"
  :test-type eltest-buffer-modifying
  :test ((block-comment-line 3)
	 (("l1.init"
	   "l3.result")
	  ("l1.init"
	   "l3c++.result"
	   :mode c++)
	  )
	 :mode c
	 :bindings
	 ((fill-column 70))))

(defTest block-comment-ling
  :parent block-cm-tests
  :name "Block comment long line"
  :test-type eltest-buffer-modifying
  :test ((block-comment-line 1)
	 (("l1long.init"
	   "l1long.result")
	  ("l1long.init"
	   "l1longc++.result"
	   :mode c++)
	  )
	 :mode c
	 :bindings
	 ((fill-column 70))))

(defTest block-comment-long-2x
  :parent block-cm-tests
  :name "Block comment long line 2x"
  :test-type eltest-buffer-modifying
  :test ((block-comment-line 2)
	 (("l1long.init"
	   "l2long.result")
	  ("l1long.init"
	   "l2longc++.result"
	   :mode c++)
	  )
	 :mode c
	 :bindings
	 ((fill-column 70))))


(defTest block-comment-long-3x
  :parent block-cm-tests
  :name "Block comment long line 3x"
  :test-type eltest-buffer-modifying
  :test ((block-comment-line 3)
	 (("l1long.init"
	   "l3long.result")
	  ("l1long.init"
	   "l3longc++.result"
	   :mode c++)
	  ("l4long.init"
	   "l4longc++.result"
	   :mode c++)
	  )
	 :mode c
	 :bindings
	 ((fill-column 70))))

;;(defTest block-cm-tests
;;  :name "make-block-region"
;;  :test-type buffer-modifying
;;  :test ((

(do-and-view-test 'block-cm-tests)

