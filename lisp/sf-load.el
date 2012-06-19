;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sf-load.el -- load sf files
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser <dewey@newvision.com>
;; Created On      : Fri Feb 21 12:48:52 1997
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defvar sf::files  '( "sf-objs.el"
;;   (defun sf::compile-files ()
;;   (defun sf::load-compiled-files ()
;;   (defun sf::profile ()
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: sf-load.el,v $
;; $Revision: 1.2 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sf::files  '( "sf-objs.el"
		      "sf-store.el"
		      "sf-token.el"
		      "sf-parse.el"
		      "sf-doc.el"
		      "sf-snarf.el"
		      ))

(defun sf::compile-files ()
  "Compile all sf::files"
  (interactive)
  (mapcar #'(lambda (x)
	      (if (file-newer-than-file-p x (concat x "c"))
		  (byte-compile-file x 't)))
	  sf::files))

(defun sf::load-compiled-files ()
  "Load all files, compiling if necessary"
  (interactive)
  (mapcar #'(lambda (x)
	      (let ((elc (concat x "c")))
		(if (file-newer-than-file-p x elc)
		    (byte-compile-file x 't)
		  (load-file elc))))
	  sf::files))

(defun sf::profile ()
  "Profile all the files"
  (interactive)
  (profile-files sf::files))

(sf::load-compiled-files)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Profiling
;; Current
;;
;; Function Name                         Call Count  Elapsed Time  Average Time
;; ====================================  ==========  ============  ============
;; sf::document-object                   5           5.4700000000  1.0940000000
;; sf::insert-object-documentation       5           2.8199999999  0.5639999999
;; save-object                           5           0.75          0.15
;; sf::get-fields                        5           0.6900000000  0.1380000000
;; sf::write-program-object              5           0.5           0.1
;; c-mode$sf::find-function              5           0.4000000000  0.0800000000
;; sf::find-program-object               5           0.4000000000  0.0800000000
;; sf::read-c-function-line              5           0.3700000000  0.0740000000
;; read-c-arg-list                       5           0.3400000000  0.0680000000
;; sf::find-object-buffer                5           0.25          0.05
;; sf::update-object                     20          0.8700000000  0.0435000000
;; sf::update-object-from-map            5           0.1900000000  0.0380000000
;; store-object                          20          0.6900000000  0.0345000000
;; sf::function-argument-formatter       5           0.1200000000  0.0240000000
;; sf::insert-field                      60          1.2600000000  0.0210000000
;; sf::parse-returns                     5           0.0899999999  0.0179999999
;; sf::update-object-field               40          0.6800000000  0.0170000000
;; print-object                          15          0.25          0.0166666666
;; read-c-argument                       15          0.2199999999  0.0146666666
;; sf::remove-header-prefix              55          0.6200000000  0.0112727272
;; sf::get-defaulted-value               60          0.4999999999  0.0083333333
;; sf::get-new-value                     60          0.4399999999  0.0073333333
;; sf::functions-format-arguments        10          0.0699999999  0.0069999999
;; sf::find-formatter                    60          0.4100000000  0.0068333333
;; sf::special-line-formatter            5           0.0300000000  0.0060000000
;; sf::insert-argument                   15          0.0900000000  0.0060000000
;; narrow-to-section                     5           0.0299999999  0.0059999999
;; sf::find-header-beginning             5           0.0299999999  0.0059999999
;; read-argument-default                 15          0.0600000000  0.0040000000
;; sf::update-doc-key                    30          0.1199999999  0.0039999999
;; sf::multiple-line-formatter           30          0.0999999999  0.0033333333
;; sf::parse-return-or-argument          15          0.0400000000  0.0026666666
;; sf::peek-token                        35          0.0900000000  0.0025714285
;; read-default-from-comment             15          0.0300000000  0.0020000000
;; initialize-instance                   15          0.0299999999  0.0019999999
;; sf::read                              70          0.0900000000  0.0012857142
;; sf::unread-token                      55          0.0600000000  0.0010909090
;; sf::program-object-get-documentation  30          0.0299999999  0.0009999999
;; sf::get-list-property                 50          0.0400000000  0.0008000000
;; sf::ensure-function                   5           0.0           0.0
;; object-to-section                     5           0.0           0.0
;; sf::complete-read-comment             10          0.0           0.0
;; sf::syntax-function-p                 15          0.0           0.0
;; sf::read-slash                        10          0.0           0.0
;; c-mode$sf::translate-return-type      5           0.0           0.0
;; sf::get-return-tokens                 5           0.0           0.0
;; get-next-word                         5           0.0           0.0
;; sf::join                              20          0.0           0.0
;; sf::comment-p                         40          0.0           0.0
;; sf::functions-get-returns             5           0.0           0.0
;; sf::find-correct-map                  5           0.0           0.0
;; sf::resolve-field-aliases             40          0.0           0.0
;; sf::get-object-order                  5           0.0           0.0
;; sf::include-if-blank-p                10          0.0           0.0
;; sf::variable-line-formatter           5           0.0           0.0
;; sf::single-line-formatter             20          0.0           0.0
;; sf::get-aliased-field-association     60          0.0           0.0
;; sf::find-header-end                   5           0.0           0.0
