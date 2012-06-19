;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; testfunc.el -- functions for testing elisp code
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser
;; Created On      : Mon Mar 11 22:35:34 1996
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defvar test-series-result-name "*Test Series Results*"
;;   (defvar test-series-order nil
;;   (defstruct (test-series
;;   (defun* make-test-series (&key tag name tests directory documentation)
;;   (defstruct test-case
;;   (defstruct (black-box-test-case
;;   (defstruct (buffer-modifying-test-case
;;   (defvar testfunc::test-series (make-hash-table)
;;   (defun* create-test-series (&rest args &key tag &allow-other-keys )
;;   (defun setf-find-test-series (tag value)
;;   (defsetf find-test-series setf-find-test-series)
;;   (defun test-series-increment (series)
;;   (defun find-test-series (name)
;;   (defmacro defTestSeries (tag &rest args)
;;   (defmacro defTest (tag &rest args)
;;   (defun* testfunc::create-test
;;   (defun* test-series-add-test (series test &key test-number name &allow-other-keys)
;;   (defun* testfunc::create-one-test
;;   (defmacro with-error-protection (&rest body)
;;   (defun execute-test (test-case)
;;   (defun black-box-executor (test-case)
;;   (defun* black-box-do-one-test (test-case args expected-result &key signal)
;;   (defmacro with-valid-series (series-tag series-var &rest body)
;;   (defun do-test-series (series-tag)
;;   (defun view-series-results (series-tag)
;;   (defun do-and-view-test-series (series-tag)
;;   (defun print-series-results (series-tag)
;;   (defun test-case-succeeded-p (test-case)
;;   (defun test-series-succeeded-p (test-series)
;;   (defun test-case-print (test-case)
;;   (defun black-box-succeeded-p (bb results)
;;   (defun black-box-printer (bb results)
;;   (defun buffer-modify-executor (test-case)
;;   (defun* buffer-modify-execute-test
;;   (defun buffer-modifying-equal (test-case result expected-result)
;;   (defun princ-test-case-results (test case-number success results-object &optional diff)
;;   (defun buffer-modify-printer (test-case results)
;;   (defun buffer-modifying-find-differences (test-case result-file results)
;;   (defun save-string-to-file (string filename)
;;   (defun do-diff-process (file1 file2)
;;   (defun buffer-modify-do-mode (mode)
;;   (defun buffer-modify-setup ()
;;   (defmacro get-file-as-string (file &rest body)
;;   (defun snarf-series-from-file (file)
;;   (defun do-and-view-all-test-series (&optional order)
;;   (defun do-all-test-series (&optional order)
;;   (defun view-all-test-series (&optional order)
;;   (defmacro with-test-series-view-buffer (&rest body)
;;   (defun create-test-result-buffer ()
;;   (defun test-results-viewing-mode ()
;;   (defun finalize-test-result-buffer (buffer)
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: testfunc.el,v $
;; $Revision: 1.7 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(if (string-lessp emacs-version "19.30")
    (require 'backquote))

(require 'cl)

(defvar test-series-result-name "*Test Series Results*"
  "Name of buffer to display test series results")

(defvar test-series-order nil
  "Order of test series")

(defstruct (test-series
	    (:constructor make-test-series-internal)
	    )
  tag
  name
  documentation
  tests
  next-number
  directory
  )

(defun* make-test-series (&key tag name tests directory documentation)
  "Make a new test-series object"
  (make-test-series-internal
   :tag tag
   :name name
   :tests tests
   :documentation documentation
   :directory (if directory
		  (expand-file-name directory)
		default-directory)
   :next-number 0))

(defstruct test-case
  name
  test-number
  test-results
  test-exec
  test-print
  test-succeeded-p
  )				;could really benefit from CLOS here

(defstruct (black-box-test-case
	    (:include test-case)
	    (:constructor nil)
	    (:constructor make-black-box-test-case
			  (func args &key name test-number
				&aux (test-exec
				      'black-box-executor)
				(test-print 'black-box-printer)
				(test-succeeded-p 'black-box-succeeded-p))))
  test
  func
  args
  actual-result
  )

(defstruct (buffer-modifying-test-case
	    (:include test-case)
	    (:constructor nil)
	    (:constructor make-buffer-modifying-test-case
			  (form files &key name
				test-number
				setup
				cleanup
				mode
				bindings
				((:trim-surrounding-whitespace
				  trim-ws))
				check-point
				&aux (test-exec
				      'buffer-modify-executor)
				(test-print
				 'buffer-modify-printer)
				(test-succeeded-p
				 'buffer-modify-succeeded-p))))
  form
  files
  setup
  cleanup
  bindings
  mode
  actual-result
  trim-ws
  check-point
  )
	    

	    
(defvar testfunc::test-series (make-hash-table)
  "Tests")


(defun* create-test-series (&rest args &key tag &allow-other-keys )
  "Create a new test-series object and enter it into test series catalog"
  (setq nt (apply 'make-test-series :allow-other-keys args))
  (setf (find-test-series (test-series-tag nt))
	nt)
  (if (not (memq tag test-series-order))
      (setq test-series-order (append test-series-order (list tag)))))

		   
(defun setf-find-test-series (tag value)
  "Arrange for (find-test tag) to produce value"
  (setf (gethash tag testfunc::test-series) value))

(defsetf find-test-series setf-find-test-series)

(defun test-series-increment (series)
  "Increment test next-number and return the old one"
  (incf (test-series-next-number series)))

(defun find-test-series (name)
  "Find the series who's name is NAME"
  (gethash name testfunc::test-series))

;;;###autoload
(defmacro defTestSeries (tag &rest args)
  `(apply 'create-test-series :tag ',tag ',args))

;;;###autoload
(defmacro defTest (tag &rest args)
  `(apply 'testfunc::create-test :tag ',tag ',args))

(defun* testfunc::create-test
    ( &rest args &key tag name test-type test &allow-other-keys)
  "Define a new test in series given by TAG"
  (let ((series (find-test-series tag)))
    (if (not series)
	     (setq series (create-test-series :tag tag)))
    (let* ((nnum (test-series-increment series))
	   (test (apply 'testfunc::create-one-test args)))
      (apply 'test-series-add-test series test :test-number nnum args)
      )))


(defun* test-series-add-test (series test &key test-number name &allow-other-keys)
  "Add test to series."
  (setf (test-case-test-number test) test-number)
  (setf (test-case-name test) name)
  (setf (test-series-tests series) (append (test-series-tests series)
					  (list test))))

(defun* testfunc::create-one-test
    (&rest args &key test-type test &allow-other-keys)
  "Create a test and return it."
  (let ((creator-name (intern (concat "make-"
				      (symbol-name test-type)
				      "-test-case"))))
    (apply creator-name test)))


(defmacro with-error-protection (&rest body)
  (let ((block-name (gensym)))
    `(block ,block-name
       (condition-case e
	   (list 't ,@body)
	 (error
	  (return-from ,block-name
	    (list nil (format "%S" e))))))))


(defun execute-test (test-case)
  "Execute a test case, returning ('t . result) if it doesn't throw an
  error, and (nil . WHY) if it does.  Note that result must still be
  checked to see if test-case passed."
  (let ((result
	 (with-error-protection
	  (funcall (test-case-test-exec test-case) test-case))))
     (setf (test-case-test-results test-case) result)))


(defun black-box-executor (test-case)
  "Execute a black box test.  Don't worry about conditions.  They'll
  be trapped in a calling context."
  (mapcar '(lambda (arg)
	       (apply 'black-box-do-one-test
		      test-case
		      arg))
	    (black-box-test-case-args test-case)))

(defun* black-box-do-one-test (test-case args expected-result &key signal)
  "Do a test"
  (let ((test (black-box-test-case-test test-case)))
    (unless test
      (setq test 'equal))
    (let ((result  (with-error-protection
		    (funcall test
			     expected-result
			     (apply (black-box-test-case-func test-case)
				    args)))))
      (if signal			;if it's supposed to signal
	  (if (not (car result))	;and it signaled
	      (list 't (equal expected-result (cadr result))) ;compare the signal value
	    (list nil (cdr result))	; but it didn't signal, so
					; return car of 'nil'
					; indicating that the signal
					; state was other than
					; expected
	    )
	result				;otherwise, return result
	))))
  

(defmacro with-valid-series (series-tag series-var &rest body)
  "Handle validating series"
  `(let ((,series-var (find-test-series ,series-tag)))
    (unless ,series-var
      (error "No such series:  %S" ,series-tag))
    ,@ body))


;;;###autoload
(defun do-test-series (series-tag)
  "Execute a series of tests"
  (with-valid-series
   series-tag series
   (let ((*current-test-series* series)
	 (default-directory (expand-file-name
			     (if (test-series-directory series)
				 (test-series-directory series)
			       "."))))
     (mapc
      '(lambda (arg)
	 (execute-test arg))
      (test-series-tests series)))))


;;;###autoload
(defun view-series-results (series-tag)
  "View test results in a temp buffer"
   (with-test-series-view-buffer "*Test Series Results*"
     (print-series-results series-tag)))

;;;###autoload
(defun do-and-view-test-series (series-tag)
  "Execute the test series and view the result"
  (do-test-series series-tag)
  (view-series-results series-tag))

(defun print-series-results (series-tag)
  "Print results from a run"
  (with-valid-series
   series-tag series
   (let ((default-directory  (test-series-directory series)))
     (princ (format "+ In Series %s\n" (test-series-name series)))
     (mapc '(lambda (arg)
	      (let ((results (test-case-test-results arg)))
		(if (eq (car results)
			nil)		;error was trapped by execute
		    (princ (format "  + Test %d %s\n"
				   (test-case-test-number arg)
				   (cadr results)))
		  (princ (format "  + Test %d (%s):\n" (test-case-test-number arg)
				 (test-case-name arg)))
		  (test-case-print arg)
		  (princ "\n"))))
	   (test-series-tests series)))))

(defun test-case-succeeded-p (test-case)
  (check-type test-case test-case)
  (funcall (test-case-test-succeeded-p test-case)))

(defun test-series-succeeded-p (test-series)
  (check-type test-series test-series)
  (apply 'and (mapcar 'test-case-succeeded-p
			(test-series-tests test-series))))

(defun test-case-print (test-case)
  "Print a test case"
  (let ((results (test-case-test-results test-case)))
    (if (car results)
	(funcall (test-case-test-print test-case) test-case (cadr results)))))


(defun black-box-succeeded-p (bb results)
  (let ((number (test-case-test-number bb))
	(index 1))
    (every '(lambda (arg) arg)
	   (mapcar '(lambda (arg)
		      (let ((success (every '(lambda (arg) arg) arg)))
			;; (princ (format "     %d.%d(%s):  %S\n"
			;;	      number index
			;;	      (if success
			;;		  "succeeded"
			;;		"failed")
			;; arg))
			success))
		   results))))
			      
(defun black-box-printer (bb results)
  "Print a black box test.  Already checks for error from execute."
  (let ((number (test-case-test-number bb))
	(index 1))
    (mapc '(lambda (arg test)
	     (let ((success (every '(lambda (arg) arg) arg)))
	       (princ-test-case-results bb index success arg
					(if success
					    nil
					  test))
	       ;; (princ (format "     %d.%d(%s):  %S\n"
	       ;;	      number index
	       ;;	      (if success
	       ;;		  "succeeded"
	       ;;		"failed")
	       ;; arg))
	       (incf index)))
	  results (black-box-test-case-args bb))))


(defun buffer-modify-executor (test-case)
  "Execute a buffer modifying test case"
  (mapcar '(lambda (arg)
	     (apply 'buffer-modify-execute-test test-case arg))
	  (buffer-modifying-test-case-files test-case)))

(defun* buffer-modify-execute-test
    (test-case start-file result-file
	       &key mode bindings form setup-form cleanup-form)
  "Do a test"
  (unless mode
    (setq mode (buffer-modifying-test-case-mode test-case)))
  (unless bindings
    (setq bindings (buffer-modifying-test-case-bindings  test-case)))
  (unless form
    (setq form (buffer-modifying-test-case-form test-case)))
  (unless setup-form
    (setq setup-form (buffer-modifying-test-case-setup  test-case)))
  (unless cleanup-form
    (setq cleanup-form (buffer-modifying-test-case-cleanup  test-case)))
  
  
  (with-error-protection
   (let ((result
	  (in-temp-buffer
	   (insert-file start-file)
	   (if mode
	       (buffer-modify-do-mode mode))
	   (if setup-form
	       (eval setup-form))
	   (buffer-modify-setup)
	   (eval `(let (,@bindings)
		    ,form))
	   ;;(eval form)
	   (if (buffer-modifying-test-case-check-point test-case)
	       (insert "XXXPOINTXXX"))
	   (if cleanup-form
	       (eval cleanup-form))
	   (buffer-substring (point-min)
			     (point-max))))
	 (expected-result
	  (get-file-as-string result-file)))
     (if (buffer-modifying-equal test-case result expected-result)
	 't
       (list nil result)))))


(defun buffer-modifying-equal (test-case result expected-result)
  "Return 't if they are equal, handling white space trimming if
  applicable"
  (let* ((tws (buffer-modifying-test-case-trim-ws test-case))
	 (res (if tws
		  (trim-whitespace
		   result)
		result))
	 (exp (if tws
		  (trim-whitespace
		   expected-result)
		expected-result)))
    (equal res exp)))

(defun princ-test-case-results (test case-number success results-object &optional diff)
  "Prints the test case results in a standardized format"
  (princ (format "     + %d.%d(%s):  \n       + result\n%S\n"
		 (test-case-test-number test)
		 case-number
		 (if success
		     "Succeeded"
		   "Failed")
		 results-object))
  (if diff
      (princ (format "       + difference\n%S\n" diff))))


(defun buffer-modify-printer (test-case results)
  "Print the test case"
  (let ((sub-test 1))
    (mapc
     '(lambda (arg files)
	(let ((success (every '(lambda (arg) (eq 't arg)) arg)))
	  
	  (princ-test-case-results test-case sub-test success arg (if (not success)
	      (if (not success)
		(buffer-modifying-find-differences test-case (cadr files)
							  (cadr arg)))))
;;	  (princ (format "  %d.%d (%s):" (test-case-test-number test-case)
;;			 sub-test
;;			 ))
;;	  (prin1 arg)
	  
;;	  (princ "\n")
	  (incf sub-test)))
     results
     (buffer-modifying-test-case-files test-case))))

(defun buffer-modifying-find-differences (test-case result-file results)
  "Find differences between actual and expected results"
  (if (listp results)
      (let* ((file (expand-file-name result-file))
	     (temp-name
	      (make-temp-name (file-name-directory file))))
	(save-string-to-file
	 (cadr results)
	 temp-name)
	(prog1
	    (do-diff-process temp-name
			     file)
	  (delete-file temp-name)))))

(defun save-string-to-file (string filename)
  "Write STRING to FILENAME, overwriting any existing file"
  (in-temp-buffer
   (insert string)
   (write-region (point-min) (point-max) filename)))

(defun do-diff-process (file1 file2)
  "Diff the two files, return the result"
  (in-temp-buffer
   (message "Doing diff")
   (call-process "diff" nil (current-buffer) nil file1 file2)
   (message "Doing diff...done")
   (buffer-substring (point-min) (point-max))))
		     

(defun buffer-modify-do-mode (mode)
  "change to specified-mode"
  (let (new-func
	(name (symbol-name mode)))
    (if (string-match "-mode" name)
	(funcall mode)
      (funcall (intern (concat name "-mode"))))))



(defun buffer-modify-setup ()
  "setup the current buffer for buffer-modify-setup"
  (save-excursion
    (beginning-of-buffer)
    (if
	(search-forward "XXXMARKXXX" nil 't)
	(progn
	  (replace-match "")
	  (set-mark-command nil))))
    (beginning-of-buffer)
    (if
	(search-forward "XXXPOINTXXX" nil 't)
	(progn (replace-match ""))))


(defmacro get-file-as-string (file &rest body)
  "Return a string whos value is the contents of FILE."
  `(in-temp-buffer
    (insert-file ,file)
    ,@body
    (buffer-substring (point-min)
		      (point-max))))


;;;###autoload
(defun snarf-series-from-file (file)
  "Read test series from file.  This function temporarily redefines
  the execution and viewing functions to be non operative"
  (flet ((noop (arg)))
    ;; TODO:  Must rewrite this.  flet-alias no longer exists
    (flet-alias
     ((do-test-series noop)
      (view-series-results noop)
      (do-and-view-test-series noop))
     (load-file file))))

(defun do-and-view-all-test-series (&optional order)
  "Do all defined test series in order specified by test-series-order"
  (let ((done nil))
    (flet ((isdone (tag)
		   (memq tag done))
	   (just-done (tag)
		      (push tag done)))
      (unless order
	(setq order test-series-order))
      (with-test-series-view-buffer test-series-result-name
	(mapc (function (lambda (series)
			  (if (isdone series)
			      nil
			    (do-test-series series)
			    (print-series-results series)
			    (just-done series))))
	      order)))))

(defun do-all-test-series (&optional order)
  "Do all defined test series in order specified by test-series-order"
  (let ((done nil))
    (flet ((isdone (tag)
		   (memq tag done))
	   (just-done (tag)
		      (push tag done)))
      (unless order
	(setq order test-series-order))
      (mapc (function (lambda (series)
			(if (isdone series)
			    nil
			  (do-test-series series)
			  (just-done series))))
	    order))))

(defun view-all-test-series (&optional order)
  "Do all defined test series in order specified by test-series-order"
  (let ((done nil))
    (flet ((isdone (tag)
		   (memq tag done))
	   (just-done (tag)
		      (push tag done)))
      (unless order
	(setq order test-series-order))
      (with-test-series-view-buffer test-series-result-name
	(mapc (function (lambda (series)
			  (if (isdone series)
			      nil
			    (print-series-results series)
			    (just-done series))))
	      order)))))


;;(defmacro with-test-series-view-buffer (&rest body)
;;  `(with-output-to-temp-buffer ,@body))

(defmacro with-test-series-view-buffer (&rest body)
  (let ((buffer (gensym)))
    `(progn
       (setq ,buffer (create-test-result-buffer))
       (with-output-to-buffer ,buffer
			      ,@body)
       (finalize-test-result-buffer ,buffer))))


(defun create-test-result-buffer ()
  "Create the test result buffer, initizlize it, etc"
  (let ((buf (generate-new-buffer test-series-result-name)))
    (in-buffer buf
	       (test-results-viewing-mode))
    buf))


(defun test-results-viewing-mode ()
  "Mode for viewing test results"
  (make-local-variable 'outline-regexp)
  (outline-mode)
  (setq outline-regexp "[ \t]*\\+"))

(defun finalize-test-result-buffer (buffer)
  "Do necessary stuff after test results have been put into buffer"
;  (in-buffer buffer
;	     (hide-sublevels 2))
  (switch-to-buffer-other-window buffer)
  (in-buffer buffer
	     (hide-sublevels 6)))



    
(provide 'testfunc)
