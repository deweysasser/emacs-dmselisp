;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eltest.el -- elisp tests
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser <dewey@newvision.com>
;; Created On      : Thu Feb 13 13:06:46 1997
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defvar eltest::indent-inc 2
;;   (defvar eltest::dont-find-differences nil
;;   (defvar eltest::princ-indent 0
;;   (defvar eltest::tags (make-hash-table)
;;   (defvar eltest::show-error nil
;;   (defcclass (eltest
;;   (defcclass (eltest-series
;;   (defcclass (eltest-multiple-tests
;;   (defcclass (eltest-black-box
;;   (defcclass (eltest-buffer-modifying
;;   (defun* create-eltest-series (&rest args
;;   (defun* create-eltest-black-box (func args &key test)
;;   (defun*  create-eltest-buffer-modifying (form args &key mode
;;   (defvar eltest::creation-functions '((eltest . create-eltest)
;;   (defvar eltest::error nil
;;   (defun eltest::define-error-symbol (symbol description &rest keys)
;;   (defmacro flet-alias (bindings &rest body)
;;   (defmacro defTest (tag &rest args)
;;   (defun* eltest::create-test (&rest args
;;   (defun* make-eltest (test-type &key parent tag name test directory
;;   (defun add-test-tag (test)
;;   (defvar eltest::top nil
;;   (defun eltest::forget-all-tests ()
;;   (defcmethod add-test ((series eltest-series) test)
;;   (defun eltest-series-get-tests (test)
;;   (defun eltest::find-tag (tag)
;;   (defun add-tag (test)
;;   (defmacro get-file-as-string (file &rest body)
;;   (defmacro with-test-series-view-buffer (&rest body)
;;   (defmacro with-valid-test (test &rest body)
;;   (defmacro with-error-protection (&rest body)
;;   (defun do-and-view-test (&optional test)
;;   (defun view-test (test &optional failed-only)
;;   (defun view-failed-test (test)
;;   (defun do-and-view-failed-only (test)
;;   (defun test-completing-read ()
;;   (defcmethod do-test ((test string) &optional count)
;;   (defcmethod do-test ((test symbol) &optional count)
;;   (defcmethod do-test ((test eltest-series) &optional count)
;;   (defcmethod do-test ((test eltest-multiple-tests))
;;   (defcmethod do-one-test ((bb-test eltest-black-box) arguments results
;;   (defun create-test-result-buffer ()
;;   (defun test-results-viewing-mode ()
;;   (defmacro with-increased-indention (&rest body)
;;   (defcmethod print-test ((test symbol) &optional failed-only)
;;   (defcmethod print-test ((test eltest-series) &optional failed-only)
;;   (defcmethod print-test ((test eltest-multiple-tests) &optional failed-only)
;;   (defun eltest-print-subtest-intermediate (test arg result failed-only)
;;   (defcmethod eltest-print-subtest ((test eltest-black-box) arg result)
;;   (defcmethod eltest-success-p ((test eltest-series))
;;   (defcmethod eltest-success-p ((test eltest-multiple-tests))
;;   (defun finalize-test-result-buffer (buffer)
;;   (defvar eltest-minibuffer-history nil
;;   (defun eltest-delete-from-series ()
;;   (defun get-series-tags-for-completion ()
;;   (defun get-test-tags-for-completion ()
;;   (defun eltest-series-tests-for-completion (series)
;;   (defun eltest-series-remove (series test-tag)
;;   (defmacro princ-to-string (object)
;;   (defvar elprint::last-char nil
;;   (defun el::princ (object &optional printcharfun)
;;   (defun el::write-char (char stream)
;;   (defun eltest::snarf-from-file (file-or-files)
;;   (defcmethod do-one-test ((test eltest-buffer-modifying) init result
;;   (defun eltest-buffer-modifying-change-mode (mode)
;;   (defun eltest-buffer-modifying-equal (result expected-result)
;;   (defun eltest-buffer-modifying-setup ()
;;   (defcmethod eltest-print-subtest ((test eltest-buffer-modifying) arg
;;   (defun eltest::find-differences (string1 string2)
;;   (defun save-string-to-file (string filename)
;;   (defun do-diff-process (file1 file2)
;;   (defcmethod eltest-get-file ((test eltest-buffer-modifying) file)
;;   (defcmethod eltest-get-file :around ((test eltest) file)
;;   (defcmethod eltest-get-directory ((test eltest))
;;   (defcmethod do-test :around ((test eltest) &optional count)
;;   (defcmethod print-object ((test eltest))
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: eltest.el,v $
;; $Revision: 1.9 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'cl)
(require 'cclass)

;;;###autoload
(defvar eltest::indent-inc 2
  "*Amount of indention for each increment")

;;;###autoload
(defvar eltest::dont-find-differences nil
  "*If 't, diffs will not be done")

(defvar eltest::princ-indent 0
  "Current indention")
	
(defvar eltest::tags (make-hash-table)
  "Hash of all test tags")

(defvar eltest::show-error nil
  "*Signal error when it occurrs or not")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                           Class definitions                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcclass (eltest
	    (:constructor create-eltest))
  tag
  name
  directory
  parent
  bindings
  )

(defcclass (eltest-series
	    (:include eltest)
	    )
  tests; list of tests
  )



(defcclass (eltest-multiple-tests
	    (:include eltest))
  args
  results)

(defcclass (eltest-black-box
	    (:include eltest-multiple-tests))
  test
  func)



  
(defcclass (eltest-buffer-modifying
	    (:include eltest-multiple-tests)
	    )
  form
  mode
  trim-ws
  check-point)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                            Creator Funtions                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun* create-eltest-series (&rest args
				    &key tag name tests &allow-other-keys)
  "Create an eltest-series"
  (apply 'make-instance 'eltest-series args))
					;:tag tag :name name :tests tests))


(defun* create-eltest-black-box (func args &key test)
  "Create a black box test series"
  (make-instance 'eltest-black-box :func func :args args :test test))


(defun*  create-eltest-buffer-modifying (form args &key mode
					     ((:trim-surrounding-whitespace 
						 trim-ws))
					     check-point bindings
					     directory)
  "Create a buffer modifying test"
  (make-instance 'eltest-buffer-modifying :form form :args args :mode mode
		:trim-ws trim-ws :check-point check-point :bindings
		bindings :directory directory))



(defvar eltest::creation-functions '((eltest . create-eltest)
				    (eltest-series . create-eltest-series)
				    (eltest-black-box . create-eltest-black-box)
				    (eltest-buffer-modifying . create-eltest-buffer-modifying))
  "alist of functions to create tests")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                             Error Symbols                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar eltest::error nil
  "Global error symbol")

(defun eltest::define-error-symbol (symbol description &rest keys)
  (if keys
      (put symbol 'error-conditions (list 'error 'eltest::error keys symbol ))
    (put symbol 'error-conditions (list 'error 'eltest::error symbol )))
  (put symbol 'error-message description))

(defmacro flet-alias (bindings &rest body)
  (let ((real-bindings (mapcar*
			'(lambda (cell)
			   `(,(car cell) (&rest args) (apply ',(cadr cell) args)))
			bindings)))
  `(flet ,real-bindings
     ,@body)))


;;; Error symbols

(eltest::define-error-symbol 'eltest::error "An Eltest error")
(eltest::define-error-symbol 'eltest::no-such-test "Test does not exist")
(eltest::define-error-symbol 'eltest::no-applicable-method "No method for type")
(eltest::define-error-symbol 'eltest::too-few-args "Test does not exist")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                      Interface and Implementation                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;###autoload
(defmacro defTest (tag &rest args)
  `(apply 'eltest::create-test :tag ',tag ',args))

;;;###autoload(autoload 'eltest::create-test "eltest"   "Create the appropriate test" nil)
(defun* eltest::create-test (&rest args
				   &key test-type parent tag
				   &allow-other-keys)
  "Create the appropriate test"
  (let ((parent (eltest::find-tag parent))
	(test (apply 'make-eltest test-type args)))
    (add-test parent test)))


(defun* make-eltest (test-type &key parent tag name test directory
			       &allow-other-keys) 
  "Make an eltest of the appropriate type"
  (let* ((creation-pair (assoc test-type eltest::creation-functions))
	 (theTest (if (not (and creation-pair (cdr creation-pair)))
		   (signal 'eltest::no-such-test (list test-type))
		 (apply (cdr creation-pair) test))))
    (setf (eltest-name theTest) name)
    (setf (eltest-tag theTest) tag)
    (if directory
	(setf (eltest-directory theTest) (expand-file-name directory)))
    (add-tag theTest)))

(defun add-test-tag (test)
  "Add the test's tag to the tags table"
  (setf (gethash (eltest-tag test) eltest::tags) test))
  
(defvar eltest::top nil
  "The Top level test series")

;;;###autoload
(defun eltest::forget-all-tests ()
  "Get rid of all test"
  (interactive)
  (setq eltest::tags (make-hash-table))
  (setq eltest::top
	(make-eltest-series :tag 'Top :name "Top Level Series"))
  (add-test-tag eltest::top))

(eltest::forget-all-tests)



(defcmethod add-test ((series eltest-series) test)
  "Add a test to the test series"
  (let* ((tag (eltest-tag test))
	 (ret (assoc tag (eltest-series-tests series))))
    (setf (eltest-parent test) series)
    (if ret
	(setcdr ret test)
      (setf (eltest-series-tests series) (append
					  (eltest-series-tests series)
					  (list (cons tag test)))))))

(defun eltest-series-get-tests (test)
  "Return just the tests"
  (mapcar '(lambda (x)
	     (cdr x))
	  (eltest-series-tests test)))

(defun eltest::find-tag (tag)
  (if tag
      (cond
       ((stringp tag)
	(eltest::find-tag (intern tag)))
       ('t
	(gethash tag eltest::tags)))
    eltest::top))

(defun add-tag (test)
  (setf (gethash (eltest-tag test) eltest::tags) test))


;;; Test implementation

(defmacro get-file-as-string (file &rest body)
  "Return a string whose value is the contents of FILE."
  `(in-temp-buffer
    (insert-file-contents ,file)
    ,@body
    (buffer-substring (point-min)
		      (point-max))))

(defmacro with-test-series-view-buffer (&rest body)
  (let ((buffer (gensym)))
    `(progn
       (let ((,buffer (create-test-result-buffer)))
	 (with-output-to-buffer ,buffer
				,@body)
	 (finalize-test-result-buffer ,buffer)))))

(defmacro with-valid-test (test &rest body)
  `(progn
     (if (not (setq ,test (eltest::find-tag ,test)))
	 (error "Test %s is not a valid test" test)
       ,@body)))

(defmacro with-error-protection (&rest body)
  (let ((block-name (gensym)))
    `(block ,block-name
       (condition-case e
;	   (list ,@body)
	   ,@body
	 (eltest::error
	  (error "Error in eltest"))
	 (error
	  (if eltest::show-error
	      (apply 'signal e)
	    (return-from ,block-name
	      (list 'signal (format "%S" e)))))))))

(defun do-and-view-test (&optional test)
  (interactive (test-completing-read))
  (do-test test)
  (view-test test))

(defun view-test (test &optional failed-only)
  "View a completed test"
  (interactive (test-completing-read))
  (with-valid-test test
		   (with-test-series-view-buffer
		    (print-test test (or failed-only current-prefix-arg)))))

(defun view-failed-test (test)
  "View a completed test"
  (interactive (test-completing-read))
  (view-test test 't))

(defun do-and-view-failed-only (test)
  "View only the failures from a test"
  (interactive (test-completing-read))
  (do-test test)
  (view-test test 't))

(defun test-completing-read ()
  "Do a completing read on all the tests and return the result in a list"
  (list (completing-read "Select a test: "
			 (get-test-tags-for-completion)
			 nil
			 't
			 nil
			 'eltest-minibuffer-history)))

(defcmethod do-test ((test string) &optional count)
  "Do a test named by a string"
  (do-test (intern test) count))

(defcmethod do-test ((test symbol) &optional count)
  "Do a test named by a symbol"
  (with-valid-test test
		   (do-test test count)))

(defcmethod do-test ((test eltest-series) &optional count)
  "Do a test series"
  (setq count (or count 1))
  (loop for c from 1 to count
	do (mapc #'(lambda (x)
		     (message "Doing repetition %d" c)
		     (do-test x))
		 (eltest-series-get-tests test)))) 

(defcmethod do-test ((test eltest-multiple-tests))
  "Do a multiple-test test"
  (setf (eltest-multiple-tests-results test)
	(mapcar '(lambda (x)
		   (with-error-protection
		    (apply 'do-one-test test x)))
		(eltest-multiple-tests-args test))))

(defcmethod do-one-test ((bb-test eltest-black-box) arguments results
			 &key test signal)
  ;; each arg we get is a cons of what to apply to and what to expect
  ;; return value is '(t . success) if succeeded, '(t . (fail
  ;; . result)) if failed and (nil . string) if an error was caught
  (unless test
    (setq test 'equal))
  (let ((retval 
	 (with-error-protection
	  (let* ((retval (apply (eltest-black-box-func bb-test)
				arguments))
		 (comp (funcall test retval results)))
	    (if comp
		(list 'success)
	      (cons 'fail retval))))))
    (if signal
	(progn
	  (if (and
	       (eq (car retval) 'signal)
	       (string= results (cadr retval)))
	      '(success)
	    (cons 'fail retval)))
      retval)))

;;(defcmethod do-one-test ((bb-test eltest-black-box) arguments results
;;			 &key test signal)
;;  ;; each arg we get is a cons of what to apply to and what to expect
;;  ;; return value is '(t . success) if succeeded, '(t . (fail
;;  ;; . result)) if failed and (nil . string) if an error was caught
;;  (unless test
;;    (setq test 'equal))
;;  (let ((result
;;	 (with-error-protection
;;	  (let* ((retval (apply (eltest-black-box-func bb-test)
;;				(car arguments)))
;;		 (comp (funcall test retval (cadr arguments))))
;;	    (if comp
;;		(list 'success)
;;	      (cons 'fail retval))))))
;;    result))
;;    (if signal
;;	(progn
;;	  (if (and (eq 'signal (car result))
;;		   (string= (cadr result) results))
;;	      (list 'success)
;;	    (cons 'fail result)))
;;      result)))


(defun create-test-result-buffer ()
  "Create the test result buffer, initialize it, etc"
  (let* ((name "*Test Results*")
	 (buf (generate-new-buffer name)))
    (in-buffer buf
	       (test-results-viewing-mode))
    buf))

(defun test-results-viewing-mode ()
  "Mode for viewing test results"
  (make-local-variable 'outline-regexp)
  (outline-mode)
  (setq outline-regexp "[ \t]*[\\+\\-]"))

(defmacro with-increased-indention (&rest body)
  `(let ((eltest::princ-indent (+ eltest::indent-inc eltest::princ-indent)))
    ,@body))


(defcmethod print-test ((test symbol) &optional failed-only)
  "Print the test named by symbol"
  (with-valid-test test
		   (print-test test failed-only)))

(defcmethod print-test ((test eltest-series) &optional failed-only)
  "Print the test series"
  (let ((success (eltest-success-p test)))
    (if (not (and failed-only success))
	(progn
	(el::princ (format "%c Series: %s (%s):\n"
			   (if success ?+ ?-)
			   (eltest-name test)
			   (symbol-name (eltest-tag test))))
      (with-increased-indention
       (mapc '(lambda (x) (if x (print-test x failed-only))) (eltest-series-get-tests test)))))))

(defcmethod print-test ((test eltest-multiple-tests) &optional failed-only)
  "Print a multiple test test"
  (let ((success (eltest-success-p test)))
    (if (not (and failed-only success))
	(progn
	  (el::princ (format "%c multiple: %s (%s):\n"
			     (if success ?+ ?-)
			     (eltest-name test)
			     (symbol-name (eltest-tag test))
			     ))
	  (with-increased-indention
	   (let ((num 0))
	     (mapc '(lambda (x y )
		      (eltest-print-subtest-intermediate test x y failed-only))
		   (eltest-multiple-tests-args test)
		   (eltest-multiple-tests-results test)))
	   )
	  (el::princ "\n")))))

(defun eltest-print-subtest-intermediate (test arg result failed-only)
  "Print the +/- Test #: line, then print the subtest"
  (let ((success (eq 'success (car result))))
    (incf num)
    (if (not (and failed-only success))
	(progn
	  (el::princ (format "%c Test %d: "
			     (if success ?+ ?-)
			     num))
	  (declare (special num))
	  (if success
	      (el::princ "succeeded\n")
	    (el::princ "failed\n")
	    (with-increased-indention
	     (eltest-print-subtest test arg result)))))))

(defcmethod eltest-print-subtest ((test eltest-black-box) arg result)
  "Print the result"
  (el::princ "- Ran:      ")
  (el::princ (prin1-to-string
	      (cons (eltest-black-box-func test)
		    (car arg))))
  (el::princ "\n- Expected: ")
  (el::princ (prin1-to-string (cadr arg)))
  (el::princ "\n- Resulted: ")
  (el::princ (prin1-to-string (cdr result)))
  (el::princ "\n"))
      

(defcmethod eltest-success-p ((test eltest-series))
  "Did the series succeeded"
  (every 'eltest-success-p (eltest-series-get-tests test)))


(defcmethod eltest-success-p ((test eltest-multiple-tests))
  "Did the multiple test succeed"
  (every '(lambda (x)
	    (eq (car x) 'success))
	 (eltest-multiple-tests-results test)))


(defun finalize-test-result-buffer (buffer)
  "Do necessary stuff after test results have been put into buffer"
;  (in-buffer buffer
;	     (hide-sublevels 2))
  (switch-to-buffer-other-window buffer)
  (in-buffer buffer
	     (hide-sublevels 3)))

(defvar eltest-minibuffer-history nil
  "Minibuffer history")

(defun eltest-delete-from-series ()
  (interactive)
  (let* ((series (completing-read "Select a series: "
				  (get-series-tags-for-completion)
				  nil
				  't
				  nil
				  'eltest-minibuffer-history))
	 (test (completing-read "Select a test: "
				(eltest-series-tests-for-completion (eltest::find-tag
						      series))
				nil
				't
				nil
				'eltest-minibuffer-history)))
    (eltest-series-remove (eltest::find-tag series) (intern test))))

(defun get-series-tags-for-completion ()
  (loop for x being the hash-keys of eltest::tags
	if (eltest-series-p (gethash x eltest::tags))
	collect (cons (symbol-name (eltest-tag (gethash x
							eltest::tags)))
		      nil)))

(defun get-test-tags-for-completion ()
  (loop for x being the hash-keys of eltest::tags
	collect (cons (symbol-name (eltest-tag (gethash x
							eltest::tags)))
		      nil)))

(defun eltest-series-tests-for-completion (series)
  (loop for x in (eltest-series-tests series)
	collect (cons (symbol-name (car x)) nil)))

(defun eltest-series-remove (series test-tag)
  "Remove the test from the series"
  (setf (eltest-series-tests series)
	(loop for x in (eltest-series-tests series)
	      unless (eq test-tag (car x))
	      collect x)))

(defmacro princ-to-string (object)
  `(in-temp-buffer
   (princ ,object (current-buffer))
   (buffer-substring (point-min)
		     (point-max))))


(defvar elprint::last-char nil
  "'t if the last character written was a newline")
(make-variable-buffer-local 'elprint::last-char)

(defun el::princ (object &optional printcharfun)
  (mapc '(lambda (x) (el::write-char x printcharfun))
	(princ-to-string object)))

(defun el::write-char (char stream)
  "Write a character to a stream"
  (if elprint::last-char
      (progn
	(princ (make-string eltest::princ-indent ? ))
	(setq elprint::last-char nil)))
  (if (eq char ?\n)
      (setq elprint::last-char 't))
  (write-char char stream))



;; (defun el::princ (object &optional printcharfun)
;;   (let ((last-count -1)
;; 	(count 0)
;; 	(str (princ-to-string object)))
;;     (while (setq count (string-match "\n" str (if (> last-count 0) last-count 0)))
;;       (princ (substring str (1+ last-count) count))
;;       (princ (concat "\n" (make-string eltest::princ-indent ? )))
;;       (setq last-count (1+ count)))
;;     (princ (substring str (min (length str) (1+ last-count))))))


(defun eltest::snarf-from-file (file-or-files)
  "Snarf all tests from files, not running any"
  (interactive "f")
  (if (listp file-or-files)
      (mapc 'eltest::snarf-from-file file-or-files)
    (flet ((noop (arg)))
      (flet-alias
       ((do-and-view-test noop))
       (load-file file-or-files)))))





(defcmethod do-one-test ((test eltest-buffer-modifying) init result
			 &key mode bindings check-point
			 trim-surrounding-whitespace)
  "Do a single buffer-modifying test"
  (let* ((mode (or mode (eltest-buffer-modifying-mode test)))
	(bindings (append (eltest-buffer-modifying-bindings test)
			  bindings))
	(check-point (or check-point
			 (eltest-buffer-modifying-check-point test)))
	(tws (or trim-surrounding-whitespace
		 (eltest-buffer-modifying-trim-ws test)))
	(form (eltest-buffer-modifying-form test))
	(retval
	 (in-temp-buffer
	  (insert-file-contents init)
	  (if mode
	      (eltest-buffer-modifying-change-mode mode))
	  (eltest-buffer-modifying-setup)
;;	  (eval `(let (,@bindings)
;;		   ,form))
	  (eval form)
	  (if check-point
	      (insert "XXXPOINTXXX"))
	  (buffer-substring (point-min) (point-max))))
	(expected-result (get-file-as-string result))
	)				; end of let bindings
    (if (eltest-buffer-modifying-equal retval expected-result)
	'(success)
      (list 'fail retval))))

(defun eltest-buffer-modifying-change-mode (mode)
  "Change the mode of the current buffer to the give mode"
  (funcall (intern (concat (symbol-name mode) "-mode"))))

(defun eltest-buffer-modifying-equal (result expected-result)
  "Return 't if they are equal, handling white space trimming if
  applicable.  Depends on the eltest-buffer-modifying's do-one-test
method run time environment"
  (declare (special tws))
  (let* ((res (if tws
		  (trim-whitespace
		   result)
		result))
	 (exp (if tws
		  (trim-whitespace
		   expected-result)
		expected-result)))
    (equal res exp)))


(defun eltest-buffer-modifying-setup ()
  "setup the current buffer for buffer-modify-setup"
  (save-excursion
    (goto-char (point-min)) 
    (if
	(search-forward "XXXMARKXXX" nil 't)
	(progn
	  (replace-match "")
	  (set-mark-command nil))))
  (goto-char (point-min))
  (if
      (search-forward "XXXPOINTXXX" nil 't)
      (progn (replace-match ""))))

(defcmethod eltest-print-subtest ((test eltest-buffer-modifying) arg
				  result)
  "Print the result"
  (el::princ "- Ran:      ")
  (el::princ (prin1-to-string
	      (eltest-buffer-modifying-form test)))
  (el::princ "\n- Expected:\n")
  (with-increased-indention
   (el::princ (eltest-get-file test (cadr arg))))
  (el::princ "\n- Resulted:\n")
  (with-increased-indention
   (el::princ (princ-to-string (cadr result))))
  (el::princ "\n- Differences:\n")
  (with-increased-indention
   (el::princ (eltest::find-differences (eltest-get-file test (cadr arg))
					(cadr result))))
  (el::princ "\n"))

(defun eltest::find-differences (string1 string2)
  "Use diff(1) to compare two strings and return the differences"
  (if (not eltest::dont-find-differences)
      (let ((temp1 (make-temp-name "a"))
	    (temp2 (make-temp-name "b")))
	(unwind-protect
	    (progn
	      (save-string-to-file string1 temp1)
	      (save-string-to-file string2 temp2)
	      (do-diff-process temp1 temp2))
	  (with-error-protection
	   (delete-file temp1))
	  (with-error-protection
	   (delete-file temp2))))))
	

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

(defcmethod eltest-get-file ((test eltest-buffer-modifying) file)
  (condition-case e
      (get-file-as-string file)
    (error
     (format "Couldn't open file: %s" file))))

(defcmethod eltest-get-file :around ((test eltest) file)
  (let ((default-directory (eltest-get-directory test)))
    (call-next-method)))

(defcmethod eltest-get-directory ((test eltest))
  "Get the proper directory"
  (or (eltest-directory test)
      (and (eltest-parent test)
	   (eltest-get-directory (eltest-parent test)))
      default-directory))

(defcmethod do-test :around ((test eltest) &optional count)
  "Handle the directory changing part"
  (let ((bindings (eltest-bindings test)))
    (let ((default-directory (eltest-get-directory test)))
      (if bindings
	  (progn
	    (eval `(let (,@bindings)
		     (call-next-method))))
	(call-next-method)))))


(defcmethod print-object ((test eltest))
  "Print an eltest"
  (print-unreadable-object (format "%s %s"
				   (cclass-name (cclass-of test))
				   (eltest-tag test))))