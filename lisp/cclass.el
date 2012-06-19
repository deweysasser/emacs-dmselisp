;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cclass.el -- fake c++
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser <dewey@newvision.com>
;; Created On      : Thu Feb 13 15:25:20 1997
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defstruct cclass
;;   (defstruct standard-object
;;   (defstruct c-standard-generic-function
;;   (defvar cclass::all-builtin-subclasses nil
;;   (defvar cclass::generic-functions (make-hash-table :test 'equal)
;;   (defvar cclass::inheritance nil
;;   (defvar cclass::sorted-classes nil
;;   (defmacro push-on-end (value location)
;;   (defun cclass::topological-sort (class)
;;   (defmacro defcclass (&rest args)
;;   (defun build-proper-defstruct (args)
;;   (defun define-cclass (args)
;;   (defun add-class-to-super (class super)
;;   (defun cclass::add-to-map (name class)
;;   (defun cclass::getname (args)
;;   (defun cclass::getincludes (args)
;;   (defun find-cclass (name)
;;   (defmacro* defcmethod (&rest arguments)
;;   (defun* parse-defcmethod-arguments (arguments)
;;   (defun extract-lambda-list (args)
;;   (defun extract-specialized-args (args)
;;   (defun cclass::verify-class-exists (spec)
;;   (defun match-p (args method)
;;   (defun mode-matches-p (mode potential-modes)
;;   (defun get-function (method)
;;   (defun cclass::collect-arg-classes (args)
;;   (defun cclass::collect-arg-class-symbols (args max)
;;   (defun cclass::cache-match-test (x y)
;;   (defun cclass::find-class-entry (args alist)
;;   (defun cclass::find-cache (generic-function class-args)
;;   (defun cclass::set-cache-nofind (generic-function class-args
;;   (defun cclass::execute-method (method-name args)
;;   (defun cclass::execute-functions (args before-methods
;;   (defun get-spec (args)
;;   (defun get-real-args (args)
;;   (defun specializer-only (spec)
;;   (defun spec-p (spec what)
;;   (defun define-crippled-method (name options spec mode function)
;;   (defun c-ensure-generic-function (name)
;;   (defun remove-function (spec list)
;;   (defun c-add-methods (methods function spec mode)
;;   (defun alist-set (match list)
;;   (defun sort-methods (methods)
;;   (defun* compare-methods (first second)
;;   (defmacro cclass::define-builtin (class &optional superclass)
;;   (defun class-all-subclasses (class)
;;   (defun forget-all-classes ()
;;   (defun forget-all-methods ()
;;   (defun remove-mode-duplicates (list)
;;   (defun* cclass-of (object)
;;   (defun no-applicable-method (generic-function args)
;;   (defcmethod initialize-instance ((arg t))
;;   (defun call-next-method ( &rest maybe-args)
;;   (defun next-method-p ()
;;   (defun make-instance (class &rest init-args)
;;   (defadvice fmakunbound (before cclass activate)
;;   (defun cclass::read-object (stream char n)
;;   (defun slot-value (object slot)
;;   (defun print-unreadable-object (string)
;;   (defcmethod print-object ((obj standard-object))
;; 
;;  OPTIONS
;;    Update Tests:  yes
;;
;;  PROBLEMS
;;    o mode is not taken into account when resolving uniqueness
;;    o lambda list is not minimized.  If we can track the lambda
;;      list, we don't have to check classes for optional arguments,
;;      so lots of cclass-of's can be eliminated
;;
;; $RCSfile: cclass.el,v $
;; $Revision: 1.12 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)
(require 'cust-print)
(require 'cl-read)

(defstruct cclass
  name
  superclass
  slots
  (subclasses (list)))

(defstruct standard-object
  --cclass-internal-class)

(defstruct c-standard-generic-function
  before-methods
  after-methods
  around-methods
  primary-methods
  cache
  name
  (max-specialized-args 0)
  )

(defvar cclass::all-builtin-subclasses nil
  "Contain a list of all (deep) subclasses of builtin class.
This is done for performance reasons and should not be modified, nor
should classes be derived from class builtin (of course)")

(defvar cclass::generic-functions (make-hash-table :test 'equal)
  "A hash table of generic functions")

(defvar cclass::inheritance nil
  "classes hashed by tag")

;;(defvar cclass::specialized-methods nil
;;  "List of all specialized methods")


(defvar cclass::sorted-classes nil
  "List of all classes, sorted in order of precedence"
  )


(defmacro push-on-end (value location)
  `(setf ,location (nconc ,location (list ,value))))

(defun cclass::topological-sort (class)
  "Topologically sort the classes"
  (append (mapcan 'cclass::topological-sort (cclass-subclasses class))
	  (list class)))

;;;###autoload
(defmacro defcclass (&rest args)
  "*Define a c like class -- this is a defstruct, but we can hook the
:include"
  `(progn
     (defstruct ,@(build-proper-defstruct args))
     (define-cclass ',args)))

;;;###autoload
(defun build-proper-defstruct (args)
  "Make sure the defstruct includes standard-object"
  (let ((name-expr (first args)))
    (if (listp name-expr)
	(progn
	  (if (assoc ':include name-expr)
	      args
	    (push-on-end '(:include standard-object) name-expr )
	    (cons name-expr (cdr args))))
      (cons (list name-expr '(:include standard-object))
	    (cdr args)))))

;;;###autoload
(defun define-cclass (args)
  (let* ((name (cclass::getname args))
	 (includes (cclass::getincludes args))
	 (class (make-cclass :name name))
	 (slots (set-difference
		 (mapcar #'(lambda (x) (if (listp x) (car x) x))
			(if (stringp (cadr args))
			    (cddr args)
			  (cdr args))) '(class)))
	 (super (find-cclass (if includes includes 'standard-object))))
    (add-class-to-super class super)
    (setf (cclass-superclass class) super)
    (setq slots (remove-duplicates (append slots (cclass-slots super))))
    (setf (cclass-slots class) slots)
    (cclass::add-to-map name class)
    (setq cclass::sorted-classes (cclass::topological-sort (find-cclass 't)))
    name))

(defun add-class-to-super (class super)
  "Add class to the superclasses inheritors"
  (let ((list (cclass-subclasses super)))
    (setf (cclass-subclasses super)
	  (cons class
		(loop for x in list
		      if (not (eq (cclass-name x) (cclass-name class)))
		      collect x)))))

(defun cclass::add-to-map (name class)
  (setf (gethash name cclass::inheritance) class))

(defun cclass::getname (args)
  "Parse a defstruct for the name"
  (let ((name-sexp (car args)))
    (if (listp name-sexp)
	(car name-sexp)
      name-sexp)))

(defun cclass::getincludes (args)
  (let ((name-sexp (car args)))
    (if (listp name-sexp)
	(let ((list (assoc ':include (cdr name-sexp))))
	  (cadr list)))))

(defun find-cclass (name)
  "Return the class"
  (cond
   ((symbolp name)
    (gethash name cclass::inheritance))
   ((eq name nil)
    (find-cclass 't))
   ((cclass-p name)
    name)))   


;;;###autoload(autoload (quote defcmethod) "cclass"   "Define a (crippled) method.  Methods can specialize on the FIRST\nargument only" nil (quote macro))
(defmacro* defcmethod (&rest arguments)
  "*Define a (crippled) method.  Methods can specialize on the FIRST
argument only"
  (multiple-value-bind (method-name method-options method-mode
				    specialized-args args body)
      (parse-defcmethod-arguments arguments)
    `(progn
       (mapcar 'cclass::verify-class-exists ',specialized-args)
       (if (fboundp ',method-name)
	   (if (not (get ',method-name 'cmethod-generic-function))
	       (error "cmethod redefining non-cmethod function \"%s\""
		      ',method-name))
	 (defun ,method-name (&rest args)
	   (cclass::execute-method ',method-name args)))
       (define-crippled-method ',method-name ',method-options
	 ',specialized-args ',method-mode 
	 (function* (lambda ,args ,@body))))))


(defun* parse-defcmethod-arguments (arguments)
  "Parse the arguments to defcmethod, returning parts"
  (let ((method-name (first arguments))
	method-options
	method-mode
	specialized-args
	args
	body
	;; those are the returns, these the internals
	(var arguments)
	)
    (block nil
      (while (setq var (cdr var))
	(if (listp (first var))
	    (progn
	      (setq args (first var))
	      (setq body (cdr var))
	      (return-from nil nil))
	  (if (eq (first var) ':mode)
	      (progn
		(error "Mode specialized not yet supported")
		(setq method-mode (cadr var))
		(setq var (cdr var)))
	    (push-on-end (first var) method-options)))))
    (values
     method-name
     method-options
     method-mode
     (extract-specialized-args args)
     (extract-lambda-list args)
     body)))

(defun extract-lambda-list (args)
  "Extract the lambda list args from the list of args"
  (let (retlist
	(var (cons nil args)))
    (block nil
      (while (setq var (cdr var))
	(if (is-normal-arg-p (first var))
	    (push-on-end (if (listp (first var))
			     (caar var)
			   (car var))
			 retlist)
	  (return-from nil nil))))
    (if var
	(append retlist var)
      retlist)))

(defun extract-specialized-args (args)
  "Extract the specializer list from the args"
  (loop for x in args
	while (listp x)
	collect (cadr x)))


  (defun is-normal-arg-p (arg)
    "Return 't if arg is normal (not &XXX)"
    (not
     (if (symbolp arg)
	 (eq (aref (symbol-name arg) 0) ?&))))




(defun cclass::verify-class-exists (spec)
  "Signal unless class exists"
  (unless (find-cclass spec)
    (error "No Such class spec %s" spec)))

(defun match-p (args method)
  "Return 't if args are specialized to method"
  (let* ((spec (car method))
	 (mode (cadr method)))
    (and (every 'typep args spec)
	 (if mode
	     (mode-matches-p major-mode mode)
	   't))))

(declaim (inline match-p))

(defun mode-matches-p (mode potential-modes)
  "Return 't if a this function is applicable to this mode"
  (if (listp potential-modes)
      (member mode potential-modes)
    (eq mode potential-modes)))

  
(defun get-function (method)
  "Return the function part"
  (cddr method))

(declaim (inline get-function))

(defun cclass::collect-arg-classes (args)
  "Collect the classes of all the args"
  (mapcar 'cclass-of args))

;; Test 1 -- this is the faster, at least on sf- stuff.  It seems
;; wierd to cache args rather than classes, but QED.  It could
;; probably be optimized more, but right now is doing 310 sets of args
;; in about .6 seconds

;; However (7/14/97), this method also seems to cause stack overflows
;; in equal, which makes sense as that equal would be comparing large,
;; linked trees of class definitions

;;(defun cclass::collect-arg-class-symbols (args max)
;;  (let ((temp))
;;    (while (and args
;;		(<= 0 (decf max)))
;;      (push (pop args) temp))
;;    (nreverse temp)))



(defun cclass::collect-arg-class-symbols (args max)
  (mapcar #'(lambda (x) (cclass-name (cclass-of x)))
 	  args))

(declaim (inline cclass::collect-arg-classes cclass::collect-arg-class-symbols))

(defun cclass::cache-match-test (x y)
  (every 'eq x y))

(defun cclass::find-class-entry (args alist)
  "Find entry for args in alist.  Args is a list which should be
compared against the cars of alist using eq for each element"
;;   (loop for x in alist
;; 	if (every 'eq args (car x))
;; 	return x)
  (assoc args alist))

(declaim (inline cclass::find-class-entry))

(defun cclass::find-cache (generic-function class-args)
  "Locate the matching cache entry for args"
  (let ((cache-entry  (cclass::find-class-entry class-args
						(c-standard-generic-function-cache
						 generic-function))))
    (if cache-entry
	(cdr cache-entry))))


(defun cclass::set-cache-nofind (generic-function class-args
					   before-methods
					   after-methods
					   around-methods
					   primary-methods)
  "Set the cache for the generic functions"
  (push (cons class-args (list before-methods after-methods
			       around-methods primary-methods))
	(c-standard-generic-function-cache generic-function)))

;;;###autoload
(defun cclass::execute-method (method-name args)
  "Execute a method"
  (let* ((generic-function (get method-name 'cmethod-generic-function))
	 (arg-classes (cclass::collect-arg-class-symbols
		       args
		       (c-standard-generic-function-max-specialized-args
			generic-function)))
	 (cache (cclass::find-cache generic-function arg-classes)))
    (if cache
	(apply 'cclass::execute-functions args cache)
      (flet ((snarf-matches (list)
			    (loop for x in list
				  if (match-p args x)
				  collect (get-function x))))
	(let* (retval
	       (befores (c-standard-generic-function-before-methods
			 generic-function))
	       (afters (c-standard-generic-function-after-methods
			generic-function))
	       (arounds (c-standard-generic-function-around-methods
			 generic-function))
	       (primaries (c-standard-generic-function-primary-methods
			   generic-function))
	       (before-methods (snarf-matches befores))
	       (after-methods (reverse (snarf-matches afters)))
	       (around-methods (snarf-matches arounds))
	       (primary-methods (snarf-matches primaries)))
					;	  (debug)
	  (cclass::set-cache-nofind generic-function arg-classes before-methods
				    after-methods
				    around-methods
				    primary-methods)
	  (cclass::execute-functions args before-methods
				     after-methods around-methods
				     primary-methods)))))) 


	
(defun cclass::execute-functions (args before-methods
				       after-methods
				       around-methods 
				       primary-methods)
  ;; TODO:  CLOS defines arounds as happening around befores & afters
  ;; as well as primary.  This does it opposite.
  (let (retval
	(cclass::around-and-primary-methods (append around-methods primary-methods))
	(cclass::method-origional-args args))
    (unwind-protect
	(progn
	  (mapc (function (lambda (x)
			    (apply x args)))
		before-methods)
	  (setq retval
		(call-next-method)))
      (mapc (function (lambda (x)
			(apply x args)))
	    after-methods))))
	 

       
(defun get-spec (args)
  "Get the specializer"
  (cdar args))

(defun get-real-args (args)
  "Get the real arguments"
  (cons (caar args) (cdr args)))


(defun specializer-only (spec)
  "Return just the specializer"
  (if (listp spec)
      (car spec)
    spec))

(defun spec-p (spec what)
  "Predicate for before specializer"
  (member what spec))

;;;###autoload
(defun define-crippled-method (name options spec mode function)
  "Figure out if this is a before, after or normal and handle
appropriately"
  (let ((gf (c-ensure-generic-function name)))
    (if (> (length options) 1)
	(error "Too many method options: %s" name)
      (cond
       ((spec-p options ':before)
	(setf (c-standard-generic-function-before-methods gf)
	      (c-add-methods
	       (c-standard-generic-function-before-methods gf)
	       function spec mode)))
       ((spec-p options ':after)
	(setf (c-standard-generic-function-after-methods gf)
	      (c-add-methods
	       (c-standard-generic-function-after-methods gf)
	       function spec mode)))
       ((spec-p options ':around)
	(setf (c-standard-generic-function-around-methods gf)
	      (c-add-methods
	       (c-standard-generic-function-around-methods gf)
	       function spec mode)))
       ('t
	(setf (c-standard-generic-function-primary-methods gf)
	      (c-add-methods
	       (c-standard-generic-function-primary-methods gf)
	       function spec mode))))
      (put name 'cmethod-generic-function gf)
      ;; zero the cache -- it's invalid      
      (setf (c-standard-generic-function-cache gf) nil)

      ;; how many args must we check
      (setf (c-standard-generic-function-max-specialized-args gf)
	    (max (c-standard-generic-function-max-specialized-args gf)
		 (length spec)))
      name)))

(defun c-ensure-generic-function (name)
  "Ensure that this generic function exists, and return it"
  (let ((gf (gethash name cclass::generic-functions)))
    (if gf
	gf
      (setq gf (make-c-standard-generic-function :name name))
      (setf (gethash name cclass::generic-functions) gf)
      gf)))
  
(defun remove-function (spec list)
  "Return list without function that matches spec"
  (loop for x in list
	if (not (equal spec (car x)))
	collect x))

(defun c-add-methods (methods function spec mode)
  "Add a function to method list and return the new list"
  (let (list)
    (setq list (remove-function spec methods))
    (push (cons spec (cons mode function)) list)
    (setq list (sort-methods list))
    list))

(defun alist-set (match list)
  "Return a list of matches to MATCH in alist LIST"
  (loop for x in list
	if (eq match (car x))
	collect x))

(defun sort-methods (methods)
  "sort the methods in order of specialization"
  (sort methods
	'compare-methods))

(defun* compare-methods (first second)
  "Return 't if first method is less specific than second method"
  (mapc (function (lambda (x y)
	   "Compare x and y to see which is more specific"
	   (let ((lx (length (member (find-cclass x) cclass::sorted-classes)))
		 (ly (length (member (find-cclass y) cclass::sorted-classes))))
	     (if (> lx ly)
		 (return-from compare-methods 't)
	       (if (< lx ly)
		   (return-from compare-methods nil))))))
	(first first)
	(first second)))
  

  
(defmacro cclass::define-builtin (class &optional superclass)
  (if superclass
    `(define-cclass '((,class (:include ,superclass))))
    `(define-cclass ',(list class))))


(defun class-all-subclasses (class)
  "Return a list of all subclasses in order"
  ;; this information could be cached for better performance (maybe),
  ;; but it would'nt buy that much
  (cclass::topological-sort (find-cclass class)))

(defun forget-all-classes ()
  "Forget about all classes"

  (setq cclass::inheritance (make-hash-table))
  (setf (gethash 't cclass::inheritance) (make-cclass :name 't))
  (define-cclass '((standard-object (:include t))))

  (cclass::define-builtin builtin-class t)
  (cclass::define-builtin sequence builtin-class)
  (cclass::define-builtin list sequence)
  (cclass::define-builtin vector sequence)
  (cclass::define-builtin string vector)
  (cclass::define-builtin symbol builtin-class)
  (cclass::define-builtin number builtin-class)
  (cclass::define-builtin float number)
  (cclass::define-builtin integer number)

  ;; all builtin-class deriveratives must be defined before this
  (setq cclass::all-builtin-subclasses (class-all-subclasses 'builtin-class))
  nil
  )  



(defun forget-all-methods ()
  "Forget about all methods"
  (loop for x being the hash-keys of cclass::generic-functions
	do (fmakunbound x)))

(forget-all-classes)
(forget-all-methods)

(defun remove-mode-duplicates (list)
  "Receive a list of (spec mode lambda...) return a list containing
only the *first* occurrence of spec, mode.  Spec and mode may be
lists."
  (remove-duplicates list :test '(lambda (x y) (and (equal (car x) (car y))
						   (equal (cadr x) (cadr y))))
		     :from-end 't))





(defun* cclass-of (object)
  (or
   (and (standard-object-p object)
	(standard-object---cclass-internal-class object))
   (loop for class in cclass::all-builtin-subclasses
	 if (typep object (cclass-name class))
	 return class)))

(defun no-applicable-method (generic-function args)
  "Signal no applicable method"
  (error "No method applicable for %s"
	 (c-standard-generic-function-name generic-function)))

(defcmethod initialize-instance ((arg t))
  "Initial a new instance")

;; these two should not be called except in context


(defun call-next-method ( &rest maybe-args)
  (declare (special cclass::method-origional-args
		    cclass::around-and-primary-methods
		    generic-function))
  (setq cclass::method-origional-args
	(or maybe-args cclass::method-origional-args))
  (if (first cclass::around-and-primary-methods)
      (apply (pop cclass::around-and-primary-methods)
	     cclass::method-origional-args)
    (apply 'no-applicable-method generic-function
	   cclass::method-origional-args)))

(defun next-method-p ()
  (declare (special cclass::method-origional-args
		    cclass::around-and-primary-methods))
  cclass::around-and-primary-methods)

(defun make-instance (class &rest init-args)
  "Make an instance of a class"
  (let* ((class-object (find-cclass class))
	 (creator (intern (concat "make-" (symbol-name class))))
	 (obj (apply creator init-args)))
    (setf (standard-object---cclass-internal-class obj) class-object)
    (initialize-instance obj)
    obj))

(require 'advice)

(defadvice fmakunbound (before cclass activate)
  "Handle the cclass methods of symbol"
  (let ((name (ad-get-arg 0)))
    (remhash name cclass::generic-functions)
    (put name 'cmethod-generic-function nil)))




(defun cclass::read-object (stream char n)
  "Read a #C expansion.  Called by cl-read"
  (reader::check-0-infix n)
  (let* ((structure (read stream))
	 (temp (cdr structure))
	 (creation-function (intern (format "make-%s" (symbol-name
						       (first structure)))))
	 )
    (while temp
      (setcar temp (intern (format ":%s" (symbol-name (car temp)))))
      (setq temp (cddr temp)))
    (apply 'make-instance structure)))


(defun slot-value (object slot)
  "Slot value -- this is slow -- use accessors instead"
  (let* ((place 0))
    (loop for x in (get (cclass-name (cclass-of object))
			'cl-struct-slots)
	  if (eq (car x) slot)
	  return place
	  do (incf place))
    (aref object place)))

(defun print-unreadable-object (string)
  "Print an unreadable object"
  (princ (format "#<%s>" string)))

(defcmethod print-object ((obj standard-object))
  (print-unreadable-object (cclass-name (cclass-of obj))))


;;(defun call-next-method (&rest args)
;;  (error "call-next-method called not in specialized method"))
;;
;;(defun next-method-p ()
;;  (error "next-method-p called not in specialized method"))


(eval-when (load)
  (custom-print-install)
  (add-custom-printer 'standard-object-p 'print-object)
  (cl-reader-install)
  (set-dispatch-macro-character ?# ?C 'cclass::read-object)
  )

(provide 'cclass)

(eval-when (eval)
  (defun cclass-test-change ()
    (interactive)
    (elprof-restore-list elprof-function-list)
    (byte-compile-file "cclass.el")
    (load-file "cclass.elc")
    (load-file "eltest.elc")
    (sf::load-compiled-files)
    (elprof-instrument-list elprof-function-list)
    (elprof-set-master 'sf::document-object)
    (switch-to-buffer "sf-funcs-profile.el")
    (garbage-collect)
    (eval-buffer)
    (elprof-hierarchal-results)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                                   ;;;;
;;;;                   F u n c t i o n   P r o f i l e s               ;;;;
;;;;                                                                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Origional

;; Function Name                Call Count  Elapsed Time  Average Time
;; ===========================  ==========  ============  ============
;; call-next-method             331         144.16000000  0.4355287009
;; cclass::execute-method       295         107.25        0.3635593220
;; cclass::execute-functions    295         101.56999999  0.3443050847
;; define-cclass                5           0.1300000000  0.0260000000
;; cclass::collect-arg-classes  295         4.7800000000  0.0162033898
;; class-all-subclasses         231         2.2399999999  0.0096969696
;; cclass-of                    554         4.6300000000  0.0083574007
;; define-crippled-method       18          0.0699999999  0.0038888888
;; cclass::verify-class-exists  18          0.0599999999  0.0033333333
;; parse-defcmethod-arguments   18          0.0599999999  0.0033333333
;; compare-methods              16          0.0400000000  0.0025000000
;; cclass::set-cache            25          0.0599999999  0.0023999999
;; sort-methods                 18          0.0400000000  0.0022222222
;; c-add-methods                18          0.0400000000  0.0022222222
;; cclass::topological-sort     2149        3.9000000000  0.0018147975
;; extract-lambda-list          18          0.0299999999  0.0016666666
;; find-cclass                  614         0.4699999999  0.0007654723
;; cclass::find-cache           295         0.1900000000  0.0006440677
;; remove-function              18          0.0           0.0
;; c-ensure-generic-function    18          0.0           0.0
;; spec-p                       54          0.0           0.0
;; extract-specialized-args     18          0.0           0.0
;; is-normal-arg-p              34          0.0           0.0
;; cclass::getincludes          5           0.0           0.0
;; cclass::getname              5           0.0           0.0
;; cclass::add-to-map           5           0.0           0.0
;; add-class-to-super           5           0.0           0.0
;; build-proper-defstruct       5           0.0           0.0

;; Current
;; Function Name                      Call Count  Elapsed Time  Average Time
;; =================================  ==========  ============  ============
;; call-next-method                   348         119.81000000  0.3442816091
;; cclass::execute-method             312         89.800000000  0.2878205128
;; cclass::execute-functions          312         85.430000000  0.2738141025
;; make-instance                      29          0.4099999999  0.0141379310
;; define-crippled-method             18          0.2099999999  0.0116666666
;; cclass::collect-arg-classes        312         3.0000000000  0.0096153846
;; c-add-methods                      18          0.1500000000  0.0083333333
;; initialize-instance                29          0.2199999999  0.0075862068
;; sort-methods                       18          0.1200000000  0.0066666666
;; build-proper-defstruct             5           0.0299999999  0.0059999999
;; cclass-of                          563         2.6000000000  0.0046181172
;; class-all-subclasses               242         1.0800000000  0.0044628099
;; compare-methods                    32          0.1200000000  0.0037500000
;; parse-defcmethod-arguments         18          0.0599999999  0.0033333333
;; cclass::set-cache                  29          0.0600000000  0.0020689655
;; cclass::find-cache                 312         0.5999999999  0.0019230769
;; cclass::verify-class-exists        18          0.0299999999  0.0016666666
;; cclass::find-class-entry           341         0.3800000000  0.0011143695
;; find-cclass                        363         0.2099999999  0.0005785123
;; spec-p                             54          0.0299999999  0.0005555555
;; remove-function                    18          0.0           0.0
;; c-ensure-generic-function          18          0.0           0.0
;; extract-specialized-args           18          0.0           0.0
;; is-normal-arg-p                    34          0.0           0.0
;; extract-lambda-list                18          0.0           0.0
;; cclass::getincludes                5           0.0           0.0
;; cclass::getname                    5           0.0           0.0
;; cclass::add-to-map                 5           0.0           0.0
;; add-class-to-super                 5           0.0           0.0
;; define-cclass                      5           0.0           0.0

