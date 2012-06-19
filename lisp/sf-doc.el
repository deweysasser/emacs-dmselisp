;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sf-doc.el -- sf documentation functions
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser <dewey@newvision.com>
;; Created On      : Wed Feb 19 19:30:41 1997
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defvar sf::default-documentation-alist
;;   (defvar sf::field-aliases '(("CAVEATS AND BUGS" . "PRIVATE NOTES"))
;;   (defvar sf::documentation-order-list '(name "CLASS" "AUTHOR"
;;   (defvar sf::lines-formatting-alist '((single
;;   (defvar sf::lines-special-formatters-alist '((name . sf::name-formatter)))
;;   (defmacro with-inserted-indention (indention &rest body)
;;   (defmacro with-inserted-indention-first (first indention &rest body)
;;   (defmacro with-point-at-end (&rest body)
;;   (defmacro with-filled-paragraphs (fill-prefix &rest body)
;;   (defmacro assoc-value (key list)
;;   (defmacro assoc-value-from-list (key list)
;;   (defmacro dassert (form string)
;;   (defmacro debug-if (form &optional return)
;;   (defun sf::get-list-property (list field property)
;;   (defun sf::get-field-property (list property)
;;   (defun sf::document-object ()
;;   (defcmethod sf::find-header-beginning ((object sf::function-base))
;;   (defun sf::insert-object-documentation (object &optional
;;   (defun sf::get-aliased-field-association (field alist)
;;   (defun sf::insert-field (object field value default)
;;   (defun sf::get-defaulted-value (object field value default)
;;   (defun sf::single-line-formatter (object field value default)
;;   (defun sf::multiple-line-formatter (object field value default)
;;   (defun sf::variable-line-formatter (object field value default)
;;   (defun sf::is-multiple-lines (string)
;;   (defun sf::include-if-blank-p (alist)
;;   (defcmethod sf::find-formatter ((object sf::program-object) field
;;   (defcmethod sf::find-formatter ((object sf::function) Field value
;;   (defun sf::function-argument-formatter (object field value default)
;;   (defun sf::insert-argument (arg value options)
;;   (defcmethod sf::special-line-formatter ((object sf::function-base)
;;   (defcmethod sf::special-line-formatter ((object sf::class) field value
;;   (defcmethod sf::get-new-value ((object sf::program-object) field value
;;   (defcmethod sf::get-new-value :around ((object sf::function-base) field value
;;   (defcmethod sf::get-new-value :around ((object sf::member-function)
;;   (defun sf::program-object-get-documentation  (ojbect key)
;;   (defun sf::get-object-order (object order)
;;   (defcmethod sf::update-object ((object sf::program-object) values
;;   (defcmethod sf::update-object ((object sf::argument) value defaults
;;   (defun sf::resolve-field-aliases (field)
;;   (defcmethod sf::update-object-field ((object sf::function-base) name value defaults)
;;   (defcmethod sf::update-object-field ((object sf::program-object) name value
;;   (defun sf::find-correct-map (object map)
;;   (defun sf::update-object-from-map (object map name value defaults)
;;   (defun sf::parse-returns (value)
;;   (defun sf::update-arguments (object value defaults)
;;   (defun sf::update-argument (argument alist)
;;   (defun sf::update-doc-key (object field key value defaults)
;;   (defmacro with-each-argument-narrowed (string &rest body)
;;   (defun sf::functions-format-arguments (object value default)
;;   (defvar sf::argument-name-regexp "^[ \t]*\\(\\(\\s_\\|\\sw\\)*\\)?[ \t]*([^)]*) -- "
;;   (defun sf::functions-get-returns (object value default)
;;   (defun sf::parse-return-or-argument ()
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: sf-doc.el,v $
;; $Revision: 1.2 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sf-objs)


(defvar sf::default-documentation-alist
  '(("VISIBILITY"
     (default-value "PUBLIC | PRIVATE")
     (documentation
      "If this function is internal to your package, it's PRIVATE")
     (lines single))
    ("FORMAT VERSION"
     (lines single)
     (default-value "0.0"))
    ("CLASS"
     (lines single))
    ("EXCEPTIONS"
     (value "NONE")
     (documentation
      "Any exceptions thrown in or below this function.")
     (lines variable))
    ("TESTS AND COVERAGE"
     (lines multiple))
    ("PRIVATE NOTES"
     (default-value "|><|")
     (documentation
      "Any private documentation.")
     (documentation-key private-notes)
     (lines multiple))
    ("RETURNS"
     (documentation "Return value of the function, and what it means")
     (default-value "NOTHING")
     (documentation-key returns)
     (include-blank t)
     (lines variable))
    ("ARGUMENTS"
     (documentation "what the function takes")
     (map (sf::function-base
	   sf::function-base-args nil))
     (lines variable))
    ("DESCRIPTION"
     (default-value "|>Description of function<|")
     (documentation-key documentation)
     (documentation
      "Describe the function.  What does it do, and how if applicable")
     (lines multiple))
    (name
     (update-function doc-insert-name)
     (documentation-key brief-documentation)
     (lines special))
    ("NOTES"
     (default-value "|><|")
     (lines multiple)
     (documentation
      "Any public notes on the function?  This goes into the command ref")
     (documentation-key notes)
     (default-value "|><|"))
    ("SEE ALSO"
     (lines multiple)
     (documentation
      "Cross References?  What should the user also look into?")
     (documentation-key see-also)
     (default-value "|><|"))
    ("AUTHOR"
     (default-function user-full-name-and-email)
     (documentation "who wrote it")
     (lines single))
    )
  "\
Documentation of format:

Top level list structure is an alist.  Each key of the alist is a
field.  All keys (except name) are strings

The value of each entry is either a string, in which case it means
\"refer to the entry for this string\", or another alist.  The string
is in here so fields that get renamed can be handled appropriately

This sub alist is the list of values for the field.  All values are
symbols.

These values can be

documentation -- documentation for how to use this field
lines         -- What type of field we have.  Choices are 'single
                 for a single line following the field name,
                 'multiple meaning the field always takes multiple
                 lines, 'variable meaning \"one if possible,
                 otherwise many\" and 'special meaning it depends on
                 the field name (and name is the only one with this
                 attribute
default-value -- the default value to use in this field
default-function -- function to evaluate to produce default.  Function
                    takes NO arguments. 
include-blank -- 't if this field should be included even if blank
documentation-key -- key for this field in documentation
map -- list of (type list-accessor documentation-key).  If this exists
       for the current type (as determined by typep),
       sf::update-object will be called for each object return by the
       given list-accessor with the arguments OBJECT VALUE DEFAULTS
       DOCUMENTATION-KEY.  Value will be determined by looking up the
       object name in the parsed values from the field.
"
  )

(defvar sf::field-aliases '(("CAVEATS AND BUGS" . "PRIVATE NOTES"))
  "List of field aliases")


(defvar sf::documentation-order-list '(name "CLASS" "AUTHOR"
                 "VISIBILITY" "DESCRIPTION" "ARGUMENTS"
                 "RETURNS" "NOTES" "SEE ALSO" "PRIVATE NOTES"
                 "TESTS AND COVERAGE" "FORMAT VERSION")
  "Order in which fields are included")

(defvar sf::lines-formatting-alist '((single
				      . sf::single-line-formatter)
				     (multiple
				      . sf::multiple-line-formatter)
				     (special
				      . sf::special-line-formatter)
				     (variable
				      . sf::variable-line-formatter)))

(defvar sf::lines-special-formatters-alist '((name . sf::name-formatter)))

;;(defmacro with-inserted-indention (indention &rest body)
;;  (let ((start (gensym))
;;	(indent (gensym)))
;;    `(let* ((,indent ,indention)
;;	    (,start (point))
;;	    (fill-column (- fill-column (length ,indent))))
;;       (progn ,@body)
;;       (insert-box ,start (point) ,indent))))

;; (defmacro with-inserted-indention (indention &rest body)
;;   (let ((start (gensym))
;; 	(end (gensym))
;; 	(indent (gensym)))
;;     `(let* ((,indent ,indention)
;; 	    (,start (point))
;; 	    (,end)
;; 	    (fill-column (- fill-column (length ,indent))))
;;        (progn ,@body)
;;        (setq ,end (point))
;;        (goto-char ,start)
;;        (while (< (point) ,end)
;; 	 (insert ,indent)
;; 	 (forward-line 1)))))

(defmacro with-inserted-indention (indention &rest body)
  (let ((indent (gensym)))
    `(let* ((,indent ,indention)
	    (fill-column (- fill-column (length ,indent))))
       (save-restriction
	 (narrow-to-region (point) (point))
	 (progn ,@body)
	 (goto-char (point-min))
	 (while (< (point) (point-max))
	   (insert ,indent)
	   (forward-line 1))))))

(defmacro with-inserted-indention-first (first indention &rest body)
  (let ((start (gensym))
	(indent (gensym)))
    `(let* ((,indent ,indention)
	    (,start (point))
	    (fill-column (- fill-column (length ,indent))))
       (save-restriction
	 (narrow-to-region (point) (point))
	 (progn ,@body)
	 (goto-char (point-min))
	 (insert ,first)
;	 (forward-line 1)
;	 (while (not (eobp))
;	   (insert ,indent)
;	   (forward-line 1)
;	   )))))
	 (goto-char (point-max))))))

(defmacro with-point-at-end (&rest body)
  `(progn
     (save-restriction
       (narrow-to-region (point) (point))
       ,@body
       (goto-char (point-max)))))
     
(defmacro with-filled-paragraphs (fill-prefix &rest body)
  (let ((start (gensym "with-filled-paragraphs-")))
  `(progn
     (let ((,start (point)))
       (save-restriction
	 (narrow-to-region ,start ,start)
	 ,@body
	 (setq ,start (point-max)))
       (goto-char ,start)))))

;; (defmacro with-filled-paragraphs (fill-prefix &rest body)
;;   (let ((start (gensym)))
;;     `(let* ((,start (point))
;; 	    (fill-prefix ,fill-prefix))
;; ;;	     (fill-prefix ""))	    
;; 	(save-restriction
;; 	  ,@body
;; 	  (narrow-to-region ,start (point))
;; 	  (goto-char (point-min))
;; ;	  (debug-if (string= field "NOTES"))
;; 	  (while (not (eobp))
;; 	    (fill-paragraph nil)
;; 	    (forward-paragraph 1))))))

(defmacro assoc-value (key list)
  "Return the value associated with KEY in LIST or nil if none exists"
  `(let ((val (assoc ,key ,list)))
    (if val
	(cdr val))))

(defmacro assoc-value-from-list (key list)
  "Return the value associated with KEY in LIST or nil if none exists"
  `(let ((val (assoc ,key ,list)))
    (if val
	(cadr val))))

(defmacro dassert (form string)
  "Debug on this condition"
  `(if ,form
       (progn
	 (message (format "Debug Assert Form %S: %s" ,form string))
	 (debug))))

(defmacro debug-if (form &optional return)
  "Debug only on condition"
  `(if ,form
       (debug)
     ,return))
     

(defun sf::get-list-property (list field property)
  "Get the value of property PROPERTY for field FIELD in LIST.  More
generally, LIST is an alist with members locating another alist of
field properties"
  (let (entry)
    (if (setq entry (assoc field list))
	(if (setq entry (assoc property entry))
	    (cadr entry)))))

(defun sf::get-field-property (list property)
  "Get the value of PROPERTY from LIST."
  (assoc-value-from-list property list))

(declaim (inline sf::get-field-property))

(defun sf::document-object ()
  "Document the program object (function, class, macro, whatever)
immediately following in the buffer" 
  (interactive)
  (let* (start
	 (prefix (header-prefix-string))
	 (theObject (save-excursion (sf::find-program-object)))
	 (thePreviousValues (save-excursion
			     (if (sf::find-header-beginning theObject)
				 (sf::get-fields sf::default-documentation-alist))))
	)
    (sf::update-object theObject thePreviousValues
		       sf::default-documentation-alist)
    (save-object theObject)
    (insert comment-start)
    (fill-line-with-preceding-character)
    (insert "\n")
    (with-inserted-indention
     prefix
     (sf::insert-object-documentation theObject nil
				 sf::default-documentation-alist
				 sf::documentation-order-list))
    (insert comment-end "\n")
    ))


(defcmethod sf::find-header-beginning ((object sf::function-base))
  "Find the function header for the given function"
  (declare (special prefix))
  (if
      (re-search-backward (concat (regexp-quote prefix)
			       (sf::function-base-name object)
			       "\\(()\\)?"
			       " -- ") nil 't)
      (if (progn
	    (forward-line -1)
	    (looking-at (regexp-quote (trim-whitespace comment-start))))
	  't)))
      

(defun sf::insert-object-documentation (object &optional
					       values
					       defaults
					       order)
  "Insert documentation for the object into the buffer.  Previous
values found are in VALUES, default values (and other properties) are
in DEFAULTS and the order in which fields should be inserted are in
ORDER"
  (setq defaults (or defaults sf::default-documentation-alist))
  (setq order (or order sf::documentation-order-list))
  (let ((fields (sf::get-object-order object order)))
    (mapcar #'(lambda (field)
		(let ((value (sf::get-aliased-field-association field values))
		      (default
;;;			(sf::get-aliased-field-association field
;;;							   defaults)))
			(assoc-value field defaults)))
		  (sf::insert-field object field value default)))
	    fields)))

(defun sf::get-aliased-field-association (field alist)
  "Get association for FIELD in ALIST, handling aliases correctly"
  (let* (temp
	 (pair (assoc field alist))
	 (aliases (unless pair
		    (loop for x in sf::field-aliases
			  if (string= (cdr x) field)
			  collect (cdr x)))))
    (if pair
	(cdr pair)
      ;; we have an ordered list to check
      (loop for x in aliases
	    if (setq temp (assoc x alist))
	    return (cdr temp)))))

(defun sf::insert-field (object field value default)
  "Insert field FIELD for OBJECT that had previous value VALUE and
default (and properties) DEFAULT"
  (let* ((new-value (sf::get-defaulted-value object field value default))
	 (formatter (sf::find-formatter object field new-value
					default)))
    (if formatter
	 (funcall formatter object field new-value default)
      (error "Formatter is nil for %s" field))
    ))

(defun sf::get-defaulted-value (object field value default)
  "Handle getting value with appropriate defaults"
  (let ((default-function (sf::get-field-property default  'default-function ))
	(value (sf::get-new-value object field value default)))
    (or value
	(and default-function (funcall default-function))
	(assoc field value)
	(and (sf::get-field-property default 'default-value)
	     (sf::get-field-property default 'default-value)))))
    
;;   (defcmethod print-object ((obj standard-object))

(defun sf::single-line-formatter (object field value default)
  "Insert FIELD, VALUE into current buffer on a single line"
  (if (or value
	  (sf::include-if-blank-p default))
      (insert (format "%s:  %s\n" field (or value "")))))

(defun sf::multiple-line-formatter (object field value default)
  "Insert FIELD, VALUE into current buffer on multiple lines"
  (if (or
       value
       (sf::include-if-blank-p default))
      (progn
	(insert field "\n")
	(with-inserted-indention "  "
	(with-filled-paragraphs
	 ""
	 (insert value "\n"))))))

(defun sf::variable-line-formatter (object field value default)
  "Insert FIELD, VALUE on one line if value is nil or on multiple
lines if it's real"
  (let ((default-value (sf::get-field-property default 'default-value)))
    (if (or (not value)
	    (and default-value (not (string= value default-value))))
	(sf::multiple-line-formatter object field value default)
      (sf::single-line-formatter object field value default))))

(defun sf::is-multiple-lines (string)
  "Return 't if string has multiple lines"
  (string-match "\n." string))

(defun sf::include-if-blank-p (alist)
  "Check for include-if-blank 't association"
  (sf::get-field-property alist 'include-if-blank))

(defcmethod sf::find-formatter ((object sf::program-object) field
				value default)
  "Return the appropriate formatter to use for each field"
  (let* ((line (sf::get-field-property default 'lines))
	 (formatter-pair (assoc line sf::lines-formatting-alist)))
    (cdr formatter-pair)))

(defcmethod sf::find-formatter ((object sf::function) Field value
				default)
  "Handle the arguments case"
  (if (string= field "ARGUMENTS")
      #'sf::function-argument-formatter
    (call-next-method)))


(defun sf::function-argument-formatter (object field value default)
  "Put in appropriate arguments.  If value is non-nil, parse it"
  (let ((args (sf::function-base-args object)))
    (if (not args)
	(insert (format "%s: NONE\n" field))
      (insert (format "%s\n" field))
      (with-inserted-indention
       "  "
       (mapcar #'(lambda (x)
		   (sf::insert-argument x (assoc-value (sf::argument-name x)
						 value) default))
	       args)))))

(defun sf::insert-argument (arg value options)
  "Insert arg in buffer with value if value exists"
  (let* ((arg-prefix (format "%s (%s) -- " (sf::argument-name arg)
			     (sf::argument-type arg)))
	 (fill-prefix (make-string (length arg-prefix) 32))
	 (value (or (sf::argument-documentation arg) value)))
    (with-inserted-indention-first
     arg-prefix (make-string (length arg-prefix) 32)
     (with-filled-paragraphs
      ""
      (if value
	  (progn
	    (insert value "\n\n")))))))

(defcmethod sf::special-line-formatter ((object sf::function-base)
					field value default)
  "Do special line formatting for functions"
  (cond
   ((eq field 'name)
    (insert
     (format "%s() -- %s\n" (sf::function-base-name object) (or value ""))))
   ('t
    (error "Don't know how to format field \"%s\" for an %s"
	   field
	   (cclass-name (cclass-of object))))))


(defcmethod sf::special-line-formatter ((object sf::class) field value
					default)
  "Do special line formatting for functions"
  (cond
   ((eq field 'name)
    (insert
     (format "class %s -- %s\n" (sf::function-base-name object) (or value ""))))
   ('t
    (error "Don't know how to format field \"%s\" for an %s"
	   field
	   (cclass-name (cclass-of object))))))


(defcmethod sf::get-new-value ((object sf::program-object) field value
			       default)
  "Get the new value for a field"
  (or
   (let ((key (sf::get-field-property default 'documentation-key)))
     (if key
	 (sf::program-object-get-documentation object key)))
   value
   (sf::get-field-property default 'default-value)))

(defcmethod sf::get-new-value :around ((object sf::function-base) field value
			       default)
  "Get the new value for field"
  (cond
   ((string= field "ARGUMENTS")
    (sf::functions-format-arguments object value default))
   ((string= field "RETURNS")
    (sf::functions-get-returns object value default))
   ('t
    (call-next-method))))


(defcmethod sf::get-new-value :around ((object sf::member-function)
				       field value default)
  (if (string= field "VISIBILITY")
      (sf::member-function-visibility object)))


(defun sf::program-object-get-documentation  (ojbect key)
  "Get documentation for KEY from OBJECT"
  (let* ((all-docs (sf::program-object-documentation object))
	 (key-pair (if all-docs
		       (assoc key all-docs))))
    (if key-pair
	(cdr key-pair))))


(defun sf::get-object-order (object order)
  "Return the appropriate fields in the appropriate order for OBJECT,
give ORDER"
  order)

(defcmethod sf::update-object ((object sf::program-object) values
			       defaults &optional key)
  "Update the OBJECT from the fields given in VALUES.  VALUES should
have already had defaults removed."
  (mapcar #'(lambda (x)
	      (let ((real-field (sf::resolve-field-aliases (car x))))
		(sf::update-object-field object
					 real-field
					 (cdr x)
					 (assoc-value real-field defaults))))
	  values))



(defcmethod sf::update-object ((object sf::argument) value defaults
			       &optional key)
  "Update argument from value"
  (setf (sf::argument-documentation object) value))

(defun sf::resolve-field-aliases (field)
  "Return the field alias"
  (or (assoc-value field sf::field-aliases)
      field))

(defcmethod sf::update-object-field ((object sf::function-base) name value defaults)
  "Update the OBJECT from the FIELD.  FIELD is a list of (field-name
value)."
;  (debug-if (string= name "PRIVATE NOTES"))
  (let ((map (assoc 'map defaults)))
    (if (stringp name)
	(cond
	 ((and map
	       (setq map (sf::find-correct-map object map)))
	  (sf::update-object-from-map object map name value defaults))
	 ((string= name "RETURNS")
	  (call-next-method object name (sf::parse-returns value) defaults))
	 ('t
	  (call-next-method)))
      (call-next-method))))

(defcmethod sf::update-object-field ((object sf::program-object) name value
				     defaults)
  "Update the OBJECT from the FIELD."
;  (debug-if (string= name "PRIVATE NOTES"))
  (let* ((key (sf::get-field-property defaults 'documentation-key)))
    (if key
	(sf::update-doc-key object name key value defaults))))

(defun sf::find-correct-map (object map)
  "Find the correct map for object in list map and return it, or
return nil.  The correct map is denoted by (typep object (car map))
being true."
  (loop for x in map
	if (listp x)
	if (typep object (car x))
	return x))


(defun sf::update-object-from-map (object map name value defaults)
  "Update an object from the give map."
  (let ((list-function (nth 1 map))
	(doc-key (nth 2 map))
	(vals (sf::functions-format-arguments object value defaults))
	)
    (mapcar #'(lambda (x)
		(sf::update-object x (assoc-value (sf::program-object-name
					     x) vals) defaults
					     doc-key))
	    (funcall list-function object))))

(defun sf::parse-returns (value)
  "Parse value to remove only description"
  (in-temp-buffer
   (insert value)
   (goto-char (point-min))
   (search-forward "-- ")
   (sf::remove-header-prefix
    (buffer-substring-no-properties (point) (point-max))
    (regexp-quote (header-prefix-string)))))


(defun sf::update-arguments (object value defaults)
  "Update the function's arguments"
  (let ((args (sf::functions-format-arguments object value defaults)))
    (mapcar #'(lambda (arg)
		(sf::update-argument arg args))
	    (sf::function-base-args object))))

(defun sf::update-argument (argument alist)
  "Update documentation for ARGUMENT getting documentation from ALIST"
  (let ((my-docs (assoc (sf::argument-name argument) alist)))
    (if my-docs
	(setf (sf::argument-documentation argument) (cdr my-docs)))))



(defun sf::update-doc-key (object field key value defaults)
  "Update the documentation given by key"
  (dassert (listp key) "Key is a list")
  (setf (sf::program-object-documentation object)
	(progn
	 (remove-duplicates
	  (acons key value
		 (sf::program-object-documentation object))
	  :test #'(lambda (x y) (eq (car x) (car y)))
	  :from-end 't))))

(defmacro with-each-argument-narrowed (string &rest body)
  (let ((start (gensym))
	(end (gensym)))
    `(let (,start ,end)
       (in-temp-buffer
	(insert ,string)
	(goto-char (point-min))
	(while (re-search-forward sf::argument-name-regexp nil 't)
	  (beginning-of-line)
	  (setq ,start (point))
	  (end-of-line)
	  (if (re-search-forward sf::argument-name-regexp nil 't)
	      (beginning-of-line)
	    (goto-char (point-max)))
	  (setq ,end (point))
	  (narrow-to-region ,start ,end)
	  (goto-char (point-min)) 
	  (progn
	    ,@body)
	  (widen)
	  (goto-char ,end))))))

(defun sf::functions-format-arguments (object value default)
  "Return an association list of argument name, value pairs"
  (if value
      (let ((values))
	(with-each-argument-narrowed
	 value
	 (push (sf::parse-return-or-argument) values))
	values)))

(defvar sf::argument-name-regexp "^[ \t]*\\(\\(\\s_\\|\\sw\\)*\\)?[ \t]*([^)]*) -- "
  "Regexp to use to match argument documentation")

(defun sf::functions-get-returns (object value default)
  "Get the returns description"
  (let* ((key (sf::get-field-property default 'documentation-key))
	 (docs (if key
		   (sf::program-object-get-documentation object key)))
	 (rettype (sf::function-base-return-type object)))
    (concat
     (if rettype
	 (format "(%s)"  rettype))
     " -- "
     (or docs value ""))))
  


(defun sf::parse-return-or-argument ()
  "Parse an argument"
  (goto-char (point-min))
  (re-search-forward sf::argument-name-regexp)
  (cons (match-buffer-substring-no-properties 1)
	(buffer-substring-no-properties (point) (save-excursion
						  (goto-char (point-max))
						  (skip-chars-backward
						   " \t\n")
						  (point)))))