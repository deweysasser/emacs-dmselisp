;;; elprof.el --- Emacs Lisp Profiler as modified by Dewey Sasser

;; Copyright (C) 1997 Dewey M. Sasser
;; Copyright (C) 1994 Free Software Foundation, Inc.

;; Author: Dewey M. Sasser <dewey@newvision.com>
;; Created:       26-Feb-1994
;; Version:       1.0
;; Last Modified: 1994/12/28 22:39:31
;; Keywords:      Emacs Lisp Profile Timing

;; This file is NOT part of GNU Emacs.

;; This file used to be part of GNU emacs, but has been modified by
;; Dewey M. Sasser. 

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; If you want to profile a bunch of functions, set elprof-function-list
;; to the list of symbols, then do a M-x elprof-instrument-list.  This
;; hacks those functions so that profiling information is recorded
;; whenever they are called.  To print out the current results, use
;; M-x elprof-results.  With elprof-reset-after-results set to non-nil,
;; profiling information will be reset whenever the results are
;; displayed.  You can also reset all profiling info at any time with
;; M-x elprof-reset-all.
;;
;; You can also instrument all functions in a package, provided that
;; the package follows the GNU coding standard of a common textural
;; prefix.  Use M-x elprof-instrument-package for this.
;;
;; If you want to sort the results, set elprof-sort-by-function to some
;; predicate function.  The three most obvious choices are predefined:
;; elprof-sort-by-call-count, elprof-sort-by-average-time, and
;; elprof-sort-by-total-time.  Also, you can prune from the output, all
;; functions that have been called fewer than a given number of times
;; by setting elprof-report-limit.
;;
;; Elp can instrument byte-compiled functions just as easily as
;; interpreted functions, but it cannot instrument macros.  However,
;; when you redefine a function (e.g. with eval-defun), you'll need to
;; re-instrument it with M-x elprof-instrument-function.  This will also
;; reset profiling information for that function.  Elp can handle
;; interactive functions (i.e. commands), but of course any time spent
;; idling for user prompts will show up in the timing results.
;;
;; You can also designate a `master' function.  Profiling times will
;; be gathered for instrumented functions only during execution of
;; this master function.  Thus, if you have some defuns like:
;;
;;  (defun foo () (do-something-time-intensive))
;;  (defun bar () (foo))
;;  (defun baz () (bar) (foo))
;;
;; and you want to find out the amount of time spent in bar and foo,
;; but only during execution of bar, make bar the master.  The call of
;; foo from baz will not add to foo's total timing sums.  Use M-x
;; elprof-set-master and M-x elprof-unset-master to utilize this feature.
;; Only one master function can be set at a time.

;; You can restore any function's original function definition with
;; elprof-restore-function.  The other instrument, restore, and reset
;; functions are provided for symmetry.

;; Note that there are plenty of factors that could make the times
;; reported unreliable, including the accuracy and granularity of your
;; system clock, and the overhead spent in lisp calculating and
;; recording the intervals.  The latter I figure is pretty constant
;; so, while the times may not be entirely accurate, I think they'll
;; give you a good feel for the relative amount of work spent in the
;; various lisp routines you are profiling.  Note further that times
;; are calculated using wall-clock time, so other system load will
;; affect accuracy too.

;; Here is a list of variable you can use to customize elp:
;;   elprof-function-list
;;   elprof-reset-after-results
;;   elprof-sort-by-function
;;   elprof-report-limit
;;
;; Here is a list of the interactive commands you can use:
;;   elprof-instrument-function
;;   elprof-restore-function
;;   elprof-instrument-list
;;   elprof-restore-list
;;   elprof-instrument-package
;;   elprof-restore-all
;;   elprof-reset-function
;;   elprof-reset-list
;;   elprof-reset-all
;;   elprof-set-master
;;   elprof-unset-master
;;   elprof-results
;;   elprof-submit-bug-report

;; Note that there are plenty of factors that could make the times
;; reported unreliable, including the accuracy and granularity of your
;; system clock, and the overhead spent in lisp calculating and
;; recording the intervals.  I figure the latter is pretty constant,
;; so while the times may not be entirely accurate, I think they'll
;; give you a good feel for the relative amount of work spent in the
;; various lisp routines you are profiling.  Note further that times
;; are calculated using wall-clock time, so other system load will
;; affect accuracy too.  You cannot profile anything longer than ~18
;; hours since I throw away the most significant 16 bits of seconds
;; returned by current-time: 2^16 == 65536 seconds == ~1092 minutes ==
;; ~18 hours.  I doubt you will ever want to profile stuff on the
;; order of 18 hours anyway.

;;; Background:

;; This program is based on the only two existing Emacs Lisp profilers
;; that I'm aware of, Boaz Ben-Zvi's profile.el, and Root Boy Jim's
;; profiler.el. Both were written for Emacs 18 and both were pretty
;; good first shots at profiling, but I found that they didn't provide
;; the functionality or interface that I wanted.  So I wrote this.
;; I've tested elp in GNU Emacs 19 and in GNU XEmacs.  There's no
;; point in even trying to make this work with Emacs 18.

;; Unlike previous profilers, elp uses Emacs 19's built-in function
;; current-time to return interval times.  This obviates the need for
;; both an external C program and Emacs processes to communicate with
;; such a program, and thus simplifies the package as a whole.

;;; Code:


;; start user configuration variables
;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

(defvar elprof-hierarchal-recursion-level 25
  "*Level to which to print call tree")

(defvar elprof-results nil
  "Results list of running functions.
Used by elprof-display-output.")

(defvar elprof-function-list nil
  "*List of function to profile.")

(defvar elprof-reset-after-results t
  "*Non-nil means reset all profiling info after results are displayed.
Results are displayed with the `elprof-results' command.")

(defvar elprof-sort-by-function nil
  "*Non-nil specifies elp results sorting function.
These functions are currently available:

  elprof-sort-by-call-count   -- sort by the highest call count
  elprof-sort-by-total-time   -- sort by the highest total time
  elprof-sort-by-average-time -- sort by the highest average times

You can write you're own sort function. It should adhere to the
interface specified by the PRED argument for the `sort' defun.  Each
\"element of LIST\" is really a 4 element vector where element 0 is
the call count, element 1 is the total time spent in the function,
element 2 is the average time spent in the function, and element 3 is
the symbol's name string.")

(defvar elprof-report-limit nil
  "*Prevents some functions from being displayed in the results buffer.
If a number, no function that has been called fewer than that number
of times will be displayed in the output buffer.  If nil, all
functions will be displayed.")


;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; end user configuration variables


(defconst elprof-version "1.0"
  "ELPROF version number.")

(defconst elprof-help-address "dewey@newvision.com"
  "Address accepting submissions of bug reports and questions.")

(defvar elprof-hierarchal-results-buffer "*ELPROF Hierarchal Profiling Results*"
  "*Buffer name for outputting hierarchal profiling results")

(defvar elprof-results-buffer "*ELPROF Profiling Results*"
  "Buffer name for outputting profiling results.")

(defconst elprof-timer-info-property 'elprof-info
  "ELP information property name.")

(defvar elprof-all-instrumented-list nil
  "List of all functions currently being instrumented.")

(defvar elprof-record-p t
  "Controls whether functions should record times or not.
This variable is set by the master function.")

(defvar elprof-master nil
  "Master function symbol.")


;;;###autoload
(defun elprof-instrument-function (funsym)
  "Instrument FUNSYM for profiling.
FUNSYM must be a symbol of a defined function."
  (interactive "aFunction to instrument: ")
  ;; TBD what should we do if the function is already instrumented???
  (let* ((funguts (symbol-function funsym))
	 (infovec (vector 0 0 funguts))
	 (newguts '(lambda (&rest args))))
    ;; we cannot profile macros
    (and (eq (car-safe funguts) 'macro)
	 (error "ELP cannot profile macro %s" funsym))
    ;; put rest of newguts together
    (if (commandp funsym)
	(setq newguts (append newguts '((interactive)))))
    (setq newguts (append newguts (list
				   (list 'elprof-wrapper
					 (list 'quote funsym)
					 (list 'and
					       '(interactive-p)
					       (not (not (commandp funsym))))
					 'args))))
    ;; to record profiling times, we set the symbol's function
    ;; definition so that it runs the elprof-wrapper function with the
    ;; function symbol as an argument.  We place the old function
    ;; definition on the info vector.
    ;;
    ;; The info vector data structure is a 3 element vector.  The 0th
    ;; element is the call-count, i.e. the total number of times this
    ;; function has been entered.  This value is bumped up on entry to
    ;; the function so that non-local exists are still recorded. TBD:
    ;; I haven't tested non-local exits at all, so no guarantees.
    ;;
    ;; The 1st element is the total amount of time in usecs that have
    ;; been spent inside this function.  This number is added to on
    ;; function exit.
    ;;
    ;; The 2nd element is the old function definition list.  This gets
    ;; funcall'd in between start/end time retrievals. I believe that
    ;; this lets us profile even byte-compiled functions.

    ;; put the info vector on the property list
    (put funsym elprof-timer-info-property infovec)

    ;; set the symbol's new profiling function definition to run
    ;; elprof-wrapper
    (fset funsym newguts)

    ;; add this function to the instrumentation list
    (or (memq funsym elprof-all-instrumented-list)
	(setq elprof-all-instrumented-list
	      (cons funsym elprof-all-instrumented-list)))
    ))

;;;###autoload
(defun elprof-restore-function (funsym)
  "Restore an instrumented function to its original definition.
Argument FUNSYM is the symbol of a defined function."
  (interactive "aFunction to restore: ")
  (let ((info (get funsym elprof-timer-info-property)))
    ;; delete the function from the all instrumented list
    (setq elprof-all-instrumented-list
	  (delq funsym elprof-all-instrumented-list))

    ;; if the function was the master, reset the master
    (if (eq funsym elprof-master)
	(setq elprof-master nil
	      elprof-record-p t))

    ;; zap the properties
    (put funsym elprof-timer-info-property nil)

    ;; restore the original function definition, but if the function
    ;; wasn't instrumented do nothing.  we do this after the above
    ;; because its possible the function got un-instrumented due to
    ;; circumstances beyond our control.  Also, check to make sure
    ;; that the current function symbol points to elprof-wrapper.  If
    ;; not, then the user probably did an eval-defun while the
    ;; function was instrumented and we don't want to destroy the new
    ;; definition.
    (and info
	 (assq 'elprof-wrapper (symbol-function funsym))
	 (fset funsym (aref info 2)))))

;;;###autoload
(defun elprof-instrument-list (&optional list)
  "Instrument for profiling, all functions in `elprof-function-list'.
Use optional LIST if provided instead."
  (interactive "PList of functions to instrument: ")
  (let ((list (or list elprof-function-list)))
    (mapcar 'elprof-instrument-function list)))

;;;###autoload
(defun elprof-instrument-package (prefix)
  "Instrument for profiling, all functions which start with PREFIX.
For example, to instrument all ELP functions, do the following:

    \\[elprof-instrument-package] RET elprof- RET"
  (interactive "sPrefix of package to instrument: ")
  (elprof-instrument-list
   (mapcar 'intern (all-completions prefix obarray
				    (function
				     (lambda (sym)
				       (and (fboundp sym)
					    (not (memq (car-safe
							(symbol-function sym))
						       '(macro keymap autoload))))))))))

(defun elprof-restore-list (&optional list)
  "Restore the original definitions for all functions in `elprof-function-list'.
Use optional LIST if provided instead."
  (interactive "PList of functions to restore: ")
  (let ((list (or list elprof-function-list)))
    (mapcar 'elprof-restore-function list)))

(defun elprof-restore-all ()
  "Restores the original definitions of all functions being profiled."
  (interactive)
  (elprof-restore-list elprof-all-instrumented-list))


(defun elprof-reset-function (funsym)
  "Reset the profiling information for FUNSYM."
  (interactive "aFunction to reset: ")
  (let ((info (get funsym elprof-timer-info-property)))
    (or info
	(error "%s is not instrumented for profiling." funsym))
    (aset info 0 0)			;reset call counter
    (aset info 1 0.0)			;reset total time
    ;; don't muck with aref 2 as that is the old symbol definition
    ))

(defun elprof-reset-list (&optional list)
  "Reset the profiling information for all functions in `elprof-function-list'.
Use optional LIST if provided instead."
  (interactive "PList of functions to reset: ")
  (let ((list (or list elprof-function-list)))
    (mapcar 'elprof-reset-function list)))

(defun elprof-reset-all ()
  "Reset the profiling information for all functions being profiled."
  (interactive)
  (elprof-reset-list elprof-all-instrumented-list))

(defun elprof-set-master (funsym)
  "Set the master function for profiling."
  (interactive "aMaster function: ")
  ;; when there's a master function, recording is turned off by
  ;; default
  (setq elprof-master funsym
	elprof-record-p nil)
  ;; make sure master function is instrumented
  (or (memq funsym elprof-all-instrumented-list)
      (elprof-instrument-function funsym)))

(defun elprof-unset-master ()
  "Unsets the master function."
  (interactive)
  ;; when there's no master function, recording is turned on by default.
  (setq elprof-master nil
	elprof-record-p t))


(defsubst elprof-get-time ()
  ;; get current time in seconds and microseconds. I throw away the
  ;; most significant 16 bits of seconds since I doubt we'll ever want
  ;; to profile lisp on the order of 18 hours. See notes at top of file.
  (let ((now (current-time)))
    (+ (float (nth 1 now)) (/ (float (nth 2 now)) 1000000.0))))

(defun elprof-wrapper (funsym interactive-p args)
  "This function has been instrumented for profiling by the ELP.
ELP is the Emacs Lisp Profiler.  To restore the function to its
original definition, use \\[elprof-restore-function] or \\[elprof-restore-all]."
  ;; turn on recording if this is the master function
  (let ((elprof-record-p
	 (or (and elprof-master
		  (eq funsym elprof-master))
	     elprof-record-p)))
  ;; get info vector and original function symbol
  (let* ((info (get funsym elprof-timer-info-property))
	 (func (aref info 2))
	 result)
    (or func
	(error "%s is not instrumented for profiling." funsym))
    (if (not elprof-record-p)
	;; when not recording, just call the original function symbol
	;; and return the results.
	(setq result
	      (if interactive-p
		  (call-interactively func)
		(apply func args)))
      ;; we are recording times
      (let ((temp-results))
	(let ((elprof-results nil)
	      (enter-time (elprof-get-time)))
	  ;; increment the call-counter
	  (aset info 0 (1+ (aref info 0)))
	  ;; now call the old symbol function, checking to see if it
	  ;; should be called interactively.  make sure we return the
	  ;; correct value
	  (setq result
		(if interactive-p
		    (call-interactively func)
		  (apply func args)))
	  ;; calculate total time in function
	  (aset info 1 (+ (aref info 1) (- (elprof-get-time) enter-time)))
	  (setq temp-results (list funsym enter-time (elprof-get-time) elprof-results))
	  )
	(push temp-results elprof-results)
	))
    ;; turn off recording if this is the master function
    (if (and elprof-master
	     (eq funsym elprof-master))
	(setq elprof-record-p nil))
    result)))



;; shut the byte-compiler up
(defvar elprof-field-len nil)
(defvar elprof-cc-len nil)
(defvar elprof-at-len nil)
(defvar elprof-et-len nil)

(defun elprof-sort-by-call-count (vec1 vec2)
  ;; sort by highest call count.  See `sort'.
  (>= (aref vec1 0) (aref vec2 0)))

(defun elprof-sort-by-total-time (vec1 vec2)
  ;; sort by highest total time spent in function. See `sort'.
  (>= (aref vec1 1) (aref vec2 1)))

(defun elprof-sort-by-average-time (vec1 vec2)
  ;; sort by highest average time spent in function. See `sort'.
  (>= (aref vec1 2) (aref vec2 2)))

(defsubst elprof-pack-number (number width)
  ;; pack the NUMBER string into WIDTH characters, watching out for
  ;; very small or large numbers
  (if (<= (length number) width)
      number
    ;; check for very large or small numbers
    (if (string-match "^\\(.*\\)\\(e[+-].*\\)$" number)
	(concat (substring
		 (substring number (match-beginning 1) (match-end 1))
		 0
		 (- width (match-end 2) (- (match-beginning 2)) 3))
		"..."
		(substring number (match-beginning 2) (match-end 2)))
      (concat (substring number 0 width)))))

(defun elprof-output-result (resultvec)
  ;; output the RESULTVEC into the results buffer. RESULTVEC is a 4 or
  ;; more element vector where aref 0 is the call count, aref 1 is the
  ;; total time spent in the function, aref 2 is the average time
  ;; spent in the function, and aref 3 is the symbol's string
  ;; name. All other elements in the vector are ignored.
  (let* ((cc (aref resultvec 0))
	 (tt (aref resultvec 1))
	 (at (aref resultvec 2))
	 (symname (aref resultvec 3))
	 callcnt totaltime avetime)
    (setq callcnt (number-to-string cc)
	  totaltime (number-to-string tt)
	  avetime (number-to-string at))
    ;; possibly prune the results
    (if (and elprof-report-limit
	     (numberp elprof-report-limit)
	     (< cc elprof-report-limit))
	nil
      (insert symname)
      (insert-char 32 (+ elprof-field-len (- (length symname)) 2))
      ;; print stuff out, formatting it nicely
      (insert callcnt)
      (insert-char 32 (+ elprof-cc-len (- (length callcnt)) 2))
      (let ((ttstr (elprof-pack-number totaltime elprof-et-len))
	    (atstr (elprof-pack-number avetime elprof-at-len)))
	(insert ttstr)
	(insert-char 32 (+ elprof-et-len (- (length ttstr)) 2))
	(insert atstr))
      (insert "\n"))))

;;;###autoload
(defun elprof-results ()
  "Display current profiling results.
If `elprof-reset-after-results' is non-nil, then current profiling
information for all instrumented functions are reset after results are
displayed."
  (interactive)
  (let ((curbuf (current-buffer))
	(resultsbuf (get-buffer-create elprof-results-buffer)))
    (set-buffer resultsbuf)
    (erase-buffer)
    (beginning-of-buffer)
    ;; get the length of the longest function name being profiled
    (let* ((longest 0)
	   (title "Function Name")
	   (titlelen (length title))
	   (elprof-field-len titlelen)
	   (cc-header "Call Count")
	   (elprof-cc-len    (length cc-header))
	   (et-header "Elapsed Time")
	   (elprof-et-len    (length et-header))
	   (at-header "Average Time")
	   (elprof-at-len    (length at-header))
	   (resvec
	    (mapcar
	     (function
	      (lambda (funsym)
		(let* ((info (get funsym elprof-timer-info-property))
		       (symname (format "%s" funsym))
		       (cc (aref info 0))
		       (tt (aref info 1)))
		  (if (not info)
		      (insert "No profiling information found for: "
			      symname)
		    (setq longest (max longest (length symname)))
		    (vector cc tt (if (zerop cc)
				      0.0 ;avoid arithmetic div-by-zero errors
				    (/ (float tt) (float cc)))
			    symname)))))
	     elprof-all-instrumented-list))
	   )				; end let*
      (insert title)
      (if (> longest titlelen)
	  (progn
	    (insert-char 32 (- longest titlelen))
	    (setq elprof-field-len longest)))
      (insert "  " cc-header "  " et-header "  " at-header "\n")
      (insert-char ?= elprof-field-len)
      (insert "  ")
      (insert-char ?= elprof-cc-len)
      (insert "  ")
      (insert-char ?= elprof-et-len)
      (insert "  ")
      (insert-char ?= elprof-at-len)
      (insert "\n")
      ;; if sorting is enabled, then sort the results list. in either
      ;; case, call elprof-output-result to output the result in the
      ;; buffer
      (if elprof-sort-by-function
	  (setq resvec (sort resvec elprof-sort-by-function)))
      (mapcar 'elprof-output-result resvec))
    ;; now pop up results buffer
    (set-buffer curbuf)
    (pop-to-buffer resultsbuf)
    ;; reset profiling info if desired
    (and elprof-reset-after-results
	 (elprof-reset-all))))


(eval-when-compile
 (require 'reporter))

;;;###autoload
(defun elprof-submit-bug-report ()
  "Submit via mail, a bug report on elp."
  (interactive)
  (and
   (y-or-n-p "Do you want to submit a report on elp? ")
   (require 'reporter)
   (reporter-submit-bug-report
    elprof-help-address (concat "elp " elprof-version)
    '(elprof-report-limit
      elprof-reset-after-results
      elprof-sort-by-function))))


(provide 'elprof)

(defmacro elprof-with-output-to-hierarchal-viewing-buffer (name &rest
								body)
  "Set things up so that all output goes to the hierarchal viewing
buffer"
  (let ((new-buf (gensym)))
    `(let ((,new-buf (get-buffer ,name)))
       (unless ,new-buf (setq ,new-buf (generate-new-buffer ,name)))
       (in-buffer ,new-buf
		  (setq buffer-read-only nil)
		  (delete-region (point-min) (point-max))
		  (elprof-hierarchal-setup-viewing-mode))
       (let ((standard-output ,new-buf))
	 ,@body)
       (in-buffer ,new-buf
		  (setq buffer-read-only 't)
		  (set-buffer-modified-p nil)
		  (goto-char (point-min)))
       (switch-to-buffer-other-window ,new-buf))))

    
(defun elprof-hierarchal-results (&optional results)
  "Display the results output"
  (interactive)
  (setq results (or results elprof-results))
  (if results
      (let ((functions (make-hash-table))
	    (this-scope (make-hash-table))
	    this-specific-scope
	    call-stack
	    fields
	    )
;	(setq this-scope functions)
	(setq this-specific-scope this-scope)
	(mapcar #'calculate-output-internal results)
	(setq fields
	      (list (min
		     48
		     (+ elprof-hierarchal-recursion-level
		       (apply 'max (loop for x being the hash-keys of functions
					 collect (length (symbol-name x))))))
		    11
		    11
		    11
		    ))
	(elprof-with-output-to-hierarchal-viewing-buffer
	 elprof-hierarchal-results-buffer
	  (print-in-fields fields
			   "Name"
			   "Calls"
			   "Total"
			   "Average")
	  (print-in-fields fields
			   "=================================================="
			   "=============="
			   "=============="
			   "==============")
	  (princ "Hierarchy\n")
	  (print-functions this-scope)
	  (princ "\n\nDirect Calls\n")
	  (print-functions functions)
	  (if elprof-reset-after-results
	      (setq elprof-results nil))))))

(defun print-functions (function-hashtab &optional max-level recursion-level)
  "Print functions in hashtab, recursively callint itself on the
functions hashtabs"
  (declare (special fields))
  (setq recursion-level (or recursion-level 0)
	max-level (or max-level elprof-hierarchal-recursion-level))
  (let (fns
	(insert (make-string (1+ recursion-level) 32)))
    (maphash #'(lambda (key value) (push value fns))
	     function-hashtab)
    (setq fns (sort fns 'compare-by-total-time))
    (mapc #'(lambda (record)
	      (print-in-fields fields
			       (concat insert (symbol-name (aref record 0)))
			       (number-to-string (aref record 1))
			       (number-to-string (aref record 2))
			       (number-to-string
				(if (not (eql (aref record 1) 0))
				    (/ (float (aref record 2))
				       (aref record 1))
				  0))
			       )
	      (if (<= recursion-level max-level)
		  (print-functions (aref record 3) max-level (1+ recursion-level))))
	  fns)))


(defun print-in-fields (fields &rest args)
  "Print things in fields"
  (mapc #'(lambda (len str)
	    (let ((length (length str)))
	      (if (> length (1- len))
		    (setq str (substring str 0 (1- len))
			  length (1- len)))
	      (princ (concat str (make-string (- len length) 32)))))
	fields args)
  (princ "\n"))

(defmacro in-scope-of (fnname &rest body)
  "Execute BODY with scope set to that of FNNAME"
  `(let ((this-scope-record (find-scope-record ,fnname))
	 (this-scope (find-scope ,fnname))
	 (this-specific-scope (find-specific-scope ,fnname)))
     ,@body))

(defmacro with-name-on-stack (name stack &rest body)
  "Execute BODY with NAME pushed on STACK, maintaining stack discipline on exit"
  `(let ((,stack (cons ,name ,stack)))
     ,@body))

(defun calculate-output-internal (function)
  "Calculate output to display"
  (declare (special call-stack))
  (let ((fnname (nth 0 function))
	(start (nth 1 function))
	(end (nth 2 function))
	(calls (nth 3 function)))
    ;;    (debug-if (eq fnname 'sf::insert-object-documentation))
    ;;    (debug-if (eq fnname 'sf::insert-field))
    (unless (member fnname call-stack)
      (with-name-on-stack
       fnname call-stack
       (add-record fnname (- end start))
       (in-scope-of fnname
		    (mapcar #'calculate-output-internal calls)))
      )))

(defun add-record (function time)
  "Add a record to the global list and to the local list"
  (declare (special functions this-scope this-specific-scope))
  (let ((global (find-record function functions))
	(local (find-record function this-scope))
	(specific (find-record function this-specific-scope)))
    (add-to-scope function time global)
    (unless (eq this-scope functions)
      (add-to-scope function time local))
    (unless (or (eq this-specific-scope function)
		(eq this-specific-scope this-scope))
      (add-to-scope function time specific))
    ))

(defun find-record (function place)
  (let ((record (gethash function place)))
    (if (not record)
	(setq record (setf (gethash function place)
			   (vector function 0 0 (make-hash-table)))))
    record))
  

(defun add-to-scope (function time scope)
  "Add a function to the current scope"
  (incf (aref scope 1))
  (incf (aref scope 2) time))


(defun find-scope (function)
  "Find the scope for FUNCTION"
  (declare (special functions))
  (aref (gethash function functions) 3))

(defun find-specific-scope (function)
  "Find the scope for FUNCTION"
  (declare (special this-specific-scope))
  (aref (gethash function this-specific-scope) 3))

(defun find-scope-record (function)
  "Find the scope record for FUNCTION"
  (declare (special functions))
  (gethash function functions))

(defun compare-by-total-time (rec1 rec2)
  "compare records by total time"
  (< (aref rec2 2)
     (aref rec1 2)))

(defun elprof-hierarchal-setup-viewing-mode ()
  "Set up the buffer for viewing"
  (outline-mode)
  (make-local-variable 'outline-regexp)
  (setq outline-regexp " *"))

;; elprof.el ends here




