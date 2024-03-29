;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  align-regexp.el[c]
;;  Copyright (C) 1993-1994, Steve Koren    (koren@fc.hp.com)
;;
;;  Version 1.1
;;
;;  In accordance with the GNU license, this file may be freely
;;  distributed and copied, provided that further distribution is not
;;  restricted.
;;
;;  There is no warranty on this software; it is distributed freely and
;;  therefore 'as is'.
;;
;;  DISCLAIMER: This software is a personal utility written by Steve
;;              Koren and is not associated in any way with Hewlett
;;              Packard Company.  Neither HP nor I support this software.
;;              Use it at your own risk.
;;
;;  This very simple function is the most wonderful function of all time.
;;
;;  Well maybe not quite, but it is right up there.  It is similar to
;;  the align-equals function, but prompts for a regexp.  It finds
;;  the first occurance of that regexp in each line, and lines them
;;  up as far left as possible but no further left than the leftmost
;;  occurance in any of the lines.  See the documentation for details.
;;
;;  Change history:
;;     19 Apr 93 Steve Koren    - initial creation
;;     25 Jan 94 Steve Koren    - add function docs for public consumption
;;     18 May 94 Steve Koren    - numeric prefix does not insert space
;;                              - optional argument gives char to insert
;;                              - another optional argument for subexp
;;                              - allow multiple alignments at once.
;;     10 Jul 97 Dewey Sasser   - Add separate history for align-regexp
;;
;;  This code has been tested on:
;;     Machine       OS            Emacs Version
;;     ------------------------------------------------
;;     Amiga 4000,   AmigaDos 3.0, GNU emacs 18.58.1
;;     HP 9000/720,  HP-UX 9.0,    GNU emacs 18.57.4
;;     Dell Lattitude, OS/2 3.0,   GNU emacs 19.33.1
;;
;;  Notes:
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar align-regexp-history nil
  "History for align-regexp")

;***************************************************************************
;** Little insert-to-column function that is useful in its own right.
;***************************************************************************

(defun insert-to-column (col &optional pad-char)
  (interactive "nColumn: ")
  (insert (make-string (max (- col (current-column)) 0) (or pad-char ? )))
)

;***************************************************************************
;** Our align-regexp function.
;***************************************************************************

;;;###autoload
(defun align-regexp (regexp begin end &optional no-space pad-char
			    subexp-num)
  "
This simple function is the most wonderful function of all time.

Well maybe not quite, but it is right up there.  It is similar to the
align-equals function, but prompts for a regexp.  It finds the first
occurrence of that regexp in each line, and lines them up as far left as
possible but no further left than the leftmost occurrence in any of the
lines.

\"Huh?\", you say?  An example will make everything clear.  Consider a
marked region containing this text:

  bool operator< (const char* abc) { return strcmp(LS_Str, abc) < 0; }
  bool operator> (const char* s) { return strcmp(LS_Str, s) > 0; }
  bool operator<= (char* s) { return strcmp(LS_Str, s) <=0; }
  bool operator>= (char* s) { return strcmp(LS_Str, s) >=0;   }

Performing align-regexp and entering \"{\" will yield:

  bool operator< (const char* abc) { return strcmp(LS_Str, abc) < 0; }
  bool operator> (const char* s)   { return strcmp(LS_Str, s) > 0; }
  bool operator<= (char* s)        { return strcmp(LS_Str, s) <=0; }
  bool operator>= (char* s)        { return strcmp(LS_Str, s) >=0;   }

Doing it again on the same region with \"}\" yields:

  bool operator< (const char* abc) { return strcmp(LS_Str, abc) < 0; }
  bool operator> (const char* s)   { return strcmp(LS_Str, s) > 0;   }
  bool operator<= (char* s)        { return strcmp(LS_Str, s) <=0;   }
  bool operator>= (char* s)        { return strcmp(LS_Str, s) >=0;   }

You could also use a longer string.  Given the original form, entering
\"return\" would give:

  bool operator< (const char* abc) { return strcmp(LS_Str, abc) < 0; }
  bool operator> (const char* s) {   return strcmp(LS_Str, s) > 0; }
  bool operator<= (char* s) {        return strcmp(LS_Str, s) <=0; }
  bool operator>= (char* s) {        return strcmp(LS_Str, s) >=0;   }

By lining up things from left to right, you can pretty much make any
alignment.  Furthermore, you can perform multiple alignments at once,
by specifying a comma separated list of expressions to align.  For
example, the string \"{,}\" would align the opening and closing braces
in the above text.

Beware of regexp chars with special meanings.  With a numeric prefix,
the function does not force a space in the text.

When called non-interactively, it has the following arguments:

    (align-regexp regexp begin end &optional no-space pad-char subexp-num)

    regexp       - string for the regular expression to align.  This
                   can contain multiple comma separated regexps.  To
                   use a comma in the expression, backslash escape it.

    begin        - start of region on which to operate

    end          - end of region on which to operate

    no-space     - numeric prefix from interactive call.  1 will always
                   force at least once space, >1 will not.

    pad-char     - character to use for adjusting text instead of
                   the default space.

    subexp-num   - the number of the subexpression within regexp which
                   is where the alignment should begin.  This is usually
                   0 for the entire regexp (and thus the alignment
                   happens at the beginning of it) but it could be
                   something else.  Passed to match-beginning.

"
  ;;  (interactive "sAlign-regexp: \nd\nm\np")
  (interactive
   (list
    (read-from-minibuffer "Allign-regexp: "
			  (if align-regexp-history
			      (car align-regexp-history))
			  nil
			  nil
			  'align-regexp-history )
    (region-beginning)
    (region-end)
    (prefix-numeric-value current-prefix-arg)))
  (let ((min-column 0) (min-char 9999) (start-pos 0) end-pos fullexp
        (old-case-fold-search case-fold-search))

    (setq case-fold-search nil)
    (save-excursion
      (save-restriction
        (narrow-to-region begin end)
        (setq fullexp (concat regexp ","))
        
        (while (setq end-pos (string-match "[^\\]," fullexp start-pos))
          (setq min-column 0 min-char 9999)
          (setq end-pos (1+ end-pos))
          (setq regexp (substring fullexp start-pos end-pos))
          (goto-char (point-min))

          (while (re-search-forward regexp (point-max) t nil)
            (goto-char (match-beginning (or subexp-num 0)))
  
            (setq min-char   (min min-char   (current-column)))
            (skip-chars-backward "\\s ")
            (setq min-column (max min-column (+ (current-column)
                                                (if (> no-space 1) 0 1))))
            (beginning-of-line)
            (forward-line 1)
	    )
  
          (goto-char (point-min))
          (setq min-column (max min-column min-char))
  
          (while (re-search-forward regexp (point-max) t nil)
            (goto-char (match-beginning (or subexp-num 0)))
            (just-one-space)
            (if (> no-space 1) (backward-delete-char 1))
            (insert-to-column min-column pad-char)
            (forward-line 1)
	    )

          (setq start-pos (1+ end-pos))
	  )
	)
      )
    (setq case-fold-search old-case-fold-search)
    )
  )
