;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; block-cm.el -- does block comments around text
;; Author          : Stuart Hungerford (stuart@csis.dit.csiro.au)
;; Created On      : sometime long ago
;; Last Modified By: Dewey M. Sasser <dsasser@cerulean.com>
;; Last Modified On: Tue May 30 21:30:44 2000
;; Update Count    : 153
;; Status          : Working
;; PURPOSE
;; 	Place block comments around regions, text, etc.
;; 
;; TABLE OF CONTENTS
;;   (defun recomment-block ()
;;   (defun block-comment-region (arg start end)
;;   (defun uncomment-region ()
;;   (defun comment-fill-character ()
;;   (defun block-comment-begin-string ()
;;   (defun block-comment-end-string (&optional begin-string)
;;   (defun remove-ws-from-string (string)
;;   (defun block-comment-line (arg)
;;   (defun fill-line-with-spaces (number)
;;   (defun block-cm-center-line ()
;;   (defun* make-block-region (&optional
;; 
;; $Locker:  $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This needs to be modified *extensively* to be useful.  Current
; problems include:
; 1) doesn't use local variables
; 2) uses only c style comments
;
;
;
;# Occasionally I use comment "boxes" like this:
;
;# /*------------------------------------------------------------*/
;# /* This is a comment in a box.                                */
;# /*------------------------------------------------------------*/
;
;# Is there a neat way to insert this kind of comment?
;
;# 					Stuart Hungerford
;# 					stuart@csis.dit.csiro.au
;
;Well, you asked for it, you got it.  Be careful with this.  Its not
;pretty, hey I'm no elisp guru, but I use it anyway.

; com.el, origional code snarfed from a gosling translation bye dsm
; and modified by stergios to find the comment block rather than
; having to set point and mark by hand.
; suggested use:
;		M-x block-comment-region
;		M-x uncomment-region
;		M-x recomment-block
; Stergios Marinopoulos. copy, steal, sell this code. do want you want
; with it - just dont blame me. 
;


;Elmakedoc:  Automatic Prototyping Destinations
;  functions: block-cm.doc
;  variables: block-cm.doc


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recomment-block() -- fill-paragraph on current comment block
;; AUTHOR:      Dewey M. Sasser <dewey@athena.mit.edu>
;; SYNOPSIS
;;   
;;   (defun recomment-block ()
;;   
;; DESCRIPTION
;;   strips the comments, runs fill paragraph, replaces the comments
;; NOTES
;;   |><|
;; CAVEATS AND BUGS
;;   will probably not be good for a comment that's more than 1
;;   paragraph.   It does not currently work with anything but c code. 
;;
;;;###autoload
(defun recomment-block ()
  "Fills paragarph for the block-styled comment point is in.  Point must
be in a block comment, it cannot not be on the 1st or last line.
My definition of a comment block looks like this:
/*********************************************************/
/* You mean you actually comment code?			 */
/*							 */
/*							 */
/*********************************************************/
You can change the regexps for different looking comment lines.
"
  (interactive)
  ; locate comment region point is in, then call uncomment it, then fill it.
  (save-excursion
    (end-of-line)
    (re-search-backward "/\\*[*]*\\*/\n")
    (push-mark)
    (next-line 1)
    (re-search-forward "/\\*[*]*\\*/\n")
    (uncomment-region)
    (backward-word 1)
    (fill-paragraph 'nil)
    (set-fill-column 63)
    
    ; locate region filled paragraph is in, and wrap it in comments.
    (backward-paragraph 1)
    (next-line 1)
    (push-mark)
    (forward-paragraph 1)
    (block-comment-region 1 (region-beginning) (region-end))
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; comment-region() -- place a block comment around a region
;; AUTHOR:      Dewey M. Sasser <dewey@athena.mit.edu>
;; SYNOPSIS
;;   
;;   (defun comment-region ()
;;   
;; DESCRIPTION
;;   |>Description of function<|
;; NOTES
;;   This should probably be changed some time in the future to place
;;   a block comment only around the current _rectangle_, but will be
;;   useful enough once I get rid of the c only comments.
;; CAVEATS AND BUGS
;;   Started using only c style comments.
;;;###autoload
(defun block-comment-region (arg start end)
  "Wrap a rectangular block of comments around region.  'C' Example: \

/*******************************************************************/ 
/* This is a block comment region:                                 */
/* My Prog:  whatever                                              */
/*******************************************************************/

This can be removed with uncomment-region (\\[uncomment-region])."
  (interactive "p\nr")
  (let* (
	save-point
	longest
	(comment-start (block-comment-begin-string))
	(comment-end   (block-comment-end-string comment-start))
	(comment-fill-character (comment-fill-character))
	)
    (make-block-region start
		       end
		       nil
		       nil
		       comment-start
		       comment-end
		       nil
		       arg)))
;;;    (narrow-to-region (point) (mark))
;;;    (goto-char (point-min))
;;;  
;;;					; Now count the length of the longest line
;;;    (setq longest 0)
;;;    (while (not (eobp))
;;;      (end-of-line)
;;;      (delete-horizontal-space)
;;;      (if (> (setq len (current-column)) longest)
;;;	  (setq longest len))
;;;      (forward-char))
;;;
;;;    (if (< longest fill-column)
;;;	(setq longest fill-column))
;;;
;;;
;;;					; Now insert the top of the comment
;;;    (goto-char (point-min))
;;;    (make-divisor (1- (+ longest
;;;		     (length comment-start)
;;;		     (length comment-end))))
;;;    
;;;					; Insert sides of comment
;;;    (while (not (eobp))
;;;      (insert comment-start)
;;;      (end-of-line)
;;;      (while (< (current-column) (+ longest (length comment-start) 1))
;;;	(insert " "))
;;;      (insert (if (> (length comment-end) 0)
;;;		  comment-end
;;;		comment-fill-character)) ;insert comment end of the 
;;;      (forward-char))			;fill character if no comment end
;;;
;;;					; Now insert last line of comment
;;;    (goto-char (point-max))
;;;    (make-divisor (1- (+ longest
;;;		     (length comment-start)
;;;		     (length comment-end))))
;;;    
;;;					; Insert sides of comment
;;;    (setq longest (count-lines (point-min) (point-max)))
;;;    (beginning-of-buffer)
;;;    (set-mark (point))
;;;    (goto-char (point-max))
;;;    (widen)
;;;
;;;    (while (looking-at "[ \n]")
;;;      (forward-char))
;;;    (setq len (current-column))
;;;    (exchange-point-and-mark)
;;;    (set-mark (point))
;;;    (while (> longest 0)
;;;      (setq longest (- longest 1))
;;;      (setq loop len)
;;;      (while (> loop 0)
;;;	(insert " ")
;;;	(setq loop (- loop 1)))
;;;      (next-line 1)
;;;      (beginning-of-line))
;;;    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uncomment-region() -- Just that.
;; AUTHOR:      Dewey M. Sasser <dewey@athena.mit.edu>
;; SYNOPSIS
;;   
;;   (defun uncomment-region ()
;;   
;; DESCRIPTION
;;   |>Description of function<|
;; NOTES
;;   |><|
;; CAVEATS AND BUGS
;;   |> describe any peculiarities <|
;;;###autoload
(defun uncomment-region ()
"Strip out rectangular blocks in a region. \
The comment look like this (for 'C'):

/*******************************************************************/ 
/* This is a block comment region:                                 */
/* My Prog:  whatever                                              */
/*******************************************************************/

and can be make by marking a region and typing \\[block-comment-region]."
  (interactive)
  (let* (
	(comment-start  (block-comment-begin-string))
	(comment-end    (block-comment-end-string comment-start))
	)
    (save-excursion
      (narrow-to-region (point) (mark))
      
					; remove spaces
      (beginning-of-buffer)
      (replace-regexp "^[ ]*" "")
      
					; remove comment top and bottom
      (beginning-of-buffer)
      (replace-regexp
       (concat
      (regexp-quote (remove-ws-from-string comment-start)) 
					;comment beginning no ws
      "[ \t]*\\("			;zero or more whitespace+(
      
      (regexp-quote (char-to-string
		     (comment-fill-character)))

					;the filler
      "\\)*[ \t]*"			;end group of filler, replace
					;0 or more and ws
      (regexp-quote (remove-ws-from-string comment-end))
					;comment ending
      )
       "")				;replace with nothing
    
    ;; remove left side
      (beginning-of-buffer)
      (replace-regexp
       (concat
	"^"				;beginning of line
	(regexp-quote comment-start)	;+ start of comment
	) "")				;to nothing
      
    
      ;; remove right side
      (beginning-of-buffer)
      (replace-regexp			;replace
       (concat				
	(regexp-quote comment-end)	;comment end
	"$")				;at end of line
       "")				;with nothint
      
      (widen)
      ) 
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; comment-fill-character() -- find a good char to use in comments.
;; AUTHOR:      Dewey M. Sasser <dewey@athena.mit.edu>
;; SYNOPSIS
;;   
;;   (defun comment-fill-character ()
;;   
;; DESCRIPTION
;;   Uses last character of comment beginning

;;;###autoload
(defun comment-fill-character ()
  "Get a character that would be good to use in comment lines."
  (let ((len (1- (length comment-start)))
	(character))
    (while (and
	    (> len 0)
	    (eq (char-syntax (aref comment-start len)) ? ))
      (setq len (1- len)))
    (aref comment-start len)))
	
;;;###autoload
(defun block-comment-begin-string ()
  "Returns a string that is suitable to begin a block in the current mode."
    (if (eq major-mode 'emacs-lisp-mode)
	(concat ";;; ")
      comment-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; block-comment-end-string() -- return the string for end of comment
;;                               block
;; AUTHOR:      Dewey M. Sasser <dewey@athena.mit.edu>
;; SYNOPSIS
;;   
;;   (defun block-comment-end-string (&optional begin-string)
;;   
;; DESCRIPTION
;;   This function returns comment-end if there is one, otherwise if
;;   we passed it a begin-string, it removes white space to use it.
;;   If we didn't pass it a begin string, it uses comment-start.

;;;###autoload
(defun block-comment-end-string (&optional begin-string)
  "Returns a string suiable for ending block comments."
    (if (> (length comment-end) 0)	;if there is a comment-end, use it
	comment-end
      (progn				;otherwise...
	(if begin-string		;if we got a begin-string
	    ()				;use it
	    (setq begin-string comment-start)) ;or make it comment-start
	(concat " " (remove-ws-from-string begin-string)))))
	

	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; remove-ws-from-string() -- just that
;; AUTHOR:      Dewey M. Sasser <dewey@athena.mit.edu>
;; SYNOPSIS
;;   
;;   (defun remove-ws-from-string (string)
;;   
;;;###autoload
(defun remove-ws-from-string (string)
  "Returns its argument with all white space removed."
  (let (
	new-string
	(len 0)
	(length-of-string (length string))
	)
    ;; while the string length is less than max, AND the syntax of
    ;; the current character is NOT whitespace, add the character
    ;; to the return string.
    (while (< len length-of-string)	;loop over whole string
      (if (not				;if the syntax is not ws
	   (=
	    (char-syntax (aref string len))
	    ? ))
	  (setq new-string (concat	;add it to our string
			    new-string
			    (char-to-string (aref string len)))))
      (setq len (1+ len))		;and increment the loop variable
      )
    new-string))			;return the string we made.
  
;;;###autoload
(defun block-comment-line (arg)
  "Put the current line in a block comment."
  (interactive "p")
  (let (start
	end)
    (unless (> arg 1)
      (block-cm-center-line))
    (beginning-of-line)
    (setq start (point-marker))
    (end-of-line) 
    (save-excursion (forward-line 1)
		    (setq end (point-marker)))
    (goto-char start)
    (if (> arg 1)
	(fill-line-with-spaces (1- arg)))
    (block-comment-region arg start end)
    (set-marker start nil)
    (set-marker end nil)
    ))
                                   

(defun fill-line-with-spaces (number)
  "Fill the current line and put spaces between letters."
  (save-restriction
    (save-excursion
      (let (
	    (start (progn
		     (beginning-of-line)
		     (point-marker) ))
	    (end (progn
		   (end-of-line) 
		   (point-marker)))
	    (spaces (make-string number ? ))
	    (lines (make-string number ?\n)))
	(let ((fill-column (/ fill-column (1+ number))))
	  (narrow-to-region start end)
	  (fill-region start end)
	  (goto-char (point-min) )
	  (while (< (point) (point-max))
	    (forward-char 1)
	    (insert spaces))
	  (goto-char (point-min))
	  (while (search-forward "\n" nil 't)
	    (insert lines)))
	(goto-char (point-min))
	(save-excursion			;hack to make ending lines correct
	  (goto-char (point-max))
	  (skip-chars-backward " \n" )
	  (delete-region (point) (point-max) )
	  (insert lines)
	  (delete-char -1)
	  (goto-char (point-min))
	  (skip-chars-forward " \n")
	  (delete-region (point-min) (point))
	  (insert lines)
	  (delete-char -1)))
      (while (< (point) (point-max) )
	(block-cm-center-line)
	(forward-line 1)))))
      
		 
		 

;;;###autoload
(defun block-cm-center-line ()
  "Center the current line within 0..fill-column, using spaces"
  (save-excursion
    (beginning-of-line)
    (while (or
	    (eq (char-after (point)) ? )
	    (eq (char-after (point)) ?	))
      (delete-char 1))
    (let ((count 0)
	  (len (save-excursion
		  (let ((here (point)))
		    (end-of-line)
		    (length (trim-whitespace
			     (buffer-substring here (point))))))))
      (insert (concat (make-vector (/ (- fill-column len) 2) ? ))))))

(require 'cl)

;;;###autoload (autoload 'make-block-region "block-cm"   "Put a box around a region, delimited by key'd characters" 't nil)

(defun* make-block-region (&optional
			   region-start
			   region-end
			   top-border
			   bottom-border
			   left-border
			   right-border
			   width
			   depth)

  "Put a box around a region, delimited by key'd characters"
  (interactive "r
cTop Border (char): 
cBottom Border (char): 
sLeft Border (string): 
sRight Border (string): ") 
  (unless region-start (setq region-start (region-beginning)))
  (unless region-end (setq region-end (region-end)))
  (unless width
    (setq width fill-column))
  (unless right-border
    (setq right-border comment-start))
  (unless left-border
    (setq left-border comment-end))
  (unless top-border
    (setq top-border (last-char-of (trim-whitespace comment-start))))
  (unless bottom-border
    (setq bottom-border top-border))
  (unless depth
    (setq depth 1))


  ;; handle depth stuff for beginning and ending comments
  (let ((add-depth (1- depth)))
    (if (> add-depth 0)
	(progn
	  (setq left-border
		(concat (trim-trailing-whitespace left-border)
			(make-string add-depth
				     (last-char-of (trim-whitespace left-border)))
			(trailing-whitespace left-border)))
	  (setq right-border
		(concat
		 (leading-whitespace right-border)
		 (make-string add-depth
			      (first-char-of (trim-whitespace right-border)))
		 (trim-leading-whitespace right-border))))))

  
  (save-excursion
    (let (
	  (start (progn
		   (goto-char region-start)
		   (beginning-of-line)
		   (point-marker)))
	  (end (progn
		 (goto-char region-end)
		 (beginning-of-line)
		 (point-marker)))
	  (max-width 0)
	  (length-right (length right-border))
	  (length-left  (length left-border))
	  (top-left (trim-all-whitespace left-border))
	  (top-right (trim-all-whitespace right-border))
	  )


      (untabify start end)
      ;; the real stuff
      (goto-char start)
      ;; look for the longest line
      (while (> end (point))
	(end-of-line)
	(setq max-width (max max-width (current-column)))
	(forward-line 1))

      (goto-char start)
      (beginning-of-line)

      (setq max-width (+ length-left
			 (max (- width length-left)
			      max-width)
			 length-right))

      (while (> end (point))
	(let ((proper-width		;by the time we get to current
					;column, we'll already have
					;inserted the left side
	       (- max-width
		  length-right)))
	  (beginning-of-line)
	  (insert left-border)
	  (end-of-line)
	  (insert (make-string (- proper-width (current-column))
			       ? )
		  right-border)
	  (forward-line 1)))
		 

      (goto-char start)
      (beginning-of-line)
      ;; top borders
      (loop for x from 1 to depth
	    do (insert
		top-left
		(make-string  (- max-width
				 (length top-left)
				 (length top-right))
			      top-border)
		top-right
		"\n"))

      ;; top blank lines
      (loop for x from 1 to (1- depth)
	    do (insert
		left-border
		(make-string  (- max-width
				 length-left
				 length-right)
				 ? )
		right-border
		"\n"))
      (goto-char end)
      (beginning-of-line)
      ;; bottom blank lines
      (loop for x from 1 to (1- depth)
	    do (insert
		left-border
		(make-string  (- max-width
				 length-left
				 length-right)
				 ? )
		right-border
		"\n"))
      ;; bottom borders
      (loop for x from 1 to depth
	    do (insert
		top-left
		(make-string (- max-width
				(length top-left)
				(length top-right))
			     bottom-border)
		top-right
		"\n"))
      )))


(provide 'block-cm)