;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autoproto.el -- automatic prototype maintainance.
;; Author          : Dewey M. Sasser <dewey@athena.mit.edu>
;; Created On      : Sat Nov  6 14:37:21 1993
;; Status          : $State: Exp $
;; Name            : $Name: HEAD $
;; PURPOSE
;; 	To set up a file for automatic prototypes, fun
;;      (make-autoproto).  To update the prototypes, run
;;      (autoproto-update).
;;
;; COPYRIGHT
;;      Currently, all right reserved by Dewey Sasser, the author.  It
;;      is anticipated that this package will be placed under the GNU
;;      Copyleft in the future, but I'm doing quite a bit of thinking,
;;      first.  I do extend to the users permission to modify this
;;      file to suit their needs, and request that a copy of these
;;      mods are mailed to dewey@athena.mit.edu.  Users also have the
;;      right to distribute this file, provided that *NO* fee is
;;      charged, and that no one down the line tries to claim
;;      ownership.  In short, very much like the GNU copyleft, but I'm
;;      not giving up the right to sell it, yet.
;; 
;; TABLE OF CONTENTS
;;   (defvar autoproto-here nil
;;   (defvar autoproto-info-message nil
;;   (defvar autoproto-inhibit-mode-check nil
;;   (defun autoproto-update ()
;;   (defun autoproto-insert-protos (filename buffer from-file type)
;;   (defun autoproto-delete-prototypes ()
;;   (defun autoproto-find-prototypes (ebuffer sbuffer)
;;   (defun autoproto-switch-prototypes (string ebuffer sbuffer)
;;   (defun autoproto-insert-prototype (string buffer)
;;   (defun find-next-c-function ()
;;   (defun skip-characters-backward (string &optional limit)
;;   (defun char-in-string (char string)
;;   (defun autoproto-find-efile-name ()
;;   (defun autoproto-find-sfile-name ()
;;   (defun find-first-word (string)
;;   (defun autoproto-format-prototype ()
;;   (defun kill-blank-lines (&optional max)
;;   (defun fixup-all-whitespace (&optional max)
;;   (defun autoproto-find-here ()
;;   (defun autoproto-add-reference (type file)
;;   (defun autoproto-replace-string (old new)
;;   (defun autoproto-replace-regexp (old new)
;;   (defun autoproto-check-mode ()
;;   (defun make-autoproto (extern static)
;; 
;; $RCSfile: autoproto.el,v $
;; $Revision: 1.18 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Elmakedoc:  Automatic Prototyping Destinations
;  functions: autoproto.doc
;  variables: autoproto.doc


(provide 'autoproto)

(defvar autoproto-here nil
  "*String to use when filename is 'here'.")

(defvar autoproto-info-message nil
  "*Put in informational message with make-autoproto.
If This variables it 't, autoproto inserts some descriptive text into
the buffer with the prototypes.")

(defvar autoproto-inhibit-mode-check nil
  "*Set this variable to make AutoProto run regardless of mode.
If this variable is nil, AutoProto will only run in c-mode or c++-mode.")

;;;###autoload
(defun autoproto-update ()
  "Update the prototypes for a file."
  (interactive)
  (let (
	(sbuffer (generate-new-buffer " *static prototypes*"))
	(ebuffer (generate-new-buffer " *extern prototypes*"))
	(sfilename)			;name of static file
	(efilename)			;name of extern file
	)
    (if (not (autoproto-check-mode))
	nil
      (message "AutoProto running...")
      (save-excursion
	(goto-char (point-min))
	(if (not (search-forward "AutoProto: "))
	    nil				;exit function
	  (autoproto-find-here)
	  (setq efilename (autoproto-find-efile-name))
	  (setq sfilename (autoproto-find-sfile-name))
	  (autoproto-find-prototypes ebuffer sbuffer) ;get
					;prototypes into buffers
	  (autoproto-insert-protos efilename ebuffer
				   (buffer-file-name) "extern")
	  (autoproto-insert-protos sfilename sbuffer
				   (buffer-file-name) "static")
	  (kill-buffer sbuffer)
	  (kill-buffer ebuffer)
	  (message "AutoProto running...done")
	  nil
	  )))))

(defun autoproto-insert-protos (filename buffer from-file type)
  "Insert prototypes into proper place in filename."
  (let (
	(newbuffer)
	(cbuffer (current-buffer))
	)
    (if (not (string= filename "here"))
	(setq newbuffer (find-file-noselect filename))
      (setq newbuffer (current-buffer))
      (setq from-file (car autoproto-here))
;;      (goto-char (cdr autoproto-here))
;;      (forward-char 1)
      )
    (set-buffer newbuffer)
    (set-window-buffer (selected-window) newbuffer)
					;take this out after testing
    (save-excursion			;in new buffer
      (goto-char (point-min))
      (if (re-search-forward (concat
				  "AutoProto " type " from:\\s *"
				  (regexp-quote
				   (file-name-nondirectory from-file)))
				 nil 't)
	  ()
	(if (not (string= filename "here"))
	    (progn
	      (goto-char (point-max))	;here's where it goes to end
	      (if (not (fboundp 'buffer-get-c-define-name))
		  ()
		(if (search-backward (concat
				  "#endif /*  "
				  (buffer-get-c-define-name)
				  "  */") nil 't)
		    (backward-char 1))))
	  (goto-char (cdr autoproto-here))
	  (forward-char 1))
	(autoproto-add-reference type (file-name-nondirectory from-file)))
      (search-forward "*/")
      (autoproto-delete-prototypes)
      (insert "\n")
      (narrow-to-region (point) (point))
      (insert-buffer buffer)
;;      (autoproto-format-prototype)
      (widen)
      )
    (set-buffer cbuffer)
    (set-window-buffer (selected-window) cbuffer)
    ))

(defun autoproto-delete-prototypes ()
  "Delete old prototypes."
  (save-excursion
    (let ((here (point)))
      (save-excursion
	(re-search-forward (concat
				(regexp-quote comment-start)
				"AutoProto End"))
	(beginning-of-line)
	(delete-region here (point))))))
			      
			      
    
    

  
(defun autoproto-find-prototypes (ebuffer sbuffer)
  "Find the prototypes, putting them into the buffers."
  (let ((string))
    (while (setq string (find-next-c-function))
	    
      (autoproto-switch-prototypes string ebuffer sbuffer))))

(defun autoproto-switch-prototypes (string ebuffer sbuffer)
  "Put prototype into appropriate buffer."
  (autoproto-insert-prototype string 
			      (if (string=
				   (find-first-word string) "static")
				  sbuffer
				ebuffer)))

(defun autoproto-insert-prototype (string buffer)
  "Insert a prototype into a buffer."
  (let ((cbuffer (current-buffer)))
    (set-buffer buffer)
    (set-window-buffer (selected-window) buffer)		;take this out after testing
    (narrow-to-region (point-max) (point-max))
    (insert string)
    (search-backward ")" nil 't)
    (forward-char 1)
    (insert ";")
    (autoproto-format-prototype)
    ;;(goto-char (point-min))
    ;;(insert "\n")
    ;;(kill-blank-lines (point-max))
    ;;(kill-ws-and-nl)
    (insert "\n")
    (widen)
    (set-buffer cbuffer)
    (set-window-buffer (selected-window) cbuffer)		;take this out after testing
    ))
   

    

    
;;;###autoload
(defun find-next-c-function ()
  "Find the prototype line of the next c function."
  (let ((limit (point))
	(start)
	(end))
    (if (not (search-forward "{" nil 't))
	nil
      (setq end (1- (point)))
      (search-backward "(" nil 't)
      (skip-characters-backward "^;/}#" limit)
      (if (not (char-equal (preceding-char) ?#))
	  ()
	(end-of-line)
	(forward-char 1))	  
      (setq start (point))
      (search-forward "{")
      (backward-char 1)
      (forward-sexp) 
    (buffer-substring start end)
    )))


(defun skip-characters-backward (string &optional limit)
  "Skip characters contained in string.\n
^ begins a complement character set."
  (if (= (aref string 0) ?^)
      (progn
	(setq string (substring string 1 (length string)))
	(while (and (if limit (> (point) limit) t)
		(not (char-in-string (preceding-char) string)))
	  (backward-char 1)))
    (while (char-in-string (preceding-char) string)
      (backward-char 1))))



	  
(defun char-in-string (char string)
  "Return 't if char is found in string."
  (let ((len (length string))
	(count 0)
	(return nil))
    (while (< count len)
      (if (char-equal char (aref string count))
	  (setq return 't))
      (setq count (1+ count)))
    return))

    

(defun autoproto-find-efile-name ()
  "Find the name of the extern file."
  (re-search-forward "extern: \\(\\(.:\\)?\\(\\s_\\|\\sw\\|\\.\\|/\\)*\\)" nil 't)
  (buffer-substring (match-beginning 1) (match-end 1)))


(defun autoproto-find-sfile-name ()
  "Find the name of the static file."
  (re-search-forward "static: \\(\\(.:\\)?\\(\\s_\\|\\sw\\|\\.\\|/\\)*\\)" nil 't)
  (buffer-substring (match-beginning 1) (match-end 1)))


(defun find-first-word (string)
  "Return the first word of string."
  (let ((start 0)
	(count 0)
	)
    (while (char-in-string (aref string start) " \t\n")
      (setq start (1+ start)))
    (setq count start)
    (while (not (char-in-string (aref string count) " \t\n"))
      (setq count (1+ count)))
    (substring string start count)))

(defun autoproto-format-prototype ()
  "Formats the Prototype correctly.
This function assumes that the buffer is narrowed to a single prototype."
  (let ((paren-position)
	(paren-line)
	(not-finished 't)
	)
    (goto-char (point-min))
    (autoproto-replace-string "\n" " ")
    (goto-char (point-min))
    (fixup-all-whitespace)
    ;;(kill-blank-lines)
    ;; this checks for K&R type and adjusts for it
    (goto-char (point-min))
    (search-forward ");")		;go to end of prototype
    (if (not (re-search-forward "[^ \t]" nil 't))
	()
      (goto-char (point-min))
      (replace-regexp "(.*$" "();"))	;end of k&r stuff
    
    (goto-char (point-min))
    (if (not (> (save-excursion (search-forward ")")
				(current-column))
		fill-column))
	()
      (setq paren-position (search-forward "("))
      (while not-finished
	(if (search-forward ")" (save-excursion (move-to-column
						 fill-column)
						(point))
			    't)
	    (setq not-finished nil)
	  (move-to-column fill-column)
	  (re-search-backward "[,(]" nil 't)
	  (forward-char 1)
	  (insert "\n")
	  (indent-for-tab-command) ;tab to indent the line
	  )))
    (goto-char (point-max))))
	
  
  

(defun kill-blank-lines (&optional max)
  "Just that."
  (interactive)
  (if max
      ()
    (setq max (point-max)))
   (while (and
	   (re-search-forward "^\\s *$" max 't)
	   (not (eobp)))
    (if (not (eobp))
	(delete-char 1))))


;;;(defun kill-ws-and-nl ()
;;;  "Kills all white space and newlines."
;;;  (while (autoproto-replace-regexp "[\\s \n][\\s \n]" "")))

(defun fixup-all-whitespace (&optional max)
  "Removes multiple whitespace from point to max."
  (if max
      ()
    (setq max (point-max)))
    (while (not (eobp))
      (if (not (char-equal (following-char) ?	)) ;if it's a tab
	  ()
	(delete-char 1)
	(insert " "))
      (if (and
	   (eq (char-syntax (preceding-char) ) ? )
	   (eq (char-syntax (following-char) ) ? ))
	  (delete-char 1)
	(forward-char 1))))

(defun autoproto-find-here ()
  "Find the place where 'here' should refer to."
  (save-excursion
    (beginning-of-line)
    (search-forward "static:")
    (search-forward comment-end)
    (setq autoproto-here (cons (buffer-file-name) (point)))))

(defun autoproto-add-reference (type file)
  "Add a place to put prototypes."
;;  (if (not (string= file "here"))
;;      (goto-char (point-max))
;;    (error "You need to define where 'here' is for static prototypes"))
;;    (setq file (file-name-nondirectory (car autoproto-here))))
    (insert "\n\n/* " "AutoProto " type " from: " file " */" )
;;    (backward-char 4)
;;    (save-excursion
;;      (goto-char (point-max))
      (insert (concat
	       "\n/* AutoProto End "
	       file " */"))
      (previous-line 1)
;;      (beginning-of-line)
      )

(defun autoproto-replace-string (old new)
  "Replace old with new.  Taken from help on replace-string."
  (while (search-forward old nil t)
      (replace-match new nil t)))


(defun autoproto-replace-regexp (old new)
  "Replace old regexp with new.  Take from help on replace-regexp."
  (while (re-search-forward old nil t)
    (replace-match new nil nil)))

(defun autoproto-check-mode ()
  "Checks the mode to make sure autoproto should run."
  (if autoproto-inhibit-mode-check
      ()				;this should be 't
    (if (or				;take out the if
	 (eq major-mode 'c-mode)
	 (eq major-mode 'c++-mode))
	t
      nil)))

;;;###autoload
(defun make-autoproto (extern static)
"Creates the autoproto header line. This function prompts for the
filenames to put to prototypes into, and inserts the keys into the
buffer for autoproto-update to locate the proper files." 
  (interactive "FEnter file name for extern prototypes:
FEnter file name for static prototypes: ")
  (let ((curdir (file-name-directory default-directory)))
    (if (string= (file-name-directory extern) curdir)
	(setq extern (file-name-nondirectory extern)))
    (if (string= (file-name-directory static) curdir)
	(setq static (file-name-nondirectory static)))
    (if autoproto-info-message
	(insert comment-start
		"Automatic prototype maintainance packages by Dewey Sasser"
		comment-end "\n"
		comment-start
		"Never copy a prototype again!"
		comment-end "\n\n" ))
    (insert comment-start
	  "AutoProto:  Automatic Prototyping Destinations"
	  comment-end "\n"
	  comment-start
	  "  extern: " extern comment-end "\n"
	  comment-start
	  "  static: " static comment-end)))

