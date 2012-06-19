;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; makedocs.el -- extract documentation string from emacs buffers.
;; 
;; Author          : Dewey M. Sasser
;; Created On      : Fri Jun 24 12:35:11 1994
;; Status          : $State: Exp $
;; Name            : $Name: HEAD $
;; Keywords        : 
;; PURPOSE
;; 	This package will extract all documentation for defun's and
;;      defvar's from and emacs lisp buffer and put it into another
;;      buffer.  It may eventually be a parallel of the "autoproto"
;;      package for emacs lisp documentation.
;; 
;; TABLE OF CONTENTS
;;   (defvar elmakedoc-here nil
;;   (defvar elmakedoc-info-message nil
;;   (defvar elmakedoc-inhibit-mode-check nil
;;   (defun elmakedoc-update ()
;;   (defun elmakedoc-insert-protos (filename buffer from-file type)
;;   (defun elmakedoc-delete-prototypes ()
;;   (defun elmakedoc-find-prototypes (ebuffer sbuffer)
;;   (defun elmakedoc-switch-prototypes (string ebuffer sbuffer)
;;   (defun elmakedoc-insert-prototype (string buffer)
;;   (defun find-next-elisp-def ()
;;   (defun elmakedoc-find-efile-name ()
;;   (defun elmakedoc-find-sfile-name ()
;;   (defun elmakedoc-format-prototype ()
;;   (defun elmakedoc-find-here ()
;;   (defun elmakedoc-add-reference (type file)
;;   (defun elmakedoc-replace-string (old new)
;;   (defun elmakedoc-replace-regexp (old new)
;;   (defun elmakedoc-check-mode ()
;;   (defun make-elmakedoc (functions variables)
;;   (defun elmakedoc-generate-new-buffer ( name )
;;   (defun elmakedoc-get-type ( string )
;;   (defun elmakedoc-name-type (type)
;; 
;; $RCSfile: makedocs.el,v $
;; $Revision: 1.11 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Elmakedoc:  Automatic Prototyping Destinations
;  functions: makedocs.doc
;  variables: makedocs.doc

(provide 'elmakedoc)

(defvar elmakedoc-here nil
  "Variable to use when filename is 'here'.")

(defvar elmakedoc-info-message nil
  "Put in informational message with make-elmakedoc.")

(defvar elmakedoc-inhibit-mode-check nil
  "*Set this variable to make Elmakedoc run regardless of mode.
If this variable is nil, Elmakedoc will only run in c-mode or c++-mode.")

(defun elmakedoc-update ()
  "Update the prototypes for a file."
  (interactive)
  (let (
	(sbuffer (elmakedoc-generate-new-buffer " *variable prototypes*"))
	(ebuffer (elmakedoc-generate-new-buffer " *functions prototypes*"))
	(sfilename)			;name of variables file
	(efilename)			;name of functions file
	)
    (if (not (elmakedoc-check-mode))
	nil
      (message "Elmakedoc running...")
      (save-excursion
	(goto-char (point-min))
	(if (not (search-forward "Elmakedoc: "))
	    nil				;exit function
	  (elmakedoc-find-here)
	  (setq efilename (elmakedoc-find-efile-name))
	  (setq sfilename (elmakedoc-find-sfile-name))
	  (elmakedoc-find-prototypes ebuffer sbuffer) ;get
					;prototypes into buffers
	  (elmakedoc-insert-protos sfilename sbuffer (buffer-file-name) "variables")
	  (elmakedoc-insert-protos efilename ebuffer (buffer-file-name) "functions")
	  (kill-buffer sbuffer)
	  (kill-buffer ebuffer)
	  (message "Elmakedoc running...done")
	  nil
	  )))))

(defun elmakedoc-insert-protos (filename buffer from-file type)
  "Insert prototypes into proper place in filename."
  (let (
	(newbuffer)
	(cbuffer (current-buffer))
	)
    (if (not (string= filename "here"))
	(setq newbuffer (find-file-noselect filename))
      (setq newbuffer (current-buffer))
      (setq from-file (car elmakedoc-here))
      )
    (set-buffer newbuffer)
    (set-window-buffer (selected-window) newbuffer)
					;take this out after testing
    (save-excursion			;in new buffer
      (goto-char (point-min))
      (if (re-search-forward (concat
				  "Elmakedoc " type " from:\\s *"
				  (regexp-quote
				   (file-name-nondirectory from-file)))
				 nil 't)
	  ()
	(if (not (string= filename "here"))
	    (goto-char (point-max))
	  (goto-char (cdr elmakedoc-here))
	  (forward-char 1))
	(elmakedoc-add-reference type (file-name-nondirectory from-file)))
      (search-forward "\n")
      (elmakedoc-delete-prototypes)
      (insert "\n")
      (narrow-to-region (point) (point))
      (insert-buffer buffer)
      (widen)
      )
    (set-buffer cbuffer)
    (set-window-buffer (selected-window) cbuffer)
    ))

(defun elmakedoc-delete-prototypes ()
  "Delete old prototypes."
  (save-excursion
    (let ((here (point)))
      (save-excursion
	(re-search-forward (concat
				(regexp-quote ";;; ")
				"Elmakedoc End"))
	(beginning-of-line)
	(delete-region here (point))))))
			      
			      
    
    

  
(defun elmakedoc-find-prototypes (ebuffer sbuffer)
  "Find the prototypes, putting them into the buffers."
  (let ((string))
    (while (setq string (find-next-elisp-def))
	    
      (elmakedoc-switch-prototypes string ebuffer sbuffer))))

(defun elmakedoc-switch-prototypes (string ebuffer sbuffer)
  "Put prototype into appropriate buffer."
  (elmakedoc-insert-prototype string 
			      (if (eq (car string) 'defvar)
				  sbuffer
				ebuffer)))

(defun elmakedoc-insert-prototype (string buffer)
  "Insert a prototype into a buffer."
  (let ((cbuffer (current-buffer)))
    (set-buffer buffer)
    (set-window-buffer (selected-window) buffer)		;take this out after testing
    (narrow-to-region (point-max) (point-max))
    (insert (elmakedoc-name-type string)
	    ": " 
	    (car (cdr string))
	    "\nDescription:\n"
	    (car (cdr (cdr string)))
	    "\n\n"
	    )
    (elmakedoc-format-prototype)
    (widen)
    (set-buffer cbuffer)
    (set-window-buffer (selected-window) cbuffer)		;take this out after testing
    ))
       

(defun find-next-elisp-def ()
  "Find the next defun or defvar line."
  (interactive)
  (let ((function-name)
	(function-docs)
	(string))
    (if (not (re-search-forward
	      "(def\\(un\\|var\\|macro\\) \\(.*\\) .*\n?[ \t]*\"\\([^\"]*\\)\"[ \t]*[ \t\n]*\\((interactive\\)?"
	      nil 't))
	()				;do nothing if it fails
      (setq function-name (buffer-substring (match-beginning 2)
					    (match-end 2)))
      (setq function-docs (buffer-substring (match-beginning 3)
					    (match-end 3)))
      (setq type (elmakedoc-get-type (buffer-substring (match-beginning 1)
				     (match-end 1))))
      (if (not (and (eq type 'defun)
	       (not (match-beginning 4))))
	      (list type function-name function-docs)
	  (find-next-elisp-def)))))


    
(defun elmakedoc-find-efile-name ()
  "Find the name of the functions file."
  (re-search-forward "functions: \\(\\(.:\\)?\\(\\s_\\|\\sw\\|\\.\\|/\\)*\\)" nil 't)
  (buffer-substring (match-beginning 1) (match-end 1)))


(defun elmakedoc-find-sfile-name ()
  "Find the name of the variables file."
  (re-search-forward "variables: \\(\\(.:\\)?\\(\\s_\\|\\sw\\|\\.\\|/\\)*\\)" nil 't)
  (buffer-substring (match-beginning 1) (match-end 1)))


(defun elmakedoc-format-prototype ()
  "Formats the Prototype correctly.
This function assumes that the buffer is narrowed to a single prototype."
  (let ((beginning (point-min))
	(end (point-max))   
	)
    (goto-char beginning)
    (search-forward "\n")
    (widen)
    (narrow-to-region (point) end)
    ;;    (elmakedoc-replace-string "\n" " ")
    (goto-char (point-min))
    ;;    (fixup-all-whitespace)
    ;;    (fill-paragraph nil)
    (next-line 1)
    (insert-box (point) (point-max) fill-prefix)
    (next-line 1)
    (goto-char (point-max))
    (widen)
    (narrow-to-region beginning end)))
	
  

(defun elmakedoc-find-here ()
  "Find the place where 'here' should refer to."
  (save-excursion
    (beginning-of-line)
    (search-forward "variables:")
    (search-forward comment-end)
    (setq elmakedoc-here (cons (buffer-file-name) (point)))))

(defun elmakedoc-add-reference (type file)
  "Add a place to put prototypes."
;;  (if (not (string= file "here"))
;;      (goto-char (point-max))
;;    (error "You need to define where 'here' is for variable prototypes"))
;;    (setq file (file-name-nondirectory (car elmakedoc-here))))
    (insert "\n\n;;; " "Elmakedoc " type " from: " file  )
;;    (backward-char 4)
;;    (save-excursion
;;      (goto-char (point-max))
      (insert (concat
	       "\n;;; Elmakedoc End "
	       file))
      (previous-line 1)
;;      (beginning-of-line)
      )

(defun elmakedoc-replace-string (old new)
  "Replace old with new.  Taken from help on replace-string."
  (while (search-forward old nil t)
      (replace-match new nil t)))


(defun elmakedoc-replace-regexp (old new)
  "Replace old regexp with new.  Take from help on replace-regexp."
  (while (re-search-forward old nil t)
    (replace-match new nil nil)))

(defun elmakedoc-check-mode ()
  "Checks the mode to make sure elmakedoc should run."
  (if elmakedoc-inhibit-mode-check
      't
    (eq major-mode 'emacs-lisp-mode)))
	

(defun make-elmakedoc (functions variables)
  "Creates the elmakedoc header line."
  (interactive "FEnter file name for functions prototypes:
FEnter file name for variables prototypes: ")
  (let ((curdir (file-name-directory default-directory)))
    (if (string= (file-name-directory functions) curdir)
	(setq functions (file-name-nondirectory functions)))
    (if (string= (file-name-directory variables) curdir)
	(setq variables (file-name-nondirectory variables)))
    (if elmakedoc-info-message
	(insert comment-start
		"Automatic prototype maintainance packages by Dewey Sasser"
		comment-end "\n"
		comment-start
		"Never copy a prototype again!"
		comment-end "\n\n" ))
    (insert comment-start
	  "Elmakedoc:  Automatic Prototyping Destinations"
	  comment-end "\n"
	  comment-start
	  "  functions: " functions comment-end "\n"
	  comment-start
	  "  variables: " variables comment-end)))


(defun elmakedoc-generate-new-buffer ( name )
  "Create a new buffer, set its mode and fill prefix."
  (let ((thisbuffer (current-buffer))
	(buffer (generate-new-buffer name)))
    (set-buffer buffer)
    (text-mode)
    (setq fill-prefix "   ")
    (set-buffer thisbuffer)
    buffer))

(defun elmakedoc-get-type ( string )
  "Return a symbol representing the type of the object,
based on the string given."
  (cond  ((string= string "var") 'defvar)
	 ((string= string "un") 'defun)
	 ((string= string "macro") 'defmacro)))

(defun elmakedoc-name-type (type)
  "Return a name for a type"
  (let ((sym (car type)))
      (cond ((eq sym 'defun) "Function")
	    ((eq sym 'defvar) "Variable")
	    ((eq sym 'defmacro) "Macro"))))