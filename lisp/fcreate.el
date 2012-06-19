;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fcreate.el -- provide "created-file-hooks"
;; Author          : Dewey M. Sasser <dewey@jenna.blake7.nvs.com>
;; Created On      : Mon Mar 28 09:23:46 1994
;; Status          : $State: Exp $
;; Name            : $Name: HEAD $
;; Keywords        : 
;; PURPOSE
;; 	If this file is loaded, the hooks "created-file-hooks" will be
;;      run sometime during the "file-file-hooks" if the file is new.
;; 
;; TABLE OF CONTENTS
;;   (defvar created-file-version-list '( "19.30" "19.27" "19.25" "19.23" "19.22" "19.19" "18.59" "19.33" "19.34")
;;   (defvar created-file-hooks nil
;;   (defvar created-file-alist '()
;;   (defun just-created-filep ()
;;   (defun run-created-hooks ()
;;   (defun string-in-list (list string)
;;   (defun fcreate-launch-alist ()
;;   (defun fcreate-match-and-call (cell)
;;   (defun fcreate-add-to-creation-alist (regexp function)
;; 
;; $RCSfile: fcreate.el,v $
;; $Revision: 1.15 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Elmakedoc:  Automatic Prototyping Destinations
;  functions: fcreate.doc
;  variables: fcreate.doc

(defvar created-file-version-list '( "19.30" "19.27" "19.25" "19.23" "19.22" "19.19" "18.59" "19.33" "19.34")
  "List of versions on which this package has been tested")

(defvar created-file-hooks nil
  "*Hooks to run when file is created.")

(defvar created-file-alist '()
  "*Alist of form (regexp . function-name) to be run at creation.
This function may also be used (via bound-p) to check for the presence
of this package.")

(defun just-created-filep ()
  "Return 't if this file has just been created.
This function works under Version 19.22, but hasn't bee tested under other
versions.  This should only be called in the context of (after-find-file).
i.e from something on find-file-hooks."
  ;; the error variable is internal to the function after-find-file
  ;; it is true if the file has just been created.
  (declare (special error))
  (if (boundp 'error)
      error
    nil))

(defun run-created-hooks ()
  "Runs the file created hooks.  This function is placed on find-file-hooks"
  (if (just-created-filep)
      (run-hooks 'created-file-hooks)))


(defun string-in-list (list string)
  "Check to see if string can be found in list."
  (let ((found))
    (while (and list
		(not
		 (setq found (string= string (car list)))))
      (setq list (cdr list)))
    found))

(add-hook 'find-file-hooks 'run-created-hooks)

(if (string-in-list created-file-version-list (substring emacs-version 0 5))
    nil
  (message "Create Hooks not tested under this version of emacs! Contact dewey@mit.edu")
  (sleep-for 1))

(defun fcreate-launch-alist ()
  "This function is called when a file is created and processes the alist.
It matches the regexp in each cell of the alist against the filename, calling the appropriate function if it matches."
  (if created-file-alist
    (mapcar 'fcreate-match-and-call created-file-alist))
  (message "(New File)"))

(defun fcreate-match-and-call (cell)
  "Match against file name or major mode and call function if match.
If (stringp (car cell)) is true, this matches the current file name
against (car cell), calling (cdr cell) if it matches.
Otherwise, if the major-mode is eq to (car cell), (cdr cell) will be called."
  (if cell
      (cond
       ((and (stringp (car cell))
	     (string-match (car cell) (buffer-file-name)))
	(funcall (cdr cell)))
       ((eq major-mode (car cell))
	(funcall (cdr cell))))))
	
;;;###autoload
(defun fcreate-add-to-creation-alist (regexp function)
  "Arrange so that if file is created that matches REGEXP, FUNCTION is
called in the newly created buffer"
   (let* ((pair (cons regexp function)))
     (if (member pair created-file-alist)
	 nil
       (setq created-file-alist (cons (cons regexp function)
				      created-file-alist)))))

;; install the function
(add-hook 'created-file-hooks 'fcreate-launch-alist)


(provide 'fcreate)
