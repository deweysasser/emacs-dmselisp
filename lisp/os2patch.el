;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; os2patch.el -- OS/2 emacs patches
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser
;; Created On      : Fri Mar 15 15:59:17 1996
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defun file-name-nondirectory (file)
;;   (defun file-separator-p (c)
;;   (defun file-name-directory (file)
;;   (defun rmail-insert-inbox-text (files renamep)
;;   (defun os2-remove-bad-letters (name)
;;   (defun good-letter-p (letter)
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: os2patch.el,v $
;; $Revision: 1.3 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun file-name-nondirectory (file)
  (let 
      ((newfile)
       (len (1- (length file))))
    (while (and (> len 0)
		(not 
		 (if (eq 
		      (aref file len)
		      (aref ":" 0))
		     (= len 1)
		   (file-separator-p (aref file len)))))
      (setq len (1- len)))

    (let ((hack (substring file 
	       (if (= len 0)
		   0
		 (1+ len)))))
      (if (string= hack "")
	  nil
	hack))))
  
(defun file-separator-p (c)
  "return non-nil if c is a path separator"
  (cond ((eq c (aref "/" 0)) 't)
	((eq c (aref "\\" 0)) 't)))

(defun file-name-directory (file)
  (let ((last -1)
	(len (length file))
	(current 0))
    (while (< current len)
      (if (or
	   (file-separator-p (aref file current))
	   (and (= current 1)
		    (= (aref file 1)
		       (aref ":" 0))))
	  (setq last current))
      (setq current (1+ current)))
    (substring file 0 (1+ last))))

(defun rmail-insert-inbox-text (files renamep)
  (let (file tofile delete-files movemail popmail)
    (while files
      (setq file (file-truename
		  (expand-file-name (substitute-in-file-name (car files))))
	    ;;>> un*x specific <<
	    ;; The "+" used to be "~", which is an extremely poor choice;
	    ;; it might accidentally be deleted when space is low
	    ;; (as happened to me!).
	    tofile (concat (os2-remove-bad-letters file) "+"))
      ;; If getting from mail spool directory,
      ;; use movemail to move rather than just renaming,
      ;; so as to interlock with the mailer.
      (setq movemail (string= (file-name-directory file)
			      (file-truename rmail-spool-directory))
	    popmail (string-match "^po:" (file-name-nondirectory file)))
      (if popmail (setq file (file-name-nondirectory file)
			renamep t))
      (if movemail
	  (progn
	    (setq tofile (expand-file-name
			   ;; Generate name to move to from inbox name,
			   ;; in case of multiple inboxes that need moving.
			   (concat ".newmail-" (file-name-nondirectory file))
			   ;; Use the directory of this rmail file
			   ;; because it's a nuisance to use the homedir
			   ;; if that is on a full disk and this rmail
			   ;; file isn't.
			   (file-name-directory
			     (expand-file-name buffer-file-name))))
	    ;; On some systems, /usr/spool/mail/foo is a directory
	    ;; and the actual inbox is /usr/spool/mail/foo/foo.
	    (if (file-directory-p file)
		(setq file (expand-file-name (user-login-name)
					     file)))))
      (if popmail
	  (message "Getting mail from post office ...")
	(if (or (and (file-exists-p tofile)
		     (/= 0 (nth 7 (file-attributes tofile))))
		(and (file-exists-p file)
		     (/= 0 (nth 7 (file-attributes file)))))
	    (message "Getting mail from %s..." file)))
      ;; Set TOFILE if have not already done so, and
      ;; rename or copy the file FILE to TOFILE if and as appropriate.
      (cond ((not renamep)
	     (setq tofile (os2-remove-bad-letters file)))
	    ((or (file-exists-p tofile) (and (not popmail)
					     (not (file-exists-p file))))
	     nil)
	    ((and (not movemail) (not popmail))
	     ;; Try copying.  If that fails (perhaps no space),
	     ;; rename instead.
	     (condition-case nil
		 (copy-file file tofile nil)
	       (error
		;; Third arg is t so we can replace existing file TOFILE.
		(rename-file file tofile t)))
	     ;; Make the real inbox file empty.
	     ;; Leaving it deleted could cause lossage
	     ;; because mailers often won't create the file.
	     (condition-case ()
		 (write-region (point) (point) file)
	       (file-error nil)))
	    (t
	     (let ((errors nil))
	       (unwind-protect
		   (save-excursion
		     (setq errors (generate-new-buffer " *rmail loss*"))
		     (buffer-disable-undo errors)
		     (call-process
		      (or rmail-movemail-program
			  (expand-file-name "movemail" exec-directory))
		      nil errors nil file tofile)
		     (if (not (buffer-modified-p errors))
			 ;; No output => movemail won
			 nil
		       (set-buffer errors)
		       (subst-char-in-region (point-min) (point-max)
					     ?\n ?\  )
		       (goto-char (point-max))
		       (skip-chars-backward " \t")
		       (delete-region (point) (point-max))
		       (goto-char (point-min))
		       (if (looking-at "movemail: ")
			   (delete-region (point-min) (match-end 0)))
		       (beep t)
		       (message (concat "movemail: "
					(buffer-substring (point-min)
							  (point-max))))
		       (sit-for 3)
		       nil))
		 (if errors (kill-buffer errors))))))
      ;; At this point, TOFILE contains the name to read:
      ;; Either the alternate name (if we renamed)
      ;; or the actual inbox (if not renaming).
      (if (file-exists-p tofile)
	  (let (size)
	    (goto-char (point-max))
	    (setq size (nth 1 (insert-file-contents tofile)))
	    (goto-char (point-max))
	    (or (= (preceding-char) ?\n)
		(zerop size)
		(insert ?\n))
	    (setq delete-files (cons tofile delete-files))))
      (message "")
      (setq files (cdr files)))
    delete-files))


(defun os2-remove-bad-letters (name)
  (let ((string)
	(current 0)
	(last)
	(len (length name)))
    (while (> len current)
      (if (or (= current 1)
	      (good-letter-p (aref name current)))
	  (setq string (concat string (list (aref name current)))))
      (setq current (1+ current)))
    string))
	

(defun good-letter-p (letter)
  (cond 
   ((eq letter 58) nil)
   ('t 't)))
   
