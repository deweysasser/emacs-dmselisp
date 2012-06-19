;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mycrypt.el -- more pgp support
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser
;; Created On      : Fri Jun 16 13:09:15 1995
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defun encrypt-region (&optional recipients scheme)
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: crypt-extra.el,v $
;; $Revision: 1.3 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun encrypt-region (&optional recipients scheme)
  "*Encrypt the message to RECIPIENTS using the given encryption SCHEME.
RECIPIENTS is a comma separated string. If SCHEME is nil, use the value
of `mc-default-scheme'."
  (interactive
   (if current-prefix-arg
       (list nil (read-from-minibuffer "Encryption Scheme: " nil nil t))))
  
  (let ((default-recipients nil)
	(start (make-marker))
	(end (make-marker))
	args signed-p retval)

    (or scheme (setq scheme mc-default-scheme))
    (setq recipients
	  (cond (recipients		; given as function argument
		 (mc-split "\\([ \t\n]*,[ \t\n]*\\)+" recipients))
		(t			; prompt for it
		 (mc-cleanup-recipient-headers
		  (read-from-minibuffer "Recipients: " default-recipients)))))

    (or recipients
	(error "No recipients!"))

    (cond ((eq scheme 'pgp)
	   (and mc-encrypt-for-me
		(setq recipients (cons mc-pgp-user-id recipients)))
	   (setq args (list "+batchmode" "-feat"))
	   (if (or mc-pgp-always-sign (y-or-n-p "Sign the region? "))
	       (setq signed-p t
		     args (nconc args (list "-su" mc-pgp-user-id))))
	   (setq args (nconc args recipients))
	   (setf (marker-position start) (region-beginning))
	   (setf (marker-position end) (region-end))
	   (goto-char (region-beginning))
	   (let ((process-environment process-environment))
	     ;; Don't need to ask for the passphrase if not signing.
	     (if signed-p
		 (progn (mc-activate-passwd 'pgp)
			(insert mc-pgp-passwd "\n")
			(setq process-environment (cons "PGPPASSFD=0"
							process-environment))))
	     (message "Encrypting...")
	     ;; Use call-process-region rather than shell-command-on-region
	     ;; to get the exit code.
	     (goto-char end)
	     (setq retval (apply 'call-process-region
				 (nconc (list (marker-position start)
					      (marker-position end)
					      mc-pgp-path
					      t t nil)
					args)))
	     (or mc-passwd-timeout (mc-deactivate-passwd))
	     (if (= retval 0)
		 (progn
		   (goto-char start)
		   (search-forward mc-pgp-msg-begin-line)
		   (search-backward mc-pgp-msg-begin-line)
		   (mc-temp-display start (point) "*Encryption*"))
	       (error "Error while encrypting. Hit C-x u to undo."))
	     (setf (marker-position start) nil) ;; so marker update
					       ;; won't take more time
	     (setf (marker-position end) nil)))
	  ;; completely untested
	  ((eq scheme 'ripem)
	   (and mc-encrypt-for-me
		(setq recipients (cons mc-ripem-user-id recipients)))
	   ;; Anyone know any better way to do the following?
	   (setq args (nconc (list "-e" "-m" "encrypted"
				   "-T" "a" "-k" "-")
			     (apply 'nconc
				    (mapcar (function
					     (lambda (x)
					       (list "-r" x))) 
					    recipients))))
	   (goto-char (point-min))
	   (search-forward (concat "\n" mail-header-separator "\n"))
	   (setq start (point))
	   (mc-activate-passwd 'ripem)
	   (insert mc-ripem-passwd "\n")
	   (message "Encrypting...")
	   (setq retval (apply 'call-process-region
			       (nconc (list start (point-max) mc-ripem-path
					    t t nil)
				      args)))
	   (or mc-passwd-timeout (mc-deactivate-passwd))
	   (if (/= retval 0)
	       (error "Error while encrypting. Hit C-x u to undo.")))
	  (t
	   (error "Encryption scheme %s not recognized" scheme)))))
