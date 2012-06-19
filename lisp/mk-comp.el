;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mk-comp.el -- mark compilation mode
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser
;; Created On      : Wed Mar 06 09:43:18 1996
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	This gives you a "compilation" mode this is sort of like dired mode in
;;      that you can mark, etc all the matches so that you can develop
;;      a list of the ones you want.
;; 
;; TABLE OF CONTENTS
;;   (defvar mkc::delete-flag "D"
;;   (defvar mkc::interest-flag "*"
;;   (defvar mkc-mode-map nil
;;   (defmacro allow-buffer-changes (&rest body)
;;   (defun mkc::mark-entry (mark-type &optional entry-number)
;;   (defmacro mkc::make-mark-function (mark-type)
;;   (defun mkc::remove-mark ()
;;   (defun mkc::expunge ()
;;   (defun mkc::delete-line ()
;;   (define-derived-mode mkc-mode text-mode
;;   (defun mark-compilation ()
;;   (defun mkc::save-buffer (&optional force)
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: mk-comp.el,v $
;; $Revision: 1.4 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (string-lessp emacs-version "19.30")
    (require 'backquote))

(defvar mkc::delete-flag "D"
  "Flag which marks entry for deletion")

(defvar mkc::interest-flag "*"
  "Flag which marks entry for special interest")

(defvar mkc-mode-map nil
  "Keymap for mkc-mode")


;;;###autoload
(defmacro allow-buffer-changes (&rest body)
  "Allow body to change buffer regardless of buffer's read-only state"
  `(let ((inhibit-read-only 't))
     ,@body))

(defun mkc::mark-entry (mark-type &optional entry-number)
  "Mark an entry"
  (let (end)
    (save-excursion
      (allow-buffer-changes      
       (if entry-number
	   (goto-line (+ entry-number 2)))
       (beginning-of-line)
       (setq end (save-excursion
		   (end-of-line)
		   (point)))
       (if (search-forward "(mkc: " end 't)
	   nil				;we're fine
	 (end-of-line) 
	 (insert "(mkc:  )")
	 (backward-char 2))
       (delete-char 1)
       (insert mark-type)))
    (next-line 1)))

(defmacro mkc::make-mark-function (mark-type)
  "Create the function which marks the argument for MARK-TYPE.  Note
that we're creating a function which is named
'mkc::mark-for-MARK-TYPE', and assumes a variable exists named
'mkc::MARK-TYPE-flag'" 
  (let* ((mark-name (symbol-name mark-type))
	 (fnname (intern (concat "mkc::mark-for-"
			 mark-name)))
	 (mark-variable (intern
			 (concat "mkc::"
				 mark-name
				 "-flag"))))
    
  `(defun ,fnname ()
     ,(concat "Mark an entry for " mark-name)
     (interactive)
     (mkc::mark-entry ,mark-variable))))


(mkc::make-mark-function delete)
(mkc::make-mark-function interest)

;;;###autoload (autoload 'mkc-mode-map "mk-comp" "keymap for mkc-mode" nil 'keymap)
(if mkc-mode-map
    ()
  (setq mkc-mode-map (make-sparse-keymap))
  (define-key mkc-mode-map "m" 'mkc::mark-for-interest)
  (define-key mkc-mode-map "d" 'mkc::mark-for-delete)
  (define-key mkc-mode-map "s" 'mkc::save-buffer)
  (define-key mkc-mode-map "u" 'mkc::remove-mark)
  (define-key mkc-mode-map "n" 'next-line)
  (define-key mkc-mode-map "p" 'previous-line)
  (define-key mkc-mode-map "x" 'mkc::expunge))



(defun mkc::remove-mark ()
  "Remove the mark on the current line"
  (interactive)
  (save-excursion
    (beginning-of-line) 
    (let ((end (save-excursion (end-of-line) (point) )))
      (allow-buffer-changes
       (if (re-search-forward "(mkc: .)" end 't)
	   (replace-match "")))))
  (forward-line 1))

	 
(defun mkc::expunge ()
  "Expunge entries marked for deletion.  This function may screw
things up for future hits."
  (interactive)
  (allow-buffer-changes
   (let ((del-regexp (concat "(mkc: "  (regexp-quote mkc::delete-flag)
			     ")$")))
     (save-excursion
       (goto-char (point-min))
       (while (and
	       (< (point) (point-max))
	       (re-search-forward del-regexp nil
				  't))
	 (mkc::delete-line))))))

(defun mkc::delete-line ()
  "Delete the current line"
  (beginning-of-line) 
  (while (not (eq (char-after
	      (point))
	     ?\n))
    (delete-char 1))
  (delete-char 1))


;;;###autoload (autoload 'mkc-mode "mk-comp"   "Mode which allows you to mark specific compilation entries for\ndeletion or special interest" 't)

(define-derived-mode mkc-mode text-mode
  "Mark Compilation"
  "Mode which allows you to mark specific compilation entries for
  deletion or special interest"
  (toggle-read-only 1)
  (compilation-minor-mode 1))

(if (featurep 'hilit19)
    (hilit-set-mode-patterns
     '(mkc-mode)
     '(("^.*(mkc: \\*)" 0 dired-marked)
       ("^.*(mkc: D)" 0 dired-deleted))))


;;;###autoload
(defun mark-compilation ()
  "Run a compilation on the current buffer"
  (interactive)
  (call-interactively 'compile)
  (in-buffer "*compilation*"
	     (mkc-mode)))

(defun mkc::save-buffer (&optional force)
  "Save the current buffer"
  (interactive "P")
  (if (or force
	  (buffer-modified-p))
      (save-excursion
	(copy-file
	 (buffer-file-name)
	 (make-backup-file-name (buffer-file-name))
	 't				;ok if already exists
	 )
	(goto-char (point-min))
	(if (looking-at "^cd")
	    (forward-line 1))
	(if (looking-at "^cat")
	    (forward-line 1))
	(let ((start (point-marker)))
	  (end-of-buffer)
	  (search-backward "Mark Compilation finished" nil 't)
	  (write-region start (point) (buffer-file-name))
	  (set-buffer-modified-p nil)))
    (message "No changes need to be saved")))

(provide 'mk-comp)