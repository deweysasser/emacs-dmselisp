;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nvs.el -- nvs customizations
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;; 
;; Author          : Dewey M. Sasser
;; Created On      : Wed May 04 13:03:05 1994
;; Status          : $State: Exp $
;; Name            : $Name: HEAD $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defvar borland-c-error-regexp nil
;;   (defvar gcc-error-regexp nil
;;   (defvar byacc-error-regexp nil
;;   (defvar microsoft-error-regexp nil
;;   (defun revert-buffer-noask ()
;;   (defun auto-fill-on ()
;;   (defun auto-fill-off ()
;;   (defun insert-user-full-name-and-date ()
;;   (defun end-and-newline-and-indent ()
;;   (defun save-all-buffers () "\
;;   (defun kill-buffer-delete-window ()
;;   (defun kill-email-address (prefix)
;;   (defvar NVS-documentation-version   "1.9.5"
;; 
;; $RCSfile: nvs.el,v $
;; $Revision: 1.27 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Elmakedoc:  Automatic Prototyping Destinations
;  functions: nvs.doc
;  variables: nvs.doc

;; Patches for 18
(if (string= "18"
	     (substring emacs-version 0 2))
    (load-library "patches.elc"))

(require 'modefn)

(defvar borland-c-error-regexp nil
  "*Error Regexp used for parsing Borland C/C++ Errors")

(defvar gcc-error-regexp nil
    "*Error Regexp used for parsing GCC Errors")

(defvar byacc-error-regexp nil
    "*Error Regexp used for parsing Berkley YACC Errors")

(defvar microsoft-error-regexp nil
      "*Error Regexp used for parsing MicroSoft Errors")

;;; Load auto-loads.  Using auto loads should significantly speed up startup
(require 'nvs-auto)


;;; Header Customizations
;;(setq inhibit-email-address 't) ; change when NVS gets on the internet

(copyright-notice-on)
(setq header-copyright-notice
      (concat "\nCopyright " (substring (current-time-string) 20 24)
	      " New Vision Systems, Inc.  All Rights Reserved\n"
	      "        Unpublished, Confidential and Proprietary\n"))


;;(global-set-key "\C-x\C-a" 'save-all-buffers)

;;; Dont question a revert-buffer
(defun revert-buffer-noask ()
  "Revert the buffer without prompting for yes or no."
  (interactive)
  (revert-buffer 't 't))

;;; Turn on and off auto-fill-mode
(defun auto-fill-on ()
  "Turn on auto fill mode."
  (interactive)
  (auto-fill-mode 1))

(defun auto-fill-off ()
  "Turn off auto-fill-mode."
  (interactive)
  (auto-fill-mode -1))

(defun insert-user-full-name-and-date ()
  "Insert name and date into buffer at current point."
  (interactive)
  (insert (user-full-name) " ")
  (insert-current-date))



;; TODO:  put these in the defvar's
(setq microsoft-error-regexp
      '(
	"^\\([a-zA-Z0-9_.:\\\\]*\\)(\\([0-9]*\\)).*$"
	1 2))

(setq borland-c-error-regexp
      '(
	"^\\(Error\\|Warning\\)[ ]+\\(.*\\)[ ]+\\([0-9]+\\): "
	2 3))

(setq gcc-error-regexp
      '(
	"\n\\([^:( \t\n]+\\)[:(][ \t]*\\([0-9]+\\)[:) \t]"
	1 2))

(setq byacc-error-regexp
      '(
	"YACC: . - line \\([0-9]+\\) of \"\\([^\"]*\\)\""
	2 1))


(defun end-and-newline-and-indent ()
  "Go to end of line, then insert newline and indent. \
This function is useful for hitting return while in a comment."
  (interactive)
  (let (
	(eol nil)
	(bol nil)
	(test nil)
	)
      (save-excursion			; find the end of line
	(end-of-line)
	(setq eol (point))
	(beginning-of-line)
	(setq bol (point)))
      (save-excursion
	(setq test (search-backward comment-start (- bol 1) 't)))
      (if test
	  (if (search-forward comment-end eol 't)
	      (end-of-line)))
      (newline-and-indent)))

;;; Add a "Save All" command
(defun save-all-buffers () "\
Saves all buffers with an associated file by calling
(save-some-buffers 1)"
  (interactive)
  (save-some-buffers 1 ))

;;; Make sure all files save before a suspend
;(add-hook 'suspend-hook 'save-all-buffers)

;;; Make a "delete all and kill window command, add it to
;;; keymap in C-xC-k
(defun kill-buffer-delete-window ()
  "Kills the current buffer and deletes it's window."
  (interactive)
  (kill-buffer nil)
  (delete-window))


(require 'fcreate)

;; Test for the package, first
;; called when files matching regexp are created
(if (and
     (featurep 'fcreate)		; file creation hooks
     (not (featurep 'nvs)))		; nvs not loaded before
    (progn
      (fcreate-add-to-creation-alist "\\.h$" 'make-header)
      (fcreate-add-to-creation-alist "\\.c$" 'make-header)))

;; remove email addresses from header stuff
(defun kill-email-address (prefix)
  "Remove the <user@system> from the author lines.
Removes only email address of current user at current system.  Any prefix
(C-u \\[kill-email-address]) removes line containing the current user
with any system."
  (interactive "P")
  (save-excursion
    (goto-char 0)
    (replace-regexp (concat
		     (regexp-quote
		      (concat
		       "<"
		       (user-login-name)
		       "@"))
		     (if prefix
			 ".*"
		       (regexp-quote (system-name)))
		     ">") "")))


(defvar NVS-documentation-version   "1.9.5"
  "Return Elisp documentation library version number.")

;;; Turn on menus
(if (boundp 'window-system)
    (load-library "nvs-menu"))


;;; Microsoft visual test mode
(prepend-to-list auto-mode-alist '("\\.mst$" . ms-visual-test-mode))
(prepend-to-list auto-mode-alist '("\\.mif$" . mif-mode))
(prepend-to-list auto-mode-alist '("\\.idl$" . idl-mode))
(prepend-to-list auto-mode-alist '("\\.inf$" . instalit-mode))
(prepend-to-list auto-mode-alist '("\\.bvd$" . instalit-bvd-mode))
(prepend-to-list auto-mode-alist '("\\.plog" . pure-mode))


(if (and
     (featurep 'fcreate)		; file creation hooks
     (not (featurep 'c-stuff)))		; c-stuff not loaded before
    (progn
      (fcreate-add-to-creation-alist "\\.h$" 'insert-header-wrapper)
      (fcreate-add-to-creation-alist "\\.c$" 'c-mode-insert-copyright)))

(provide 'nvs)