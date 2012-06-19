;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nvscomp.el -- compile all files in nvs distribution.
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;; 
;; Author          : Dewey M. Sasser
;; Created On      : Wed Jun 22 13:00:04 1994
;; Status          : $State: Exp $
;; Name            : $Name: HEAD $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defvar nvs-comp-files '("alists"
;;   (defun compile-file-if-necessary (file)
;;   (defun compile-nvs ()
;; 
;; $RCSfile: nvscomp.el,v $
;; $Revision: 1.22 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar nvs-comp-files '("alists"
			 "cl-read"
			 ("cclass" 't)
			 "autoproto"
			 "whitesp"
			 "block-cm"
			 "c-boxes"
			 "fcreate"
			 "c-stuff"
			 "c-support"
			 "cando"
			 "comm-align"
			 "align-eq"
			 "align-regexp"
			 "convert"
			 "date"
			 "faceedit"
			 "functions"
			 "classdoc"
			 "fundoc"
			 "generic-code"
			 "header"
			 "instalit"
			 "makedocs"
			 "mk-comp"
			 "modefn"
			 "nvs-auto"
			 "nvs-menu"
			 "nvs"
			 "par-align"
			 "protos"
			 "testsup"
			 "wsearch"
			 "msvt"
			 "mifmode"
			 "multi-mode"
			 "c++-tools"
			 "idl-mode"
			 "spellsup"
			 "diredsup"
			 ))


(defun compile-file-if-necessary (file)
  "Compile the elisp file FILE if necessary.

This is done if FILE.el is newer than FILE.elc or if FILE.elc doesn't exist."
  (let* ((load-first (if (listp file) (nth 1 file)))
	 (file (if (listp file) (first file) file))
	 (el-name (concat file ".el"))
	 (elc-name (concat file ".elc")))
    (if (or (not (file-exists-p elc-name))
	    (file-newer-than-file-p el-name elc-name))
	(progn
	  (if load-first (load-file el-name))
	  (message (format "Byte-compiling %s..." el-name))
	  (byte-compile-file el-name)))
    (load-library file)))


(defun compile-nvs ()
  "Byte-compile all uncompiled files of nvs elisp distribution."

  ;; Be sure to have . in load-path since a number of files in elib
  ;; depend on other files and we always want the newer one even if
  ;; a previous version of elib exists.

  (interactive)
  (let ((load-path (append '(".") load-path))
	(byte-compile-dynamic-docstrings nil))
    (mapcar (function compile-file-if-necessary)
	    nvs-comp-files)))



;;(require 'cl)
;;(defun compile-loaded-files ()
;;  "Byte-compile all buffers whose names end in .el"
;;  (interactive)
;;  (loop for x being the buffers
;;	if (and (buffer-file-name x)
;;		(string-match "\\.el$" (buffer-file-name x)))
;;	do (byte-compile-file (buffer-file-name x))))
			 