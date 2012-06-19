;;; idl-mode.el --- major mode for editing IDL code
;;; (changed cc-mode.el)

;; Authors: 1992 Barry A. Warsaw <bwarsaw@cnri.reston.va.us>
;;          1987 Dave Detlefs and Stewart Clamen
;;          1985 Richard M. Stallman
;; Maintainer: cc-mode-help@anthem.nlm.nih.gov
;; Created: a long, long, time ago. adapted from the original c-mode.el
;; Version:         4.85
;; Last Modified:   1994/09/08 14:27:45
;; Keywords: C++ C Objective-C editing major-mode

;; Copyright (C) 1992, 1993, 1994 Barry A. Warsaw
;; Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.
;; ----------------------------------------------------------------------
;; Changed for idl-mode by Andrey A. Aristarkhov, Fintel, Moscow
;; E-mail:        dron@fintel.msk.su
;; Version:       1.01
;; Last Modified: 09/13/1996
;; Known bugs: TAB does not work. (fixed)
;;
;; Fixed bugs: 
;;        1. 'T' character (capital 'T') is know typable
;;           [bug report from John Koper <jrk@tsci.com>, September 1996]
;;        2. 'TAB' works now
;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
(require 'font-lock)
(require 'cc-mode)
(provide 'idl-mode)


(defvar idl-mode-hook nil
  "*Hook called by `idl-mode'.")

(defvar idl-mode-common-hook nil
  "*Hook called by `idl-mode' during common init.")

(defvar idl-mode-menu
  '(["Comment Out Region"     comment-region (mark)]
    ["Macro Expand Region"    c-macro-expand (mark)]
    ["Backslashify"           c-backslash-region (mark)]
    ["Indent Expression"      c-indent-exp
     (memq (following-char) '(?\( ?\[ ?\{))]
    ["Indent Line"            c-indent-command t]
    ["Fill Comment Paragraph" c-fill-paragraph t]
    ["Up Conditional"         c-up-conditional t]
    ["Backward Conditional"   c-backward-conditional t]
    ["Forward Conditional"    c-forward-conditional t]
    ["Backward Statement"     c-beginning-of-statement t]
    ["Forward Statement"      c-end-of-statement t]
    ["Mark function"          c-mark-function t]
    ["Indent region"          indent-region t]
    )
  "XEmacs 19 (formerly Lucid) menu for IDL modes.")

(defvar idl-mode-abbrev-table nil
  "Abbrev table in use in idl-mode buffers.")
(define-abbrev-table 'idl-mode-abbrev-table ())

(defun idl-mode-fsf-menu (name map)
  ;; Add FSF menu to a keymap.  FSF menus suck.  Don't add them for
  ;; XEmacs. This feature test will fail on other than FSF's Emacs 19.
  (condition-case nil
      (progn
	(define-key map [menu-bar] (make-sparse-keymap))
	(define-key map [menu-bar c] (cons name (make-sparse-keymap)))

	(define-key map [menu-bar c comment-region]
	  '("Comment Out Region" . comment-region))
	(define-key map [menu-bar c c-macro-expand]
	  '("Macro Expand Region" . c-macro-expand))
	(define-key map [menu-bar c c-backslash-region]
	  '("Backslashify" . c-backslash-region))
	(define-key map [menu-bar c indent-exp]
	  '("Indent Expression" . c-indent-exp))
	(define-key map [menu-bar c indent-line]
	  '("Indent Line" . c-indent-command))
	(define-key map [menu-bar c fill]
	  '("Fill Comment Paragraph" . c-fill-paragraph))
	(define-key map [menu-bar c up]
	  '("Up Conditional" . c-up-conditional))
	(define-key map [menu-bar c backward]
	  '("Backward Conditional" . c-backward-conditional))
	(define-key map [menu-bar c forward]
	  '("Forward Conditional" . c-forward-conditional))
	(define-key map [menu-bar c backward-stmt]
	  '("Backward Statement" . c-beginning-of-statement))
	(define-key map [menu-bar c forward-stmt]
	  '("Forward Statement" . c-end-of-statement))
	(define-key map [menu-bar c markfunc]
	  '("Mark function" . c-mark-function))
	(define-key map [menu-bar c indreg]
	  '("Indent region" . indent-region))

	;; RMS: mouse-3 should not select this menu.  mouse-3's global
	;; definition is useful in C mode and we should not interfere
	;; with that.  The menu is mainly for beginners, and for them,
	;; the menubar requires less memory than a special click.
	t)
    (error nil)))

(defvar idl-mode-syntax-table nil
  "Syntax table used in idl-mode buffers.")
(if idl-mode-syntax-table
    ()
  (setq idl-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table idl-mode-syntax-table)
  ;; add extra comment syntax
  (cond
   ((memq '8-bit c-emacs-features)
    ;; XEmacs (formerly Lucid) has the best implementation
    (modify-syntax-entry ?/  ". 1456" idl-mode-syntax-table)
    (modify-syntax-entry ?*  ". 23"   idl-mode-syntax-table)
    (modify-syntax-entry ?\n "> b"    idl-mode-syntax-table)
    ;; Give CR the same syntax as newline, for selective-display
    (modify-syntax-entry ?\^m "> b"    idl-mode-syntax-table))
   ((memq '1-bit c-emacs-features)
    ;; FSF Emacs 19 does things differently, but we can work with it
    (modify-syntax-entry ?/  ". 124b" idl-mode-syntax-table)
    (modify-syntax-entry ?*  ". 23"   idl-mode-syntax-table)
    (modify-syntax-entry ?\n "> b"    idl-mode-syntax-table)
    ;; Give CR the same syntax as newline, for selective-display
    (modify-syntax-entry ?\^m "> b"    idl-mode-syntax-table))
   )
  ;; TBD: does it make sense for colon to be symbol class in C++?
  ;; I'm not so sure, since c-label-key is busted on lines like:
  ;; Foo::bar( i );
  ;; maybe c-label-key should be fixed instead of commenting this out,
  ;; but it also bothers me that this only seems appropriate for C++
  ;; and not C.
  ;;(modify-syntax-entry ?: "_" c++-mode-syntax-table)
  )

(defvar idl-mode-map ()
  "Keymap used in idl-mode buffers.")

(if idl-mode-map
    ()
  ;; In Emacs 19, it makes more sense to inherit c-mode-map
  (if (memq 'v19 c-emacs-features)
      ;; XEmacs (formerly Lucid) and FSF Emacs 19 do this differently
      (if (not (fboundp 'set-keymap-parent))
	  (setq idl-mode-map (cons 'keymap c-mode-map))
	(setq idl-mode-map (make-sparse-keymap))
	(set-keymap-parent idl-mode-map c-mode-map))
    ;; Do it the hard way for Emacs 18 -- given by JWZ
    (setq idl-mode-map (nconc (make-sparse-keymap) c-mode-map)))
  ;; add binding which fix TAB-bug for idl-mode buffers
  ;; DMS: TODO Fix this
  (define-key idl-mode-map '[TAB]   'c-indent-command)
  ;; add bindings which are only useful for IDL
;  (define-key idl-mode-map "\C-i:"  'c-scope-operator)
  ;; FSF Emacs 19 defines menus in the mode map. This call will return
  ;; t on FSF Emacs 19, otherwise no-op and return nil.
  (c-mode-fsf-menu "IDL" idl-mode-map))

;;;###autoload
(defun idl-mode ()
  "Major mode for editing IDL code.
idl-mode Revision: 1.01

The hook variable `idl-mode-hook' is run with no args, if that
variable is bound and has a non-nil value.

Key bindings:
\\{IDL-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table idl-mode-syntax-table)
  (setq major-mode 'idl-mode
	mode-name "IDL"
	local-abbrev-table idl-mode-abbrev-table
	;; should be set before c-common-init call
	c-recognize-knr-p nil)
  (use-local-map idl-mode-map)
  (c-common-init)
  (setq comment-start "// "
	comment-end ""
	c-conditional-key c-C++-conditional-key
	c-comment-start-regexp "//\\|/\\*"
	c-class-key c-C++-class-key
	c-access-key c-C++-access-key)
  (run-hooks 'idl-mode-hook))

;; menus for XEmacs (formerly Lucid)
(defun idl-popup-menu (e)
  "Pops up the IDL menu."
  (interactive "@e")
  (popup-menu (cons (concat mode-name " Mode Commands") idl-mode-menu))
  (c-keep-region-active))

(provide 'idl-mode)
;;; idl-mode.el ends here


