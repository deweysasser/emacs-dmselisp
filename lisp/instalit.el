;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; instalit.el -- definition for instalit mode
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser
;; Created On      : Fri Jun 16 15:30:03 1995
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defvar instalit-mode-types '(
;;   (defvar instalit-mode-keywords '(
;;   (defvar instalit-bvd-keywords
;;   (defvar instalit-language-font-lock-keywords
;;   (defvar instalit-inf-font-lock-keywords
;;   (defvar instalit-bvd-font-lock-keywords
;;   (defvar instalit-mode-font-lock-keywords
;;   (defvar instalit-bvd-mode-font-lock-keywords
;;   (define-derived-mode instalit-mode generic-code-mode "Instalit"
;;   (define-derived-mode instalit-bvd-mode instalit-mode "InstalitBVD"
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: instalit.el,v $
;; $Revision: 1.5 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'generic-code)

;;;###autoload (autoload 'instalit-mode "instalit"   "Major mode for editing instalit scripts.\n\n\\{instalit-mode-map}" 't)

(defvar instalit-mode-types '(
			      "Array"
			      "Bitmap"
			      "LText"
			      "Number"
			      "Text"
			      "TextArray"
			      "Logical"
			      "Directory"
			      ))

(defvar instalit-mode-keywords '(
				 "@xy"
				 "BlackRect"
				 "DefPushButton"
				 "DefineVariables"
				 "Dialog"
				 "DialogBox"
				 "Disabled"
				 "Do"
				 "Else"
				 "EndDefineVariables"
				 "EndDialog"
				 "EndDialogBox"
				 "EndIf"
				 "EndProcedure"
				 "EndTextBox"
				 "Font"
				 "If"
				 "Procedure"
				 "PushButton"
				 "Returns"
				 "SetBackgroundColor"
				 "Static"
				 "TextBox"
				 "UseHeader"
				 "While"
				 "Windows95"
				 "WindowsNT"
				 "WindowsVersion"
				 ))

(defvar instalit-bvd-keywords
  '(
    "CreateOutputLibrary"
    "ExcludeFiles"
    "EndExcludeFiles"
    "AddFiles"
    "EndAddFiles"
    "FinishLibrary"
    ))

(defvar instalit-language-font-lock-keywords
  '(
    (":=\\(.*\\)" (1 font-lock-string-face nil niil))
    ("\\(:\\|<\\|>\\)=" . font-lock-keyword-face)
    ("\\[[a-zA-Z_]+[a-zA-Z_0-9]\\]" . font-lock-variable-name-face)
    ))

(defvar instalit-inf-font-lock-keywords
  '(
    ("\\(Array\|Bitmap\|Directory\|L\(Text\|ogical\)\|Number\|Text\(\|Array\)\\)" . font-lock-type-face)
    ("Procedure \\([a-zA-Z_]+[a-zA-Z_0-9]\\)"
     (1 font-lock-function-name-face keep nil))    ("\\(DialogBox\\|EndDialogBox\\|@xy\\|BlackRect\\|D\\(ef\\(PushButton\\|ineVariables\\)\\|i\\(alog\\(\\|Box\\)\\|sabled\\)\\|o\\)\\|E\\(lse\\|nd\\(D\\(efineVariables\\|ialog\\(\\|Box\\)\\)\\|If\\|Procedure\\|TextBox\\)\\)\\|Font\\|If\\|P\\(rocedure\\|ushButton\\)\\|Returns\\|S\\(etBackgroundColor\\|tatic\\)\\|TextBox\\|UseHeader\\|W\\(hile\\|indows\\(95\\|NT\\|Version\\)\\)\\)"
     ( 1 font-lock-keyword-face keep nil ))
    ))


(defvar instalit-bvd-font-lock-keywords
  '(
    (
    "\\(AddFiles\\|CreateOutputLibrary\\|E\\(nd\\(AddFiles\\|ExcludeFiles\\)\\|xcludeFiles\\)\\|FinishLibrary\\)"
    . font-lock-keyword-face)))
					   

(defvar instalit-mode-font-lock-keywords
  (append instalit-language-font-lock-keywords instalit-inf-font-lock-keywords))

(defvar instalit-bvd-mode-font-lock-keywords
  (append instalit-language-font-lock-keywords instalit-bvd-font-lock-keywords))

(define-derived-mode instalit-mode generic-code-mode "Instalit"
  "Major mode for editing instalit scripts.

\\{instalit-mode-map}"
  (setq comment-start ";")
  (setq comment-end "")
  (modify-syntax-entry 59 "<")
  (modify-syntax-entry ?\n ">")
  (modify-syntax-entry ?\f ">")
  (setq comment-start-skip ";+ *")
  (make-local-variable 'tab-stop-list)
  (setq tab-stop-list '(2 4 6 8))
  (setq font-lock-defaults '(instalit-mode-font-lock-keywords))
  )

(define-derived-mode instalit-bvd-mode instalit-mode "InstalitBVD"
  "Major mode for editing Instalit BVD Files"
  (setq font-lock-defaults '(instalit-bvd-mode-font-lock-keywords)))

(provide 'instalit)
