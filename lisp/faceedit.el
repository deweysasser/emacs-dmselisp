;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; faceedit.el -- major mode for editing faces
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser
;; Created On      : Wed Mar 13 16:12:10 1996
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defvar face-edit-mode-map nil
;;   (defvar face-edit-history-list nil
;;   (defun face-edit-mark ()
;;   (defun face-edit-clear-mark ()
;;   (defun face-edit-insert-mark (mark)
;;   (defun fe::valid-face-line ()
;;   (defmacro defFaceModify (&rest names)
;;   (defFaceModify font foreground background)
;;   (defun face-edit-modify (attribute)
;;   (defun fe::find-marked-or-current-face ()
;;   (defun fe::find-marked-faces ()
;;   (defun fe::refresh-line ()
;;   (defun fe::refresh-buffer ()
;;   (defun fe::find-current-face ()
;;   (defun fe::find-argument (arg)
;;   (define-derived-mode face-edit-mode fundamental-mode "Face Edit"
;;   (defun edit-faces ()
;;   (defun fe::insert-faces ()
;;   (defun fe::insert-face-description (face &optional inhibit-newline)
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: faceedit.el,v $
;; $Revision: 1.5 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (string-lessp emacs-version "19.30")
    (require 'backquote))

(eval-when (compile)
  (require 'mk-comp))

(defvar face-edit-mode-map nil
  "Keymap for face edit mode")

(defvar face-edit-history-list nil
  "History list")

(if face-edit-mode-map
    ()
  (setq face-edit-mode-map (make-sparse-keymap))
  (define-key face-edit-mode-map "c" 'face-edit-set-font)
  (define-key face-edit-mode-map "f" 'face-edit-set-foreground)
  (define-key face-edit-mode-map "b" 'face-edit-set-background)
  (define-key face-edit-mode-map "m" 'face-edit-mark)
  (define-key face-edit-mode-map "u" 'face-edit-clear-mark)  
  (define-key face-edit-mode-map "g" 'fe::refresh-buffer)
  (define-key face-edit-mode-map "i" 'face-edit-make-italic))

(defun face-edit-mark ()
  "Mark the current face"
  (interactive)
  (face-edit-insert-mark ?*))

(defun face-edit-clear-mark ()
  "Clear the current mark"
  (interactive)
  (face-edit-insert-mark ? ))
  

(defun face-edit-insert-mark (mark)
  "Mark the current face"
  (if (fe::valid-face-line)
      (allow-buffer-changes
       (save-excursion
	 (beginning-of-line)
	 (insert mark)
	 (delete-char 1))
       (forward-line 1))))

(defun fe::valid-face-line ()
  "Return 't if line is a valid face line"
  't)

(defmacro defFaceModify (&rest names)
  `(progn
     ,@(mapcar 'fe::generate-func
	    names)))

(eval-when (compile load)
  (defun fe::generate-func (name)
    (let* ((name-str  (symbol-name name))
	   (fn-symbol (intern (concat "face-edit-set-" name-str)))
	   (documentation-string (concat "Set the current faces " name-str ".")))
      `(defun ,fn-symbol ()
	 ,documentation-string
	 (interactive)
	 (face-edit-modify ,name-str)))))


(defFaceModify font foreground background)


(defun face-edit-modify (attribute)
  "Modify a face's attribute"
  (let ((function (intern (concat "set-face-" attribute)))
	(faces (fe::find-marked-or-current-face))
	(arg (fe::find-argument attribute)))
    (if (string= arg "nil")
	(setq arg nil))
    (mapc '(lambda (face)
	     (funcall function face arg))
	  faces)
    (fe::refresh-buffer)))

(defun fe::find-marked-or-current-face ()
  "Find list of marked faces, or current face if none are marked"
  (let ((faces (fe::find-marked-faces)))
    (if (null faces)
	(list (fe::find-current-face))
      faces)))

(defun fe::find-marked-faces ()
  "Find list of marked faces"
  (let ((faces nil))
    (save-excursion
      (beginning-of-buffer) 
      (while (re-search-forward "^\\*" nil 't)
	(push (fe::find-current-face) faces)))
    (reverse faces)))

;; for with-output-to-buffer
(require 'ptmacros)

(defun fe::refresh-line ()
  "Refresh the current line"
  (allow-buffer-changes
   (with-output-to-buffer (current-buffer)
   (let ((this-face  (fe::find-current-face)))
     (save-excursion
       (beginning-of-line)
       (kill-line)
       (fe::insert-face-description this-face 't))))))


(defun fe::refresh-buffer ()
  "Refresh the current buffer"
  (interactive)
  (allow-buffer-changes
   (erase-buffer)
   (fe::insert-faces)
   (beginning-of-buffer)))

(defun fe::find-current-face ()
  "Return the name of the current face, as a symbol"
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward "* ")
    (read (current-buffer))))

(defun fe::find-argument (arg)
  "Find argument (color, font, etc)"
  (read-from-minibuffer (concat "Give me a " arg ": ")
			nil nil nil
			face-edit-history-list))

  
(define-derived-mode face-edit-mode fundamental-mode "Face Edit"
  "Major mode for editing faces"
  (toggle-read-only 1))

;;;###autoload
(defun edit-faces ()
  "Edit faces"
  (interactive)
  (let ((buf (generate-new-buffer "*Faces*")))
    (set-buffer buf)
    (set-window-buffer (selected-window) buf)
    (fe::insert-faces)
    (beginning-of-buffer)
    (set-buffer-modified-p nil)
    (face-edit-mode)))
  



(defun fe::insert-faces ()
  "List all and attributes"
  (interactive)
  (with-output-to-buffer (current-buffer)
    (princ (format "  %-40s%-15s%-15s%-15s\n" "face" "foreground" "background" "font"))
    (princ (make-string 85 ?-))
    (princ "\n")
    (mapc 'fe::insert-face-description
     (face-list))))

(defun fe::insert-face-description (face &optional inhibit-newline)
  "Insert a face description"
  (princ (format "  %-40s%-15s%-15s%-15s"
			    (face-name face)
			    (face-foreground face)
			    (face-background face)
			    (face-font face)))
  (unless inhibit-newline
    (princ "\n")))