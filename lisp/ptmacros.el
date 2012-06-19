;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ptmacros.el -- Some commonly used macros
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser <dewey@newvision.com>
;; Created On      : Fri Apr 11 22:25:28 1997
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defmacro in-buffer (theBuf &rest body)
;;   (defmacro in-temp-buffer (&rest body)
;;   (defmacro in-temp-buffer-same-mode (&rest body)
;;   (defmacro with-output-to-buffer (buffer &rest body)
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: ptmacros.el,v $
;; $Revision: 1.7 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defmacro in-buffer (theBuf &rest body)
  "Execute execute in THEBUF forms BODY
THEBUF must be a buffer or the name of an existing buffer"
  ` (let ((buf (current-buffer))
	   (new-buf , theBuf))
    (set-buffer new-buf)
;;    (set-window-buffer (selected-window) (current-buffer))
    (unwind-protect
	(progn ,@ body)
      (set-buffer buf)
;;      (set-window-buffer (selected-window) (current-buffer))
      )))

(put 'in-buffer 'lisp-indent-function 1)


;;;###autoload
(defmacro in-temp-buffer (&rest body)
  ` (let ((buf (current-buffer))
	(new-buf (generate-new-buffer " *temp* (in-temp-buffer)")))
    (set-buffer new-buf)
;;    (set-window-buffer (selected-window) (current-buffer))
    (unwind-protect
	(progn ,@ body)
      (set-buffer buf)
;;      (set-window-buffer (selected-window) (current-buffer))
      (kill-buffer new-buf))))

;;;###autoload
(defmacro in-temp-buffer-same-mode (&rest body)
  ` (let (ret-val
	   (buf (current-buffer))
	   (mode major-mode)
	   new-buf)
       (unwind-protect
	   (progn
	   (setq new-buf (generate-new-buffer " *temp* (in-temp-buffer)"))
	   (set-buffer new-buf)
;;	   (set-window-buffer (selected-window) (current-buffer))
	   (funcall mode)
	   (setq ret-val (progn ,@ body)))
	 (set-buffer buf)
;;	 (set-window-buffer (selected-window) (current-buffer))
	 (kill-buffer new-buf))
       ret-val))


;;;###autoload
(defmacro with-output-to-buffer (buffer &rest body)
  "Evaluate body with buffer as standard output"
  `(let ((standard-output ,buffer))
     ,@body))

(put 'with-output-to-buffer 'lisp-indent-function 1)

(provide 'ptmacros)
