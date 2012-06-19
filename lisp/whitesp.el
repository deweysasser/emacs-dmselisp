;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; whitesp.el -- white space manipulation functions.
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser
;; Created On      : Fri Mar 08 13:36:22 1996
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defun trim-all-whitespace (string)
;;   (defun trim-whitespace (string)
;;   (defun trim-leading-whitespace (string)
;;   (defun trim-trailing-whitespace (string)
;;   (defun trailing-whitespace (string)
;;   (defun leading-whitespace (string)
;;   (defun whitespacep (ch )
;;   (defun last-char-of (string)
;;   (defun first-char-of (string)
;;   (defun doc-whitespacep (ch)
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: whitesp.el,v $
;; $Revision: 1.7 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)

(eval-when (compile)
  ;; only get macro definitions
  (require 'protos))

;;;###autoload
(defun trim-all-whitespace (string)
  "Process a string leaving only one space in place of any whitespace."
  (let ((count 0)
	(pos 0)
	(end (1- (length string)))
	(newstr))
    (if (string= string "")
	""
      (setq newstr (make-string (1+ end) 0))
      ;; first, kill all leading white space
      (while (doc-whitespacep (aref string pos))
	(setq pos (1+ pos)))
      ;; then the end
      (while (doc-whitespacep (aref string end))
	(setq end (1- end)))
      (setq end (1+ end))
      ;; now, go through the string, leaving only one whitespace character
      (while (< pos end)
	(if (doc-whitespacep (aref string pos))
	    (progn
	      (aset newstr count ? )
	      (while (doc-whitespacep (aref string pos))
		(setq pos (1+ pos)))
	      (setq pos (1- pos)))
	  (aset newstr count (aref string pos))
	  )
	(setq count (1+ count))
	(setq pos (1+ pos)))
      (substring newstr 0 count))))

;;;###autoload
;; (defun trim-whitespace (string)
;;   "Remove white space from both ends of a string"
;;   (if (or
;;        (string= "\n" string)
;;        (string= "" string))
;;       ""
;;     (let ((start 0)
;; 	  (end (1- (length string))))
;;       (if (= end -1)
;; 	  nil
;; 	(while (whitespacep (aref string start))
;; 	  (increment start))
;; 	(while (whitespacep (aref string end))
;; 	  (decrement end))
;; 	(substring string start (1+ end))))))

(defun trim-whitespace (string)
  "Remove white space from both ends of a string"
  (let* ((start 0)
	(end (length string))
	(max-start (1- end)))
    (if (< end 1)
	""
      (while (and (< start max-start)
		  (whitespacep (aref string start)))
	(increment start))
      (decrement end)
      (while (and (> end -1)
		  (whitespacep (aref string end)))
	(decrement end))
      (if (< end start)
	  ""
	(substring string start (1+ end))))))


;;;###autoload
(defun trim-leading-whitespace (string)
  "Remove the leading whitespace"
  (let ((l (1- (length string)))
	(here 0))
    (while (and
	    (< here l)
	    (whitespacep (aref string here)))
      (setq here (1+ here)))
    (substring string here)))

;;;###autoload
(defun trim-trailing-whitespace (string)
  "Remove the trailing whitespace"
  (let* ((l (1- (length string)))
	(here l))
    (while (and (>= here 0)	    
		(whitespacep (aref string here)))
      (setq here (1- here)))
    (substring string 0 (1+ here))))

;;;###autoload
(defun trailing-whitespace (string)
  "Return the trailing whitespace"
  (let* ((l (1- (length string)))
	(here l))
    (while (and (>= here 0)	    
		(whitespacep (aref string here)))
      (setq here (1- here)))
    (substring string (1+ here))))

;;;###autoload
(defun leading-whitespace (string)
  "Return the leading whitespace"
  (let ((l (1- (length string)))
	(here 0))
    (while (and
	    (< here l)
	    (whitespacep (aref string here)))
      (setq here (1+ here)))
    (substring string 0 here)))


;;;###autoload
(defun whitespacep (ch )
  "Predicate for whitespace."
  (or
   (= (char-syntax ch) ? )
   (= ch ?\n)))


;;;###autoload
(defun last-char-of (string)
  "Return the last character of STRING"
  (aref string (1- (length string))))
      

;;;###autoload
(defun first-char-of (string)
  "Return the first character of STRING"
  (aref string 0))

;;;###autoload
(defun doc-whitespacep (ch)
  (or (eq ch 10)
      (whitespacep ch)))
    
(provide 'whitesp)