;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sf-snarf.el -- snarf old documentation
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser <dewey@newvision.com>
;; Created On      : Sun Feb 23 09:17:19 1997
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defun sf::is-blank-field (field)
;;   (defun sf::get-fields (proplist)
;;   (defun sf::remove-header-prefix (string qheader-prefix-string)
;;   (defun sf::find-header-end (prefix-string)
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: sf-snarf.el,v $
;; $Revision: 1.3 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sf::is-blank-field (field)
  (or (not field)
      (string= "" field)
      (string= "|><|" field)))

(declaim (inline sf::is-blank-field))

(defun sf::get-fields (proplist)
  "Starting at the top of a function documentation, proceed to the end
snarfing the field names and values.  Use PROPLIST to determine if we
simply match the defaults"
  (let* ((header-prefix-string (header-prefix-string))
	 (qheader-prefix-string (regexp-quote header-prefix-string))
	 (end (sf::find-header-end qheader-prefix-string))
	 last
	 field
	 value
	 list
	 (qprefix-field (concat qheader-prefix-string
				     "\\([A-Z][A-Z ]*\\)[:\n]"))
	 )
    (flet ((advance () (if (>= (point) end)
			   (throw 'before-while nil)
			 (looking-at qprefix-field)))
	   (save-header (name num &optional end)
			(let ((default (sf::get-list-property proplist
							  name 'default-value))
			      (desc
			       (trim-whitespace
				(sf::remove-header-prefix
				 (if end
				     (buffer-substring-no-properties num end)
				   (match-buffer-substring-no-properties num))
				 qheader-prefix-string))))
			  (if (not (or (sf::is-blank-field desc)
				       (string= default desc))
				       )
			      (push (cons name desc) list)))))

      ;; find the first line
      (if
	  (re-search-forward (concat
			      (regexp-quote header-prefix-string)
			      "\\(\\(" c-token "\\|[ \t]\\)+\\)\\(()\\)? -- \\(.*\\)$")
			     end 't)
	  (progn
	    (save-header 'name 4)
	    (forward-char 1)
	    (catch 'before-while
	      (while (advance)
		(if last
		    (save-header (car last)
				 (cdr last)
				 (point)))
		(setq last (cons (match-buffer-substring-no-properties 1)
				 (match-end 0)))
		(forward-line 1)
		(while (not (advance))
		  (forward-line 1))
		)))
	
	  
	(if last
	    (save-header (car last)
			 (cdr last)
			 (point)))))
    (reverse list)))




	  
(defun sf::remove-header-prefix (string qheader-prefix-string)
  "Remove from STRING each QHEADER-PREFIX-STRING following a new line"
  (let ((real-string (concat (trim-trailing-whitespace qheader-prefix-string)
			     " ? ? ?")))
    (if (sf::is-blank-field string)
	""  
      (save-match-data
	(in-temp-buffer
	 (insert string)
	 (goto-char (point-min))
	 (while (looking-at real-string)
	   (replace-match "")
	   (forward-line 1)
	   )
	 (buffer-substring (point-min) (point-max)))))))


(defun sf::find-header-end (prefix-string)
  "Return the point at the end of the header"
  (let ((real-prefix-string (trim-trailing-whitespace prefix-string)))
    (save-excursion
      (if (looking-at (regexp-quote (trim-whitespace
				     comment-start)))
	  (forward-line 1))
      (while (looking-at real-prefix-string)
	(forward-line 1))
      (if (looking-at (regexp-quote comment-end))
	  (forward-line 1))
      (point))))


