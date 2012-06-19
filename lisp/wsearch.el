;;; Word search functions
;;; by Dewey Sasser
;;; Tue Oct 19 19:23:46 1993

;;; $Id: wsearch.el,v 1.9 2000/06/19 15:46:03 dewey Exp $
;;;

;Elmakedoc:  Automatic Prototyping Destinations
;  functions: wsearch.doc
;  variables: wsearch.doc

(provide 'wsearch)
;;;###autoload
(defun search-backward-current-symbol ()
  "Searches for the preceding occurrence of the symbol under the current point."
  (interactive)
  (let* (
	 (begin (end-current-symbol))
	 (end   (begin-current-symbol))
	 (string (buffer-substring begin end)) ;clip out current symbol
	 )
    (push-mark)				; push a mark on the ring so that
					; you can get back to current point
    (search-backward string)))		; find last symbol

;;;###autoload
(defun search-forward-current-symbol ()
  "Searches for the preceding occurrence of the symbol under the current point."
  (interactive)
  (let* (
	 (begin (end-current-symbol))
	 (end   (begin-current-symbol))
	 (string (buffer-substring begin end))
	 )
    (push-mark)
    (search-forward string)))
;;;###autoload
(defun end-current-symbol ()
  "Return the location of the end of the current word."
  (save-excursion
    (while (or (eq (char-syntax (following-char)) ?w)
	       (eq (char-syntax (following-char)) ?_))
      (forward-char))
    (point)))
            
;;;###autoload
(defun begin-current-symbol ()
  "Return the location of the beginning of the current word."
  (save-excursion
    (while (or (eq (char-syntax (preceding-char)) ?w)
	       (eq (char-syntax (preceding-char)) ?_))
      (backward-char))
    (point)))
    
       
;;;###autoload 
(defun find-toc-entry ()
  "Find a function definition from a table of contents entry."
  (interactive)
  (let ((bol))
    (push-mark)
    (beginning-of-line)
;;    (re-search-forward (concat (regexp-quote comment-start) "*")  nil 't)
    (forward-char 2)
    (skip-syntax-forward " ")
    (setq bol (point))
    (end-of-line)
    (if (search-forward (buffer-substring bol (point)) nil 't)
	()
      (goto-char (mark))
      (pop-mark)
      (message "Could not locate reference"))))
	
    

