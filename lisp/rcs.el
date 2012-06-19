;; $Id: rcs.el,v 1.4 1997/02/19 09:44:40 dewey Exp $
;;
;;
;;
(provide 'rcs)

;; RCS check in 
(defun rcs-check-in (name message)
  "Checks a buffer in to an RCS file.  Uses message as the log message.\
  It is an error for the RCS file not to exist"
  (interactive "bEnter buffer to check in:\nsEnter Log Message:")
  (save-buffer)
  (if (call-process  "ci" nil nil nil " -l -m\"" message "\" " name)
      (message "Sucessfully completed")
    (message "Error checking in file %s." name))
  (revert-buffer 't 't))

(defun rcs-check-out (name)
  "Checks out a file kept by RCS.  Use this to find an rcs file. \
It will not realize of there is not an existing rcs file."
  (interactive "sEnter File to Check Out:")
  (if (call-process "co" nil nil nil "-l " name)
      (find-file-other-window name)
    (message "Error Checking out %s." name)))

  