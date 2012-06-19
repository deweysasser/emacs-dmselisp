;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stopwtch.el -- Stopwatches for Emacs Lisp
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser <dewey@newvision.com>
;; Created On      : Tue Mar 04 20:00:51 1997
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defsubst stopwatch-create ()
;;   (defsubst stopwatch-elapsed (sw)
;;   (defsubst stopwatch-last-start (sw)
;;   (defsubst stopwatch-set-elapsed (sw el)
;;   (defsetf stopwatch-elapsed stopwatch-set-elapsed)
;;   (defsubst stopwatch-set-last-start (sw st)
;;   (defsetf stopwatch-last-start stopwatch-set-last-start)
;;   (defsubst stopwatch-p (sw)
;;   (defsubst stopwatch-get-time ()
;;   (defsubst stopwatch-start (sw)
;;   (defsubst stopwatch-stop (sw)
;;   (defsubst stopwatch-reset (sw)
;;   (defsubst stopwatch-create-running ()
;;   (defmacro with-paused-stopwatch (stopwatch-symbol &rest body)
;;   (defmacro time-body (&rest body)
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: stopwtch.el,v $
;; $Revision: 1.2 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)

(defsubst stopwatch-create ()
  "Create a stopwatch"
  (vector 'stopwatch 0 nil))

(defsubst stopwatch-elapsed (sw)
  "Get the elapsed amount"
  (aref sw 1))

(defsubst stopwatch-last-start (sw)
  "Get the time last started"
  (aref sw 2))

(defsubst stopwatch-set-elapsed (sw el)
  "Set the elapsed time"
  (aset sw 1 el))

(defsetf stopwatch-elapsed stopwatch-set-elapsed)

(defsubst stopwatch-set-last-start (sw st)
  "Set the time last started"
  (aset sw 2 st))

(defsetf stopwatch-last-start stopwatch-set-last-start)

(defsubst stopwatch-p (sw)
  "Return 't if object is a stopwatch"
  (and (vectorp sw)
       (eq (aref sw 0) 'stopwatch)))

(defsubst stopwatch-get-time ()
  ;; get current time in seconds and microseconds. I throw away the
  ;; most significant 16 bits of seconds since I doubt we'll ever want
  ;; to profile lisp on the order of 18 hours. See notes at top of file.
  (let ((now (current-time)))
    (+ (float (nth 1 now)) (/ (float (nth 2 now)) 1000000.0))))  

(defsubst stopwatch-start (sw)
  "Start a stopwatch"
  (if (stopwatch-last-start sw)
      ;; it's already started
      nil
    (setf (stopwatch-last-start sw) (stopwatch-get-time))))


(defsubst stopwatch-stop (sw)
  "Stop a stopwatch"
  (let ((now (stopwatch-get-time)))
    (setf (stopwatch-elapsed sw)
	  (+ (stopwatch-elapsed sw)
	     (- now (stopwatch-last-start sw))))
    (setf (stopwatch-last-start sw) nil)
    (stopwatch-elapsed sw)))


(defsubst stopwatch-reset (sw)
  "Reset a stopwatch"
  (setf (stopwatch-elapsed sw) 0))

(defsubst stopwatch-create-running ()
  "Create a stopwatch and start it"
  (let ((sw (stopwatch-create)))
    (stopwatch-start sw)
    sw))


(defmacro with-paused-stopwatch (stopwatch-symbol &rest body)
  "Set things up so that stopwatch-symbol is a stopwatch, but is
paused if this body is reentered"
  `(let ((nested (and (boundp ',stopwatch-symbol)
		      (stopwatch-p ,stopwatch-symbol))))
     (if nested
	 (stopwatch-stop ,stopwatch-symbol))
     (unwind-protect
	 (let ((,stopwatch-symbol (stopwatch-create-running)))
	   ,@body)
       (if nested
	   (stopwatch-start ,stopwatch-symbol)))))

(defmacro time-body (&rest body)
  `(let ((stopwatch (stopwatch-create-running)))
     ,@body
     (stopwatch-stop stopwatch)))

(provide 'stopwtch)
(provide 'stopwatch)