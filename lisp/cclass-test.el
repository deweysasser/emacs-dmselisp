;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cclass-test.el -- 
;; 
;; Copyright 2012 Dewey M. Sasser  All Rights Reserved
;;         Unpublished, Confidential and Proprietary
;; 
;; Author          : Dewey M. Sasser <dewey@newvision.com>
;; Created On      : Sat Feb 15 10:10:44 1997
;; Status          : $State: Exp $
;; Keywords        : 
;; PURPOSE
;; 	|>Description of modules purpose<|
;; 
;; TABLE OF CONTENTS
;;   (defcclass person
;;   (defcclass (astronaut (:include person))
;;   (defcclass (mission-specialist (:include astronaut))
;;   (defcclass (senator-in-space (:include mission-specialist))
;;   (defcmethod print-name ((p person))
;;   (defcmethod print-name :before  ((m mission-specialist))
;;   (defcmethod print-name :around ((a astronaut))
;;   (defcmethod print-name :after ((s senator-in-space))
;; 
;;  OPTIONS
;;    Update Tests:  yes
;; 
;; $RCSfile: cclass-test.el,v $
;; $Revision: 1.2 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defcclass person
  name
  age)

(defcclass (astronaut (:include person))
  position)

(defcclass (mission-specialist (:include astronaut))
  specialty)

(defcclass (senator-in-space (:include mission-specialist))
  district)


(defcmethod print-name ((p person))
  (insert (person-name p)))

(defcmethod print-name :before  ((m mission-specialist))
  (insert "Dr. "))

(defcmethod print-name :around ((a astronaut))
  (insert "the amazing ")
  (let ((p (copy-sequence a)))
    (setf (person-name p) "Dewey Sasser")
    (call-next-method p))
  (insert " so there"))

(defcmethod print-name :after ((s senator-in-space))
  (insert " Senator from " (senator-in-space-district s)))

;;(defcmethod print-name :after :mode emacs-lisp-mode ((p person))
;;  (insert " Doing Lisp!"))


;;(defcmethod print-name :after :mode (emacs-lisp-mode c-mode) ((p senator-in-space))
;;  (insert " and Lisping"))

(setq s (make-instance 'senator-in-space :name "Jones" :district "NC"))


(print-name s)








