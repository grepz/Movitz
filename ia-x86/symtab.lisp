;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 2000, 2004,
;;;;    Department of Computer Science, University of Tromso, Norway
;;;; 
;;;; Filename:      symtab.lisp
;;;; Description:   Assembly symbolic lookups.
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Tue Aug 22 10:01:38 2000
;;;; Distribution:  See the accompanying file COPYING.
;;;;                
;;;; $Id: symtab.lisp,v 1.3 2004/02/10 00:04:19 ffjeld Exp $
;;;;                
;;;;------------------------------------------------------------------

(in-package #:ia-x86)

;;; A symtab is a stack-organized list of "frames", where each frame
;;; is a hash-table.

(define-condition unresolved-labels ()
  ((labels :initarg labels
	   :reader unresolved-labels-labels))
  (:report (lambda (condition stream)
	     (format stream "Unable to resolve labels ~A."
		     (unresolved-labels-labels condition)))))

(defun make-symtab ()
  nil)

(defun symtab-add-frame (symtab)
  symtab)

(defun symtab-lookup-label (symtab label)
  (or (symtab-try-lookup-label symtab label)
      (error 'unresolved-labels 'labels (list label))))

(defun symtab-try-lookup-label (symtab label)
  (declare (special *symtab-lookup*))
  (or (cdr (assoc label symtab))
      (if (and (boundp '*symtab-lookup*)
	       *symtab-lookup*)
	  (funcall *symtab-lookup* label)
	nil)))

(defun symtab-def-label (symtab label value)
  (when (symtab-try-lookup-label symtab label)
    (error "Label ~A multiply defined." label))
  (acons label value symtab))

(defun symtab-collapse-frames (symtab)
  symtab)
