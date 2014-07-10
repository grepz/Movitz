;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 20012000, 2003-2004, Frode Vatved Fjeld
;;;; 
;;;; Filename:      utilities.lisp
;;;; Description:   
;;;; Author:        Nikodemus Siivola
;;;; Created at:    Sun Nov  2 14:51:21 EET 2003
;;;; Distribution:  See the accompanying file COPYING.
;;;;                
;;;; $Id: utilities.lisp,v 1.3 2004/02/10 00:04:24 ffjeld Exp $
;;;;                
;;;;------------------------------------------------------------------

;; FIXME: There is probably a better place in the great scheme of
;; things for stuff like this...
(in-package #:ia-x86)

;; Unfortunately ANSI doesn't specify the slots and initargs of
;; STYLE-WARNINGS... but we don't want to signal full WARNIGN either,
(define-condition style-note (style-warning)
  ((format :reader style-note-format :initarg :format)
   (args :reader style-note-args :initarg :args))
  (:report (lambda (condition stream)
	     (apply #'format 
		    stream 
		    (style-note-format condition) 
		    (style-note-args condition)))))

(defun style-note (format &rest args)
  (signal 'style-note :format format :args args))

;; This is def-... for now, since that's the style followed elsewhere
;; in the assembler.
(defmacro def-equal-constant (name expr &optional doc)
  "ANSI allows only EQL constants. Most lisps swallow EQUAL constants
as well, but not all. Hence this portability device."
  #+acl `(defconstant ,name ,expr ,doc)
  #+sbcl `(sb-impl::defconstant-eqx ,name ,expr #'equal ,doc)
  #-(or acl sbcl)
  `(progn
     (style-note "DEF-EQUAL-CONSTANT not defined for this implementation. ~
                  Using DEFPARAMETER instead.")
     (defparameter ,name ,expr ,doc)))
