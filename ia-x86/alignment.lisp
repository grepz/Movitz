;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 2001-2002, 2004, 
;;;;    Department of Computer Science, University of Tromso, Norway.
;;;; 
;;;;    For distribution policy, see the accompanying file COPYING.
;;;; 
;;;; Filename:      alignment.lisp
;;;; Description:   
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Wed Apr 10 12:47:08 2002
;;;;                
;;;; $Id: alignment.lisp,v 1.3 2004/02/10 00:03:09 ffjeld Exp $
;;;;                
;;;;------------------------------------------------------------------

(in-package #:ia-x86)

(defclass alignment ()
  ((type
    :initarg :type
    :reader alignment-type)))

(defmethod create-alignment ((alignment alignment) pc)
  (ecase (first (alignment-type alignment))
    (:code
     ;; For optimal performance across the Intel Architecture family, it is recommended that:
     ;; *  Loop entry labels should be 16-byte aligned when less than eight bytes away from a
     ;;    16-byte boundary.
     ;; *  Labels that follow a conditional branch should not be aligned.
     ;; *  Labels that follow an unconditional branch or function call should be 16-byte aligned
     ;;    when less than eight bytes away from a 16-byte boundary.
     (ecase (second (alignment-type alignment))
       (:loop
	 (when (< 8 (mod pc 16))
	   (nthcdr (mod pc 16) '#.(make-list 16 :initial-element #c(#x90 1))))
	 )))))


