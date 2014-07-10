;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 200120001999, 2002, 2004,
;;;;    Department of Computer Science, University of Tromso, Norway
;;;; 
;;;; Filename:      ia-x86.lisp
;;;; Description:   
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Fri Dec 17 18:01:26 1999
;;;; Distribution:  See the accompanying file COPYING.
;;;;                
;;;; $Id: ia-x86.lisp,v 1.4 2004/09/02 09:01:29 ffjeld Exp $
;;;;                
;;;;------------------------------------------------------------------

(in-package #:ia-x86)

;;; ----------------------------------------------------------------
;;;                    Instruction Definition
;;; ----------------------------------------------------------------

(defclass instruction ()
  ((operands
    :type list
    :initform '()
    :initarg :operands
    :accessor instruction-operands)
   (prefixes
    :type list
    :initarg :prefixes
    :initform '()
    :accessor instruction-prefixes)
   (user-size
    :initarg :user-size
    :reader instruction-user-size)
   (user-finalizer
    :initarg :user-finalizer
    :initform nil
    :reader instruction-finalizer)
   (original-datum
    :initarg :datum
    :accessor instruction-original-datum)))

(defmethod print-object ((obj instruction) stream)
  (cond
   (*print-readably*
    (format stream "(~@[~A ~]~A~{ ~A~})"
	    (instruction-prefixes obj)
	    (type-of obj)
	    (instruction-operands obj))
    obj)
   (t (print-unreadable-object (obj stream)
	(format stream "asm ~@[~A ~]~A~{ ~A~}"
		(instruction-prefixes obj)
		(type-of obj)
		(instruction-operands obj))))))



;;; ----------------------------------------------------------------
;;;            Symbolic break-down of encoded instructions
;;; ----------------------------------------------------------------

(defstruct instr-symbolic
  mod reg r/m
  scale index base
  displacement
  immediate
  opcode)

(defmethod print-object ((obj instr-symbolic) stream)
  (if *print-pretty*
      (progn
	(princ "#IS(" stream)
	(dolist (slot '(mod reg r/m scale index base displacement immediate opcode))
	  (when (slot-boundp obj slot)
	    (format stream "(~A: ~A)" slot (slot-value obj slot))))
	(princ ")" stream)
	obj)
    (call-next-method obj stream)))

;;; ----------------------------------------------------------------
;;;   stupid "decoder sets"..
;;; ----------------------------------------------------------------

(defmacro make-decoder-set (&rest decoder-spec)
  (let ((keys (mapcar #'first decoder-spec))
	(vals (mapcar #'second decoder-spec)))
    (list 'quote
	  (mapcar #'cons keys vals))))

(defmacro make-encoder-set (&rest decoder-spec)
  (let ((keys (mapcar #'first decoder-spec))
	(vals (mapcar #'second decoder-spec)))
    (list 'quote
	  (mapcar #'cons  vals keys))))

(defun decode-set (decoder-set index &key (errorp t))
  ;;  (declare (type list decoder))
  (let ((r (cdr (assoc index decoder-set
		       :test #'equal))))
    (when (and (not r) errorp)
      (error "failed to look up ~A:~A in the set ~A"
	     (type-of index)
	     index
	     (mapcar #'(lambda (cons)
			 (format nil "(~A:~A . ~A)"
				 (type-of (car cons))
				 (car cons)
				 (cdr cons)))
		     decoder-set)))
    r))
