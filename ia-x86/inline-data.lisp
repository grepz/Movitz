;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 2000, 2004,
;;;;    Department of Computer Science, University of Tromso, Norway
;;;; 
;;;; Filename:      inline-data.lisp
;;;; Description:   Objects that represents inline data in assembly listings.
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Mon Aug 21 10:35:46 2000
;;;; Distribution:  See the accompanying file COPYING.
;;;;                
;;;; $Id: inline-data.lisp,v 1.3 2004/02/10 00:03:30 ffjeld Exp $
;;;;                
;;;;------------------------------------------------------------------

(in-package #:ia-x86)

(defclass inline-data () ())

(defun bitsize-to-sizeof (bitsize)
  (unless (and (>= bitsize 0)
	       (zerop (mod bitsize 8)))
    (error "Illegal bitsize ~A, must be positive multiple of 8." bitsize))
  (truncate bitsize 8))

(defclass inline-data-format (inline-data)
  ((format :reader inline-data-format-format
	   :initarg  format)
   (sizeof :reader inline-data-format-sizeof
	   :initarg sizeof)
   (args   :reader inline-data-format-args
	   :initarg args)))

(defun make-inline-data-format (bitsize format args)
  "Make an inlined, formatted, null-terminated string."
  (check-type format string)
  (make-instance 'inline-data-format
    'format format
    'sizeof (and bitsize (bitsize-to-sizeof bitsize))
    'args args))

(defmethod inline-data-encode ((data inline-data-format) env)
  (with-slots (format sizeof args) data
    (let ((string (apply #'format nil format (inline-data-resolve-arglist args env))))
      (when (and sizeof (plusp sizeof)
		 (>= (length string) (expt 2 (* 8 sizeof))))
	(error "String doesn't fit in ~D-byte: ~S" (* 8 sizeof) format))
      (nconc
       ;; if we have a positive sizeof, insert byte with string-length.
       (if (and sizeof (plusp sizeof))
	   (list (complex (length string) sizeof))
	 nil)
       ;; insert the string
       (map 'list #'(lambda (c) (complex (char-code c) 1)) string)
       ;; if sizeof is zero, null-terminate the string.
       (and sizeof (zerop sizeof) (list #c(0 1)))))))

(defmethod inline-data-guess-sizeof ((data inline-data-format) env)
  (with-slots (format sizeof args) data
    (+ (or (and sizeof (zerop sizeof) 1) 0)
       (length (apply #'format nil format (inline-data-resolve-arglist-with-zeros args env))))))

(defmethod print-object ((obj inline-data-format) stream)
  (cond
   (*print-pretty*
    (format stream
	    "#<asm string ~S>"
	    (inline-data-format-format obj))
    obj)
   (t (call-next-method obj stream))))

;;;; Inline data generated assembly-time by functions.

(defclass inline-data-fun (inline-data)
  ((fun    :reader inline-data-fun-fun
	   :initarg fun)
   (args   :reader inline-data-fun-args
	   :initarg args)))

(defun make-inline-fun (expr)
  (destructuring-bind ((fun &rest args)) expr
    (make-instance 'inline-data-fun
      'fun fun
      'args args)))

(defmethod inline-data-guess-sizeof ((data inline-data-fun) env)
  (loop for cbyte in (apply (inline-data-fun-fun data)
			    (inline-data-resolve-arglist-with-zeros (inline-data-fun-args data) env))
      summing (imagpart cbyte)))

(defmethod inline-data-encode ((data inline-data-fun) env)
  (mapcar #'(lambda (cbyte)
	      (complex (change-endian (realpart cbyte) (imagpart cbyte))
		       (imagpart cbyte)))
	  (apply (inline-data-fun-fun data)
		 (inline-data-resolve-arglist (inline-data-fun-args data) env))))

;;;; Inline bytes (of various sizes)

(defclass inline-bytes (inline-data)
  ((values :reader inline-bytes-values
	   :initarg values)
   (sizeof :reader inline-bytes-sizeof
	   :initarg sizeof)))

(defmethod inline-data-guess-sizeof ((data inline-bytes) env)
  (declare (ignore env))
  (* (length (inline-bytes-values data))
     (inline-bytes-sizeof data)))

(defmethod inline-data-encode ((data inline-bytes) env)
  (mapcar #'(lambda (value)
	      (complex (change-endian value (inline-bytes-sizeof data))
		       (inline-bytes-sizeof data)))
	  (inline-data-resolve-arglist (inline-bytes-values data) env)))

(defun make-inline-bytes (sizeof args)
  (unless (and (plusp sizeof)
	       (zerop (mod sizeof 8)))
    (error "Illegal byte-size ~A, must be positive multiple of 8." sizeof))
  (make-instance 'inline-bytes
    'values args
    'sizeof (truncate sizeof 8)))

;;; Inline aligment

(defclass inline-alignment (inline-data)
  ((align :reader inline-alignment-align
	  :initarg align)
   (fill-octet :reader inline-alignment-fill-octet
	       :initarg fill-octet)))

(defun make-inline-alignment (align &optional (fill-octet 0))
  (check-type fill-octet (unsigned-byte 8))
  (make-instance 'inline-alignment
    'align align
    'fill-octet fill-octet))

(defmethod inline-data-encode ((data inline-alignment) env)
  (loop for i from 0
      while (not (zerop (mod (+ i (assemble-env-current-pc env))
			     (inline-alignment-align data))))
      collect (complex (inline-alignment-fill-octet data) 1)))


;;;; label resolver functions

(defun inline-data-label-reference-p (expr)
  "Determine if EXPR is a label refenence (i.e. a quoted symbol)."
  (and (listp expr)
       (= 2 (length expr))
       (eq 'quote (first expr))
       (symbolp (second expr))))

(defun inline-data-resolve-arglist (arglist env)
  (let* (unresolveds
	 (arglist (mapcar #'(lambda (arg)
			      (if (inline-data-label-reference-p arg)
				  (or (symtab-try-lookup-label (assemble-env-symtab env) (second arg))
				      (pushnew (second arg) unresolveds))
				arg))
			  arglist)))
    (if (not (null unresolveds))
	(error 'unresolved-labels 'labels unresolveds)
      arglist)))

(defun inline-data-resolve-arglist-with-zeros (arglist env)
  (mapcar #'(lambda (arg)
	      (if (inline-data-label-reference-p arg)
		  (or (symtab-try-lookup-label (assemble-env-symtab env) (second arg))
		      0)
		arg))
	  arglist))


(defun read-inline-data (expr)
  (destructuring-bind (marker type &rest args)
      expr
    (assert (string= '% marker))
    (cond
     ((string= 'format type) (make-inline-data-format (first args) (second args) (nthcdr 2 args)))
     ((string= 'fun type) (make-inline-fun args))
     ((string= 'bytes type) (make-inline-bytes (first args) (rest args)))
     ((string= 'align type) (apply #'make-inline-alignment args))
     (t (error "Unknown inline data item: ~A" expr)))))
