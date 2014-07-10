;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 2000-2004,
;;;;    Department of Computer Science, University of Tromso, Norway
;;;; 
;;;; Filename:      read.lisp
;;;; Description:   Functions for reading assembly expressions.
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Mon Jul 31 13:54:27 2000
;;;; Distribution:  See the accompanying file COPYING.
;;;;                
;;;; $Id: read.lisp,v 1.8 2004/11/10 15:36:15 ffjeld Exp $
;;;;                
;;;;------------------------------------------------------------------

(in-package #:ia-x86)

;;; Implements the following assembly syntax:
;;;
;;;       program ::= (<sexpr>*)
;;;
;;;         sexpr ::= ( <expr> ) | <label> | (% <inline-data> %) | (:include . <program>)
;;;          expr ::= <instro> | ( { <prefix> } ) <instro>
;;;        instro ::= <instruction> { <operand> }
;;;
;;;       operand ::= <concrete> | <abstract>
;;;
;;;      concrete ::= <immediate> | <register> | <indirect>
;;;     immediate ::= <number>
;;;      register ::= eax | ecx | ...
;;;      indirect ::= ( iexpr )
;;;         iexpr ::=   <address>
;;;                   | (quote <label>)
;;;                   | <segment> <address> 
;;;                   | pc+ <offset>
;;;                   | pc  <address>
;;;                   | { (quote <label>) } <offset> <scaled-register> <register>
;;;     scaled-register ::= <register> | ( <register> <scale> )
;;;               scale ::= 1 | 2 | 4 | 8
;;;             address ::= <number>
;;;              offset ::= <signed-number>
;;;
;;;      abstract ::= (quote <absexpr>)
;;;       absexpr ::= <label> | <number> | append-prg
;;;    append-prg ::= program
;;;
;;;        prefix ::= <segment-override> | (:size <size>)
;;;
;;;
;;; Instructions are recognized by symbol-name, so there should be no
;;; need to worry about packages etc.

(defparameter +all-registers+
    '(:eax :ecx :edx :ebx :esp :ebp :esi :edi
      :ax :cx :dx :bx :sp :bp :si :di
      :al :cl :dl :bl :ah :ch :dh :bh
      :cs :ds :es :fs :gs :ss
      :dr0 :dr1 :dr2 :dr3 :dr4 :dr5 :dr6 :dr7 
      :cr0 :cr2 :cr3 :cr4
      :mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7
      :xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7
      ))

(defun read-register-symbol (spec)
  (assert (member spec +all-registers+)
      (spec)
    "Expected a register: ~A" spec)
  (find-symbol (symbol-name spec) '#:ia-x86))

(defun is-register-p (spec)
  (and (symbolp spec)
       (member spec +all-registers+)))

(defun is-address-p (spec)
  (typecase spec
    ((integer 0 *) t)
    (t nil)))

(defun is-offset-p (spec)
  (typecase spec
    (integer t)
    (symbol t)
    (t nil)))

(defun is-pc-relative-p (iexpr)
  (and (<= 2 (length iexpr))
       (symbolp (first iexpr))
       (string= :pc+ (first iexpr))
       (is-offset-p (second iexpr))))

(defun extract-label-ref (expr)
  "Return NIL if expr is not a label reference, otherwise
returns the symbol/label that is referenced."
  (and (listp expr)
       (= 2 (length expr))
       (symbolp (first expr))
       (string= 'quote (first expr))
       (symbolp (second expr))
       (second expr)))

(defun read-indirect-operand (iexpr)
  (cond
   ((and (= 1 (length iexpr))		; simple direct memory reference
	 (is-address-p (car iexpr)))
    (make-instance 'operand-direct
      'address (car iexpr)))
   ((and (= 1 (length iexpr))		; abstract simple direct memory reference
	 (extract-label-ref (first iexpr)))
    (make-instance 'operand-direct
      'address (extract-label-ref (first iexpr))))
   ((and (= 2 (length iexpr))		; segmented indirect address
	 (integerp (first iexpr))
	 (integerp (second iexpr)))
    (make-instance 'operand-direct
      'address (second iexpr)
      'segment (first iexpr)))
   ((is-pc-relative-p iexpr)		; PC-relative address
    (make-instance 'operand-rel-pointer
      'offset (second iexpr)))
   (t (let (register register2 (offset 0) symbolic-offsets scale) ; indirect register(s)
	(loop for s in iexpr
	    do (cond
		((integerp s)		; an offset
		 (incf offset s))
		((and (listp s)		; an abstract offset
		      (= 2 (length s))
		      (symbolp (first s))
		      (string= 'quote (first s))
		      (symbolp (second s)))
		 (push (second s) symbolic-offsets))
		((and (is-register-p s)	; register number one
		      (null register))
		 (setf register (read-register-symbol s)))
		((and (is-register-p s)	; a register number two
		      (null register2))
		 (setf register2 (read-register-symbol s)))
		((and (listp s)		; a scaled register
		      (is-register-p (first s))
		      (integerp (second s))
		      (null register))
		 (setf register (read-register-symbol (first s))
		       scale (second s)))
		((and (listp s)		; a scaled register after a non-scaled register
		      (is-register-p (first s))
		      (integerp (second s))
		      (null register2)
		      (symbolp register))
		 (setf register2 register
		       register (read-register-symbol (first s))
		       scale (second s)))
		(t (error "Can't read indirect operand ~A [~A]" iexpr s))))
	(make-instance 'operand-indirect-register
	  'register register
	  'register2 register2
	  'offset (if (null symbolic-offsets)
		      offset
		    `(+ ,offset ,@symbolic-offsets))
	  'scale (or scale 1))))))

(defun read-abstract-operand (spec)
  (etypecase (first spec)
    (symbol
     (assert (not (null (first spec))) (spec)
       "NIL is an illegal assembly label: ~S" spec)
     (make-instance 'operand-label
       'label (first spec)
       'user-size (second spec)))
    (list
     (let ((type (caar spec))		; (<type> &rest <datum>)
	   (datum (cdar spec)))
       (ecase type
	 (:sub-program
	  (let* ((options (car datum))
		 (sub-program (cdr datum)))
	    (destructuring-bind (&optional (label (gensym "sub-program-label-")))
		options
	      (declare (special *programs-to-append*))
	      (pushnew (cons label sub-program) *programs-to-append*
		       :test #'equal)
	      (make-instance 'operand-label
		'label label))))
	 (:funcall
	  (make-instance 'calculated-operand
	    :calculation (coerce (car datum) 'function)
	    :sub-operands (mapcar #'read-operand (cdr datum)))))))
    (integer
     (make-instance 'operand-number
       'number (first spec)))))


(defun read-operand (spec)
  (cond
   ((integerp spec)
    (make-instance 'operand-immediate
      'value spec))
   ((and (symbolp spec)
	 (is-register-p spec))
    (make-instance 'operand-register
      'register (read-register-symbol spec)))
   ((null spec)
    (error "Operand was NIL."))
   ((and (listp spec)
	 (symbolp (first spec))
	 (string= 'quote (first spec)))
    (read-abstract-operand (rest spec)))
   ((listp spec)
    (read-indirect-operand spec))
   (t (error "Can't read operand ~S" spec))))

(defun read-prefixes (prefix-spec)
  (loop with user-size = nil with user-finalizer = nil
      for p in prefix-spec
      if (symbolp p)
      collect (let ((ps (find-symbol (symbol-name p) '#:ia-x86)))
		(if (decode-set +prefix-opcode-map+ ps :errorp nil)
		    ps
		  (error "No such prefix: ~A" p)))
      into prefixes
      else do
	   (check-type p list)
	   (ecase (car p)
	     (:size
	      (let ((size (second p)))
		(check-type size integer)
		(setf user-size size)))
	     (:finalize			; XXX
	      (let ((finalizer (second p)))
		(check-type finalizer symbol "a function name")
		(setf user-finalizer finalizer))))
      finally (return (values prefixes user-size user-finalizer))))
	      
(defvar *find-instruction-cache* (make-hash-table :test #'eq))

(defun read-instruction (sexpr)
  "Parse a list into an assembly instruction."
  (let (prefix-list user-size user-finalizer instr-name operand-list)
    (if (listp (first sexpr))
	(setf (values prefix-list user-size user-finalizer) (read-prefixes (first sexpr))
	      instr-name (second sexpr)
	      operand-list (nthcdr 2 sexpr))
      (setf prefix-list nil
	    user-size nil
	    instr-name (first sexpr)
	    operand-list (nthcdr 1 sexpr)))
    (case instr-name
      (:align
       (make-instance 'alignment :type operand-list))
      ((nil)
       (mapcar #'read-operand operand-list)
       nil)
      (t (make-instance (or (gethash instr-name *find-instruction-cache*)
			    (setf (gethash instr-name *find-instruction-cache*)
			      (multiple-value-bind (instr-symbol instr-symbol-status)
				  (find-symbol (string instr-name) '#:ia-x86-instr)
				(unless instr-symbol-status
				  (error "No instruction named ~A." (string instr-name)))
				instr-symbol)))
	   :prefixes prefix-list
	   :user-size user-size
	   :user-finalizer user-finalizer
	   :operands (mapcar #'read-operand operand-list))))))
      

(defun inline-data-p (expr)
  (and (listp expr)
       (symbolp (first expr))
       (string= '% (first expr))))

(defun read-proglist (program &rest args)
  (when program
    (let ((*programs-to-append* ())
	  (*already-appended* 0))
      (declare (special *programs-to-append* *already-appended*))
      (apply #'read-proglist-internal program args))))
  
(defun read-proglist-internal (program &key no-warning (do-append-programs t)
					    (append-after-type 'ia-x86-instr::unconditional-branch))
  "Read a symbolic assembly program (a list with the syntax described in BNF in this file) into 
a proglist: A list of INSTRUCTION objects."
  (declare (special *programs-to-append* *already-appended*))
  (loop for expr in program
      if (null expr)
      do (error "Illegal NIL expr in program: ~S" program)
      else if (or (symbolp expr) (integerp expr))
      collect expr			; a label, collect it.
      else if (inline-data-p expr)
      collect (read-inline-data expr)	; inline data, read it.
      else if (and (consp expr) (eq :include (car expr)))
      append (read-proglist (cdr expr)
			    :do-append-programs do-append-programs
			    :no-warning no-warning)
      else
      append (let ((i (read-instruction expr))) ; an instruction, read it, possibly append
	       (if (and do-append-programs
			*programs-to-append*
			(typep i append-after-type))
		   ;; auto-insert *programs-to-append* here:
		   (let ((sub-programs (reduce #'append
					       (nreverse
						(subseq *programs-to-append* 0
							(- (length *programs-to-append*)
							   *already-appended*))))))
		     (setf *already-appended* (length *programs-to-append*))
		     (list* i (read-proglist-internal sub-programs
						      :do-append-programs do-append-programs)))
		 ;; nothing to insert other than i itself..
		 (when i (list i))))
      finally (assert (= (length *programs-to-append*)
			 *already-appended*) ()
		"Dangling sub-programs to append: ~S"
		(nreverse (subseq *programs-to-append*
				  0 (- (length
					*programs-to-append*)
				       *already-appended*))))))


(defmacro asm (&rest spec)
  `(instruction-encode (read-instruction ',spec) nil))
