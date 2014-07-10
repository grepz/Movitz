;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 20012000, 2003-2004, Frode Vatved Fjeld
;;;; 
;;;; Filename:      def-instr.lisp
;;;; Description:   
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Thu May  4 16:41:20 2000
;;;; Distribution:  See the accompanying file COPYING.
;;;;                
;;;; $Id: def-instr.lisp,v 1.3 2004/02/10 00:03:19 ffjeld Exp $
;;;;                
;;;;------------------------------------------------------------------

(in-package #:ia-x86)

(defvar *instr-definitions* (make-hash-table))

(defmacro def-instr-template (&rest args)
  `(pushnew ',args
	    (gethash ',(car args) *instr-definitions*)
	    :test #'equalp))

(defun init-instruction-tables ()
  (template-forget-all)
  (maphash (lambda (name instr-specs)
	     (let ((templates (loop for instr-spec in instr-specs
				  collect (make-instr-template instr-spec))))
	       (templates-remember name templates)))
	   *instr-definitions*)
  (table-stats "ia-x86:     " *template-table*)
  (table-stats "ia-x86 2op: " *template-table-0f*))

(defun make-instr-template (instr-spec)
  (destructuring-bind (base-class
		       ((match-numo match-val match-mask ; template
				    &optional not-list ; ((not-value not-mask) ...)
				    (req-prefixes '())
				    (not-prefixes '()))
			(&optional (modr/m-p nil)
				   (sib-p nil)
				   (disp-numo 0) ; format
				   (imm-numo 0)
				   (opcode-numo 1)))
		       operand-types
		       operand-encoding-set
		       &key priority
			    (cpu-mode :any-mode)
			    (operand-mode :any-mode)
			    (addressing-mode :any-mode))
      instr-spec
    (check-type not-list list)
    (check-type cpu-mode (member :16-bit :32-bit :any-mode))
    (check-type operand-mode (member :16-bit :32-bit :any-mode))
    (check-type addressing-mode (member :16-bit :32-bit :any-mode))
    (assert (null (intersection req-prefixes not-prefixes))
	(req-prefixes not-prefixes)
      "def-instr-template ~A has overlapping match-prefix-sets: [~A] ~~[~A]"
      base-class
      req-prefixes
      not-prefixes)
    (let ((operand-classes
	   (when operand-encoding-set
	     (mapcar #'(lambda (ot)
			 (let ((encodings (gethash ot *operand-encoding-by-type*)))
			   (unless encodings
			     (warn "Operand-type ~A has no encodings." ot)
			     (return-from make-instr-template nil))
			   (let ((c (intersection encodings
						  (union operand-encoding-set
							 '(immediate-constant
							   register-constant)))))
			     (case (length c)
			       ((0) (warn "[~A] No match for ~A in ~A"
					  base-class encodings operand-encoding-set)
				    (return-from make-instr-template nil)) ; inconsistent template
			       ((1) (find-operand-class (first c) ot))
			       (t   (error
				     "Operand set intersection for ot ~A too big (was ~A)"
				     ot c))))))
		     operand-types))))
      (make-instance 'instr-template
	'value match-val
	'mask match-mask
	'priority (or priority +template-default-priority+)
	'numo match-numo
	'not-list not-list
	'cpu-mode cpu-mode
	'operand-mode operand-mode
	'addressing-mode addressing-mode
	'req-prefixes req-prefixes
	'not-prefixes not-prefixes
	'instr-numo (+ (if modr/m-p 1 0)
		       (if sib-p 1 0)
		       disp-numo
		       imm-numo
		       opcode-numo)
	'instr-classname base-class
	'instr-operand-types operand-types
	'instr-operand-classes operand-classes
	'instr-operand-base-classes (mapcar #'operand-class-base-class
					    operand-classes)
	;; instruction format
	'modr/m-p modr/m-p
	'sib-p sib-p
	'displacement-numo disp-numo
	'immediate-numo imm-numo
	'opcode-numo opcode-numo))))


#+ignore
(defmacro def-instr-template (base-class
			      ((match-numo match-val match-mask ; template
				&optional not-list ; ((not-value not-mask) ...)
					  (req-prefixes '())
					  (not-prefixes '()))
			       (&optional (modr/m-p nil)
					  (sib-p nil)
					  (disp-numo 0) ; format
					  (imm-numo 0)
					  (opcode-numo 1)))
			      operand-types
			      operand-encoding-set
			      &key priority
				   (cpu-mode :any-mode)
				   (operand-mode :any-mode)
				   (addressing-mode :any-mode))
  (check-type not-list list)
  (check-type cpu-mode (member :16-bit :32-bit :any-mode))
  (check-type operand-mode (member :16-bit :32-bit :any-mode))
  (check-type addressing-mode (member :16-bit :32-bit :any-mode))
  (assert (null (intersection req-prefixes not-prefixes))
      (req-prefixes not-prefixes)
    "def-instr-template ~A has overlapping match-prefix-sets: [~A] ~~[~A]"
    base-class
    req-prefixes
    not-prefixes)
  (let ((operand-classes
	 (when operand-encoding-set
	   (mapcar #'(lambda (ot)
		       (let ((encodings (gethash ot *operand-encoding-by-type*)))
			 (unless encodings
			   (warn "Operand-type ~A has no encodings." ot)
			   (return-from def-instr-template nil))
			 (let ((c (intersection encodings
						(union operand-encoding-set
						       '(immediate-constant
							 register-constant)))))
			   (case (length c)
			     ((0) (warn "[~A] No match for ~A in ~A"
					base-class encodings operand-encoding-set)
				  (return-from def-instr-template nil)) ; inconsistent template
			     ((1) (find-operand-class (first c) ot))
			     (t   (error
				   "Operand set intersection for ot ~A too big (was ~A)"
				   ot c))))))
		   operand-types))))

    `(make-instance 'instr-template
       'value ,match-val
       'mask ,match-mask
       'priority ,(or priority +template-default-priority+)
       'numo ,match-numo
       'not-list ',not-list
       'cpu-mode ,cpu-mode
       'operand-mode ,operand-mode
       'addressing-mode ,addressing-mode
       'req-prefixes ',req-prefixes
       'not-prefixes ',not-prefixes
       'instr-numo ,(+ (if modr/m-p 1 0)
		       (if sib-p 1 0)
		       disp-numo
		       imm-numo
		       opcode-numo)
       'instr-classname ',base-class
       'instr-operand-types ',operand-types
       'instr-operand-classes ',operand-classes
       'instr-operand-base-classes (mapcar #'operand-class-base-class
					   ',operand-classes)
       ;; instruction format
       'modr/m-p ,modr/m-p
       'sib-p ,sib-p
       'displacement-numo ,disp-numo
       'immediate-numo ,imm-numo
       'opcode-numo ,opcode-numo)))


;;; ----------------------------------------------------------------
;;;     Instruction format:      /r and /digit
;;; ----------------------------------------------------------------


(defparameter +operand-table-indirect+
    '(((register-reg indirect-register-mod00) ; 00  modR/M
       nil (nil 0) 1 #x00 #xc0 #x04 #x06)
      ((register-reg indirect-register-00-sib) ; 00  modR/M + SIB, index/=4 base/=5
       nil (t   0) 2 #x0400 #xc700 #x0005 #x0007 #x0020 #x0038)
      ((register-reg indirect-register-00-sib-base5) ; 00  modR/M + SIB + disp32,
       nil (t   4) 2 #x0405 #xc707 #x0020 #x0038) ;  index/=4 base=5
      ((register-reg indirect-register-00-sib-index4)
       nil (t   0) 2 #x0420 #xc738 #x0005 #x0007) ; index=4, base/=5
      ((register-reg indirect-pointer-00-sib-index4-base5)
       nil (t   4) 2 #x0425 #xc73f)	; 00  modR/M + SIB + disp32, index=4, base=5
      ((register-reg indirect-pointer-00)
       nil (nil 4) 1 #x05 #xc7)		; 00  modR/M + disp32
      ((register-reg indirect-register-01)
       nil (nil 1) 1 #x40 #xc0 #x04 #x07) ; 01 modR/M + disp8
      ((register-reg indirect-register-01-sib)
       nil (t   1) 2 #x4400 #xc700 #x0020 #x0038) ; 01 modR/M + SIB + disp8
      ((register-reg indirect-register-01-sib-index4)
       nil (t   1) 2 #x4420 #xc738)	; 01 modR/M + SIB + disp8
      ((register-reg indirect-register-10)
       nil (nil 4) 1 #x80 #xc0 #x04 #x07) ; 10 modR/M + disp32
      ((register-reg indirect-register-10-sib)
       nil (t   4) 2 #x8400 #xc700 #x0020 #x0038) ; 10 modR/M + SIB + disp32
      ((register-reg indirect-register-10-sib-index4)
       nil (t   4) 2 #x8420 #xc738)
      ;; 16-bit [IISR table 1: "16-bit addressing forms"]
      ((register-reg 16bit-indirect-register-mod00)
       t   (nil 0) 1 #x00 #xc0 #x06 #x07) ; 00
      ((register-reg 16bit-indirect-pointer)
       t   (nil 2) 1 #x06 #xc7)		; 00
      ((register-reg 16bit-indirect-register-mod01)
       t   (nil 1) 1 #x40 #xc0)		; 01
      ((register-reg 16bit-indirect-register-mod10)
       t   (nil 2) 1 #x80 #xc0)))		; 10

(defparameter +operand-table-direct+
    '(((register-reg register-r/m)
       dont-care (nil 0) 1 #xc0 #xc0)))	; 11 -- same for 16 and 32-bit
;;;      ((register-reg register-r/m) ; same as 32-bit.
;;;       t   (nil 0) 1 #xc0 #xc0)))	; 11)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun canonical-opcode (byte)
    (cond
     ((complexp byte)
      byte)
     ((and (integerp byte)
	   (<= #x00 byte #xff))
      (complex byte 1))
     ((and (integerp byte)
	   (<= #x0f00 byte #x0fff))
      (complex byte 2))
     (t (error "Illegal opcode byte ~A" byte)))))

(defmacro def-instr/r-and-/digit (class opcode-byte-spec
				  (&optional (imm-numo 0) (digit nil))
				  operand-types
				  &key req-prefixes not-prefixes
				       (operand-mode :any-mode)
				       (cpu-mode :any-mode)
				       priority
				       (indirect nil) ; only indirect operands?
				       (direct nil)) ; only direct operands?
  (assert (not (and indirect direct))
      (indirect direct)
    "An instruction can't be both direct and indirect-only.")
  (unless (typep digit '(or (unsigned-byte 3) null))
    (error "def-instr/r-and-/digit: digit <~A> must be of 3-bit value." digit))
  (let ((opcode-byte (canonical-opcode opcode-byte-spec))
	forms)
    (dolist				; 32-bit [IISR table 2: "32-bits addressing forms"]
	(efmt (append (and (not direct) +operand-table-indirect+)
		      (and (not indirect) +operand-table-direct+)))
      (destructuring-bind (operand-encodings 16-bit-addr-p fmt match-numo match-value match-mask
			   &rest not-list-flat)
	  efmt
	(push `(def-instr-template ,class
		   ((,(+ (imagpart opcode-byte) ; prepend opcode-byte to match-value and mask..
			 match-numo)
		     ,(dpb (realpart opcode-byte) ; match-value
			   (byte (* 8 (imagpart opcode-byte))
				 (* 8 match-numo))
			   (dpb (or digit 0)
				(byte 3 (- (* 8 match-numo) 5))
				match-value))
		     ,(dpb -1		; match-mask
			   (byte (* 8 (imagpart opcode-byte))
				 (* 8 match-numo))
			   (dpb (if digit #b111 #b000)
				(byte 3 (- (* 8 match-numo) 5))
				match-mask))
		     ,(loop for i on not-list-flat by #'cddr ; unflatten not-list by twos.
			  collect (subseq i 0 2))
		     ,req-prefixes
		     ,not-prefixes)
		    (t ,@fmt ,imm-numo ,(imagpart opcode-byte)))
		 ,operand-types
		 ,(if (zerop imm-numo)
		      operand-encodings
		    (cons 'immediate
			  operand-encodings))
		 :priority ,priority
		 :cpu-mode ,cpu-mode
		 :operand-mode ,operand-mode
		 :addressing-mode ,(ecase 16-bit-addr-p
				     ((t) :16-bit)
				     ((nil) :32-bit)
				     ((dont-care) :any-mode)))
	      forms)))
    (values (cons 'list (nreverse forms)))))

(defmacro def-instr/r (class opcode-byte operand-types &rest key-args)
  `(def-instr/r-and-/digit ,class ,opcode-byte ()
			   ,operand-types
			   ,@key-args))

(defmacro def-instr/r-imm (class opcode-byte imm-numo operand-types
			   &rest key-args)
  `(def-instr/r-and-/digit ,class ,opcode-byte (,imm-numo)
			   ,operand-types
			   ,@key-args))

(defmacro def-instr/digit (class (opcode-byte digit) imm-numo operand-types
			   &rest key-args)
  `(def-instr/r-and-/digit ,class ,opcode-byte (,imm-numo ,digit)
			   ,operand-types
			   ,@key-args))

;;; ----------------------------------------------------------------
;;;                  Instruction format: moffs
;;; ----------------------------------------------------------------

(defmacro def-instr/moffs (base-class opcode operand-types)
  (multiple-value-bind (addressing-mode disp-numo)
      (cond
       ((member 'moffs8 operand-types)
	(values :any-mode 1))
       ((member 'moffs16 operand-types)
	(values :16-bit 2))
       ((member 'moffs32 operand-types)
	(values :32-bit 4))
       (t (error "No moffsXX operand-type in specification of moffs instruction ~A."
		 base-class)))
    `(list (def-instr-template ,base-class
	       ((1 ,opcode #xff () () ())
		(nil nil ,disp-numo 0))
	     ,operand-types
	     (abs-pointer-moffs)
	     :addressing-mode ,addressing-mode))))
       
;;; ----------------------------------------------------------------
;;;                  Instruction format: /+
;;; ----------------------------------------------------------------

(defmacro def-instr/+ (class opcode-spec imm-numo operand-types
		       &key req-prefixes not-prefixes
			    (operand-mode :any-mode)
			    (addressing-mode :any-mode))
  (let ((opcode-byte (canonical-opcode opcode-spec)))
    (assert (zerop (ldb (byte 3 0) (realpart opcode-byte)))
	(opcode-byte)
      "The 3 lower bits of the opcode of a +rX-type instruction must be zero")
    `(list (def-instr-template ,class
	       ((,(imagpart opcode-byte)
		 ,(realpart opcode-byte)
		 -8 () ,req-prefixes ,not-prefixes) (nil nil 0 ,imm-numo ,(imagpart opcode-byte)))
	     ,operand-types
	     (register-plus immediate)
	     :operand-mode ,operand-mode
	     :addressing-mode ,addressing-mode))))

;;; ----------------------------------------------------------------
;;;                  Instruction format: "plain"
;;; ----------------------------------------------------------------

(defmacro def-instr/plain (class opcode-spec (disp-numo imm-numo) operand-types
			   &key req-prefixes not-prefixes priority
				(cpu-mode :any-mode)
				(operand-mode :any-mode)
				(addressing-mode :any-mode))
  (let ((opcode-byte (canonical-opcode opcode-spec)))
    `(list (def-instr-template ,class
	 ((,(imagpart opcode-byte) ,(realpart opcode-byte) -1 () ,req-prefixes ,not-prefixes)
	  (nil nil ,disp-numo ,imm-numo ,(imagpart opcode-byte)))
       ,operand-types
       (immediate-constant register-constant
	,@(unless (zerop imm-numo)
	    '(immediate imm8 simm8 imm16-8 imm8-0))
	  ,@(unless (zerop disp-numo)
	      '(plain-displacement pc-relative ptr16-16 ptr16-32)))
       :priority ,priority
       :cpu-mode ,cpu-mode
       :operand-mode ,operand-mode
       :addressing-mode ,addressing-mode))))

(defmacro def-instr/simple (class-name opcode
			    &key req-prefixes not-prefixes priority
				 (operands ())
				 (operand-mode :any-mode)
				 (addressing-mode :any-mode))
  `(def-instr/plain ,class-name ,opcode (0 0) ,operands
		    :req-prefixes ,req-prefixes
		    :not-prefixes ,not-prefixes
		    :priority ,priority
		    :operand-mode ,operand-mode
		    :addressing-mode ,addressing-mode))
			    

(defmacro def-instr/simple-fp (class-name opcode
			       &key req-prefixes not-prefixes)
  `(def-instr/simple ,class-name ,opcode
     :req-prefixes ,req-prefixes
     :not-prefixes ,not-prefixes))


(defmacro def-instr/jcc (name opcode &rest rest-args)
  `(def-instr/plain ,name ,opcode (1 0) (rel8) ,@rest-args))

(defmacro def-instr/jcc2 (name opcode &rest rest-args)
  `(progn
     (def-instr/plain ,name ,(cl:complex (cl:logior #x0f00 opcode) 2)
       (4 0) (rel32) :operand-mode :32-bit ,@rest-args)
     (def-instr/plain ,name ,(cl:complex (cl:logior #x0f00 opcode) 2)
       (2 0) (rel16) :operand-mode :16-bit ,@rest-args)))
;;;     (:plain ,opcode (1 0) (rel8) ,@rest-args)))
;;;     (def-instr/plain ,name ,(cl:complex (cl:logior #x0f00 opcode) 2)
;;;       (2 0) (rel16) :operand-mode :16-bit ,@rest-args)


(defmacro def-instr/set (name opcode &key priority)
  `(def-instr/r ,name ,opcode (r/m8) :indirect nil :priority ,priority))

(defmacro def-instr (name supers &rest specs)
  `(progn
     (defclass ,name ,supers ())
     ,@(loop for spec in specs
	   collecting
	     (let ((form (ecase (first spec)
			   ((:r) 'def-instr/r)
			   ((:r-imm) 'def-instr/r-imm)
			   ((:digit) 'def-instr/digit)
			   ((:plain) 'def-instr/plain)
			   ((:simple) 'def-instr/simple)
			   ((:+) 'def-instr/+)
			   ((:moffs) 'def-instr/moffs)
			   ((:jcc) 'def-instr/jcc)
			   ((:jcc2) 'def-instr/jcc2)
			   ((:set) 'def-instr/set)
			   ((:simple-fp) 'def-instr/simple-fp)
			   )))
	       (list* form name (rest spec))))
     ',name))

  
