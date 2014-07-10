;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 20012000, 2002-2005,
;;;;    Department of Computer Science, University of Tromso, Norway
;;;; 
;;;; Filename:      operands.lisp
;;;; Description:   Operand representation.
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Wed Feb 16 14:02:57 2000
;;;; Distribution:  See the accompanying file COPYING.
;;;;                
;;;; $Id: operands.lisp,v 1.6 2005/08/13 20:31:51 ffjeld Exp $
;;;;                
;;;;------------------------------------------------------------------

(in-package #:ia-x86)

;;; ----------------------------------------------------------------
;;;                         Operand types
;;; ----------------------------------------------------------------

;;; Operand types are identified by symbols

(defmacro def-operand-types (&rest ot-specs)
  (list* 'cl:eval-when '(:load-toplevel)
	 (loop for (sym . properties) in ot-specs
	     append
	       `((setf ,@(loop for (p v) in properties
			     appending `((cl:get ',sym ',p) ,v)))
		 (import ',sym :ia-x86-instr)))))

(defun operand-type-property (sym p)
  (get sym p))

(def-operand-types
    (immediate (immediate t))
    (displacement (immediate nil))
    (imm32 (immediate t)
	   (bit-size 32)
	   (signed nil))
    (imm16 (immediate t)
	   (bit-size 16)
	   (signed nil))
  (imm8  (immediate t)
	 (bit-size 8)
	 (signed nil))
  (simm32 (immediate t)
	  (bit-size 32)
	  (signed t))
  (simm16 (immediate t)
	  (bit-size 16)
	  (signed t))
  (simm8  (immediate t)
	  (bit-size 8)
	  (signed t))
  (imm16-8 (immediate t)
	   (bit-size 16))
  (imm8-0 (immediate t)
	  (bit-size 8))
  (r32 (bit-size 32))
  (r16 (bit-size 16))
  (r8  (bit-size 8))
  (+r32 (bit-size 32))
  (+r16 (bit-size 16))
  (+r8  (bit-size 8))
  (m (bit-size 32))			; memory poiner
  (m64 (bit-size 64))
  (mm (bit-size 64))			; mmx register
  (mm/m64 (immediate nil))
  (mm/m32 (immediate nil))
  (xmm (bit-size 128))			; simd register
  (xmm/m128) (xmm/m64) (xmm/m32)
  (moffs8  (immediate nil)
	   (bit-size 8)
	   (signed nil))
  (moffs16 (immediate nil)
	   (bit-size 16)
	   (signed nil))
  (moffs32 (immediate nil)
	   (bit-size 32)
	   (signed nil))
  (al      (immediate nil)
	   (bit-size 8))
  (ah      (immediate nil)
	   (bit-size 8))
  (ax      (immediate nil)
	   (bit-size 16))
  (eax     (immediate nil)
	   (bit-size 32))
  (ebx     (immediate nil)
	   (bit-size 32))
  (dx      (immediate nil)
	   (bit-size 16))
  (edx     (immediate nil)
	   (bit-size 32))
  (cr0) (cr2) (cr3) (cr4)
  (dr0) (dr1) (dr2) (dr3)
  (dr4) (dr5) (dr6) (dr7)
  (ptr16-16 (immediate nil))
  (ptr16-32 (immediate nil))
  (m16-16 (immediate nil))
  (m16-32 (immediate nil))
  (m32real (immediate nil))
  (m64real (immediate nil))
  (m80real (immediate nil))
  (m16int (immediate nil))
  (m32int (immediate nil))
  (m64int (immediate nil)))

;;; ----------------------------------------------------------------
;;;                     Operand Class
;;; ----------------------------------------------------------------

(defclass operand () ())

(defclass concrete-operand (operand) ()
	  (:documentation "Operands that correspond directly
to one of the x86 operand adressing modes."))

(defclass abstract-operand (operand) ()
	  (:documentation "Operands that are not concrete, for example
symbolic references. Abstract operands need to be resolved to concrete
operands at encoding-time."))

(defmethod print-object ((obj concrete-operand) stream)
  (format stream "~A" (operand-listform obj))
  obj)

(defmethod print-object ((obj abstract-operand) stream)
  (format stream "~A" (operand-listform obj))
  obj)

;;; ----------------------------------------------------------------
;;;                        Abstract operands
;;; ----------------------------------------------------------------

(defun abstract-operand-to-offset (operand template instr env)
  (sign-extend (mod (- (operand-resolve-to-number operand env)
		       (assemble-env-current-pc env)
		       (template-instr-and-prefix-length template instr env))
		    #x100000000)
	       4))

(defclass operand-label (abstract-operand)
  ((label
    :type symbol
    :initarg label
    :accessor operand-label)
   (user-size
    :initarg user-size
    :reader operand-user-size
    :initform nil)))

(defmethod operand-user-size ((operand t)) nil)

(defmethod operand-listform ((operand operand-label))
  (list* 'quote
	 (operand-label operand)
	 (operand-user-size operand)))

(defmethod operand-resolve-to-number ((operand operand-label) env)
  (assert (not (null env)) ()
    "Resolving ~A requires an assemble-environment." operand)
  (symtab-lookup-label (assemble-env-symtab env)
		       (operand-label operand)))

(defclass calculated-operand (abstract-operand)
  ((sub-operands
    :initarg :sub-operands
    :accessor sub-operands)
   (calculation
    :initarg :calculation
    :reader operand-calculation)
   (user-size
    :initarg user-size
    :reader operand-user-size
    :initform nil)))

(defmethod operand-resolve-to-number ((operand calculated-operand) env)
  (assert (not (null env)) ()
    "Resolving ~A requires an assemble-environment." operand)
  (apply (operand-calculation operand)
	 (mapcar #'operand-resolve-to-number
		 (sub-operands operand)
		 (let ((x (cons env nil)))
		   (setf (cdr x) x)))))	; make circular one-list.

(defclass operand-number (abstract-operand)
  ((number
    :type integer
    :initarg number
    :reader operand-number)))

(defmethod operand-listform ((operand operand-number))
  (list* 'quote
	 (operand-number operand)))

(defmethod operand-resolve-to-number ((operand operand-number) env)
  (declare (ignore env))
  (operand-number operand))

;;; ----------------------------------------------------------------
;;;     Concrete operands (modelling the "real world" x86 CPU)
;;; ----------------------------------------------------------------

;;; ----------------------------------------------------------------
;;;                          Immediate
;;; ----------------------------------------------------------------

(defclass operand-immediate (concrete-operand)
  ((value
    :initarg value
    :accessor operand-value)))

(defmethod operand-listform ((obj operand-immediate))
  (operand-value obj))

(defmethod print-object ((obj operand-immediate) stream)
  (if (and (not *print-readably*)
	   *print-pretty*)
      (progn
	(format stream "~A" (slot-value obj 'value))
	obj)
    (call-next-method obj stream)))

;;; ----------------------------------------------------------------
;;;                      Register
;;; ----------------------------------------------------------------

(defclass operand-register (concrete-operand)
  ((register
    :initarg register
    :accessor operand-register)))

(defmethod operand-listform ((obj operand-register))
  (operand-register obj))

(defmethod print-object ((obj operand-register) stream)
  (if (and (not *print-readably*)
	   *print-pretty*)
      (progn (format stream "%~A" (slot-value obj 'register))
	     obj)
    (call-next-method obj stream)))

;;; ----------------------------------------------------------------
;;;                        Memory operands
;;; ----------------------------------------------------------------

(defclass operand-memory (concrete-operand)
  (referenced-size))

;;; ----------------------------------------------------------------
;;;                        Absolute Pointer
;;; ----------------------------------------------------------------

(defclass operand-direct (operand-memory)
  ((address :accessor operand-address
	    :initarg address)
   (segment :accessor operand-segment
	    :initform nil
	    :initarg segment)))

(defmethod operand-listform ((obj operand-direct))
  (if (null (operand-segment obj))
      (list (operand-address obj))
    (list (operand-segment obj)
	  (operand-address obj))))

(defmethod print-object ((obj operand-direct) stream)
  (if (not *print-readably*)
      (progn
	(format stream "[~@[~A:~]~A]"
		(operand-segment obj)
		(operand-address obj))
	obj)
    (call-next-method obj stream)))

;;; ----------------------------------------------------------------
;;;                     PC-Relative Pointer
;;; ----------------------------------------------------------------

(defclass operand-rel-pointer (operand-memory)
  ((offset
    :accessor operand-offset
    :initarg offset)))

(defmethod operand-listform ((obj operand-rel-pointer))
  (list :pc+ (operand-offset obj)))

(defmethod print-object ((obj operand-rel-pointer) stream)
  (if (not *print-readably*)
      (progn
	(format stream "%PC+~A" (slot-value obj 'offset))
	obj)
    (call-next-method obj stream)))

;;; ----------------------------------------------------------------
;;;                  Register-Relative pointer
;;; ----------------------------------------------------------------

(defclass operand-indirect-register (operand-memory)
  ((register
    :accessor operand-register
    :initarg register)
   (register2
    :initform nil
    :accessor operand-register2
    :initarg register2)
   (offset
    :accessor operand-offset
    :initarg offset
    :initform 0)
   (scale
    :type (integer 0 8)			; scale for register (not register2)
    :initarg scale
    :accessor operand-scale
    :initform 1)))

(defmethod operand-listform ((obj operand-indirect-register))
  (with-slots (offset register scale register2)
      obj
    (append (unless (and (integerp offset) (zerop offset))
	      (list offset))
	    (if (= 1 scale)
		(list register)
	      (list (list register scale)))
	    (when register2
	      (list register2)))))

(defmethod print-object ((obj operand-indirect-register) stream)
  (if (not *print-readably*)
      (with-slots (offset register2 register scale) obj
	(format stream "[~@[~A+~]~@[%~A+~]%~A~@[*~D~]]"
		(unless (and (integerp offset) (zerop offset))
		  offset)
		register2
		register
		(when (> scale 1)
		  scale))
	obj)
    (call-next-method obj stream)))

(defun resolve-indirect-register (operand env)
  (with-slots (register register2 offset scale) operand
    (etypecase offset
      (integer
       operand)
      (symbol
       (make-instance 'operand-indirect-register
	 'offset (symtab-lookup-label (assemble-env-symtab env) offset)
	 'register register
	 'register2 register2
	 'scale scale))
      (list
       (make-instance 'operand-indirect-register
	 'offset (apply (car offset)
			(mapcar #'(lambda (o)
				    (etypecase o
				      (integer o)
				      (symbol
				       (symtab-lookup-label (assemble-env-symtab env) o))))
				(cdr offset)))
	 'register register
	 'register2 register2
	 'scale scale)))))

(defun resolve-direct (operand env)
  (with-slots (address segment) operand
    (if (not (symbolp address))
	operand
      (make-instance 'operand-direct
	'address (symtab-lookup-label (assemble-env-symtab env) address)
	'segment segment))))

;;; ----------------------------------------------------------------
;;;             Definition of specialized operand classes
;;; ----------------------------------------------------------------

(defvar *operand-classes* (make-hash-table :test #'equal))
(defvar *operand-encoding-by-type* (make-hash-table :test #'eq))

(defmacro def-operand-class ((operand-encoding operand-types
			      &optional (reg-set (first operand-types)))
			     (base-operand-class) slots)
  (let ((name (intern (format nil "~A~S~{-~S~}" ; the name isn't really important
			      (symbol-name '#:operand-)
			      operand-encoding operand-types))))
    `(progn
       (assert (subtypep (find-class ',base-operand-class)
			 (find-class 'operand))
	   ()
	 "Base operand-class ~A is not an OPERAND class." ',base-operand-class)
       (defclass ,name (,base-operand-class) ,slots)
       (defmethod operand-class-register-set ((operand-encoding (eql (find-class ',name))))
	 (values ',reg-set))
       (defmethod operand-class-encoding ((operand-encoding (eql (find-class ',name))))
	 (values ',operand-encoding))
       (defmethod operand-class-base-class ((operand-class (eql (find-class ',name))))
	 (values (find-class ',base-operand-class)))
       ,@(loop for ot in operand-types
	     appending
	       `((setf (gethash (cons ',operand-encoding ',ot) *operand-classes*) 
		   (find-class ',name))
		 (pushnew ',operand-encoding
			  (gethash ',ot *operand-encoding-by-type*)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +operand-types-indirect-modrm+
      '(r/m8 r/m16 r/m32 m m64
	mm/m64 mm/m32
	xmm/m128 xmm/m64 xmm/m32
	m32real m64real m80real
	m16int m32int m64int
	m16-16 m16-32)
    "This set of operand-types are pointers which are encoded the same way,
but differ in only in what they point to."))

(defmacro def-operand-class-imodrm ((operand-encoding
				     &optional (reg-set nil))
				    (base-operand-class) slots)
  `(def-operand-class (,operand-encoding ,+operand-types-indirect-modrm+ ,reg-set)
       (,base-operand-class) ,slots))
     

(defun find-operand-class (operand-encoding operand-type &key (errorp t))
  "Locate the operand class that encodes <operand-type> into <operand-encoding>."
  (let ((oc (gethash (cons operand-encoding operand-type) *operand-classes*)))
    (unless (or oc (not errorp))
      (error "couldn't find operand-class for (~A ~A)." operand-encoding operand-type))
    (values oc)))

(defun find-operand-type-encodings (operand-type)
  (gethash operand-type *operand-encoding-by-type*))

(defmethod operand-decode (operand (encoding (eql nil)) instr-symbolic)
  "Fallback when no operand-encoding was specified."
  (if (operand-class-encoding (class-of operand))
      (operand-decode operand (operand-class-encoding (class-of operand)) instr-symbolic)
    (call-next-method operand encoding instr-symbolic)))


;;; ----------------------------------------------------------------
;;;                     Operand unification
;;; ----------------------------------------------------------------

(defgeneric operand-and-encoding-unifies-p (operand encoding operand-type)
  (:documentation "This predicate determines if an operand instance may
be encoded in a particular encoding and operand-type."))

(defmethod operand-and-encoding-unifies-p (operand encoding operand-type)
  "If no specialized method exists, the operand and encoding don't unify."
  (declare (ignore operand encoding operand-type))
  (values nil))

(defun operand-unifies-with (operand operand-type)
  "Return a list of all encodings this operand unifies with."
  (loop for encoding in (find-operand-type-encodings operand-type)
      when (operand-and-encoding-unifies-p operand encoding operand-type)
      collect encoding))



(defgeneric operand-and-encoding-unify (operand encoding operand-type template instr env)
  (:documentation "If OPERAND cannot be encoded in ENCODING and
OPERAND-TYPE, NIL is returned. Otherwise, a concretized OPERAND
is returned (if OPERAND is concrete, the same operand is typically
returned."))

(defmethod operand-and-encoding-unify (operand encoding operand-type template instr env)
  "If no specialized method exists, the operand and encoding don't unify."
  (declare (ignore operand encoding operand-type template instr env))
  (values nil))

;;; ----------------------------------------------------------------
;;;                  General, plain operand classes
;;; ----------------------------------------------------------------

;;; Displacement

(def-operand-class (plain-displacement (displacement)) (operand-direct) ())

(defmethod operand-and-encoding-unify ((operand operand-direct)
				       (encoding (eql 'plain-displacement))
				       operand-type
				       template instr
				       env)
  (declare (ignore operand-type instr))
  (let ((resolved-operand (resolve-direct operand env)))
    (with-slots (address segment)
	resolved-operand
      (and (null segment)
	   (<= 0 address (expt 2 (* 8 (template-instr-displacement-numo template))))
	   resolved-operand))))

(defmethod operand-decode ((operand operand-direct)
			   (encoding (eql 'plain-displacement))
			   instr-symbolic)
  (setf (operand-address operand)
    (slot-value instr-symbolic 'displacement))
  (values operand))

(defmethod operand-encode ((operand operand-direct)
			   (encoding (eql 'plain-displacement))
			   operand-type
			   instr-symbolic)
  (declare (ignore operand-type))
  (setf (slot-value instr-symbolic 'displacement)
    (operand-address operand))
  (values instr-symbolic '(displacement)))

;;; Immediate

;;;(def-operand-class (plain-immediate (immediate)) (operand-immediate) ())
;;;
;;;(defmethod operand-and-encoding-unify ((operand operand-immediate)
;;;				       (encoding (eql 'plain-immediate))
;;;				       operand-type
;;;				       template instr
;;;				       env)
;;;  (declare (ignore operand-type instr env))
;;;  (with-slots (value)
;;;      operand
;;;    (and (<= 0 value (expt 2 (* 8 (template-instr-immediate-numo template))))
;;;	 operand)))
;;;
;;;(defmethod operand-decode ((operand operand-immediate)
;;;			   (encoding (eql 'plain-immediate))
;;;			   instr-symbolic)
;;;  (setf (operand-value operand)
;;;    (slot-value instr-symbolic 'immediate))
;;;  (values operand))
;;;
;;;(defmethod operand-encode ((operand operand-immediate)
;;;			   (encoding (eql 'plain-immediate))
;;;			   operand-type
;;;			   instr-symbolic)
;;;  (declare (ignore operand-type))
;;;  (setf (slot-value instr-symbolic 'immediate)
;;;    (operand-value operand))
;;;  (values instr-symbolic '(immediate)))


;;; ----------------------------------------------------------------
;;;                  Specialized operand classes
;;; ----------------------------------------------------------------


;;; Direct register operands encoded in the REG of MODR/M.

(def-operand-class (register-reg (r8)) (operand-register) ())
(def-operand-class (register-reg (r16)) (operand-register) ())
(def-operand-class (register-reg (r32)) (operand-register) ())
(def-operand-class (register-reg (sreg)) (operand-register) ())
(def-operand-class (register-reg (mm)) (operand-register) ()) ; MMX
(def-operand-class (register-reg (xmm)) (operand-register) ()) ; SIMD

(defmethod operand-and-encoding-unify ((operand operand-register)
				       (encoding (eql 'register-reg))
				       operand-type
				       template instr
				       env)
  (declare (ignore template instr env))
  (and (decode-set (find-register-encode-set operand-type)
		   (slot-value operand 'register)
		   :errorp nil)
       operand))

(defmethod operand-decode ((operand operand-register)
			   (encoding (eql 'register-reg))
			   instr-symbolic)
  (setf (operand-register operand)
    (decode-set (find-register-decode-set (operand-class-register-set (class-of operand)))
		(slot-value instr-symbolic 'reg)))
  (assert (not (null (operand-register operand)))
      ((operand-register operand))
    "Unable to decode operand value ~A from set ~A"
    (slot-value instr-symbolic 'reg)
    (find-register-decode-set (operand-class-register-set (class-of operand))))
  (values operand))

(defmethod operand-encode ((operand operand-register)
			   (encoding (eql 'register-reg))
			   operand-type
			   instr-symbolic)
  (setf (slot-value instr-symbolic 'reg)
    (decode-set (find-register-encode-set operand-type)
		(operand-register operand)))
  (values instr-symbolic '(reg)))


;;; Direct register operands encoded in the R/M of of MODR/M.

(def-operand-class (register-r/m (r/m8)) (operand-register) ())
(def-operand-class (register-r/m (r/m16)) (operand-register) ())
(def-operand-class (register-r/m (r/m32)) (operand-register) ())
(def-operand-class (register-r/m (mm/m64)) (operand-register) ()) ; MMX
(def-operand-class (register-r/m (xmm/m128 xmm/m64 xmm/m32)) (operand-register) ()) ; SIMD

(defmethod operand-and-encoding-unify ((operand operand-register)
				       (encoding (eql 'register-r/m))
				       operand-type
				       template instr
				       env)
  (declare (ignore template instr env))
  (and (decode-set (find-register-encode-set operand-type)
		   (slot-value operand 'register)
		   :errorp nil)
       operand))

(defmethod operand-decode ((operand operand-register)
			   (encoding (eql 'register-r/m))
			   instr-symbolic)
  (assert (= #b11 (slot-value instr-symbolic 'mod)))
  (setf (operand-register operand)
    (decode-set (find-register-decode-set (operand-class-register-set (class-of operand)))
		(slot-value instr-symbolic 'r/m)))
  (values operand))

(defmethod operand-encode ((operand operand-register)
			   (encoding (eql 'register-r/m))
			   operand-type
			   instr-symbolic)
  (with-slots (mod r/m)
      instr-symbolic
    (setf mod #b11
	  r/m (decode-set (find-register-encode-set operand-type)
			  (slot-value operand 'register))))
  (values instr-symbolic '(mod r/m)))


;;; Indirect register operand encoded in R/M,
;;; with Mod=00 and R/M /= {#b100, #b101}

(def-operand-class-imodrm (indirect-register-mod00) (operand-indirect-register) ())

(defmethod operand-and-encoding-unify ((operand operand-indirect-register)
				       (encoding (eql 'indirect-register-mod00))
				       operand-type
				       template instr
				       env)
  (declare (ignore template instr))
  (let ((resolved-operand (resolve-indirect-register operand env)))
    (with-slots (offset register register2 scale)
	resolved-operand
      (and (member operand-type +operand-types-indirect-modrm+)
	   (zerop offset)
	   (not register2)
	   (= 1 scale)
	   (member register '(eax ecx edx ebx esi edi))
	   resolved-operand))))

(defmethod operand-decode ((operand operand-indirect-register)
			   (encoding (eql 'indirect-register-mod00))
			   instr-symbolic)
  (assert (= #b00 (slot-value instr-symbolic 'mod)))
  (assert (/= #b100 #b101 (slot-value instr-symbolic 'r/m)))
  (with-slots (register register2 offset scale)
      operand
    (setf register (decode-set (find-register-decode-set 'r/m32-00)
			       (slot-value instr-symbolic 'r/m))
	  register2 nil
	  offset 0
	  scale 1))
  (values operand))

(defmethod operand-encode ((operand operand-indirect-register)
			   (encoding (eql 'indirect-register-mod00))
			   operand-type
			   instr-symbolic)
  (declare (ignore operand-type))
  (with-slots (mod r/m)
      instr-symbolic
    (setf mod #b00
	  r/m (decode-set (find-register-encode-set 'r/m32-00)
			  (slot-value operand 'register))))
  (values instr-symbolic '(mod r/m)))
	  

;;; Indirect register with MOD=#b00, R/M=#b100 in ModR/M
;;; and neither index=#b100 nor base=#b101 in SIB.

(def-operand-class-imodrm (indirect-register-00-sib) (operand-indirect-register) ())

(defmethod operand-and-encoding-unify ((operand operand-indirect-register)
				       (encoding (eql 'indirect-register-00-sib))
				       operand-type
				       template instr
				       env)
  (declare (ignore template instr))
  (let ((resolved-operand (resolve-indirect-register operand env)))
    (with-slots (register register2 offset scale)
	resolved-operand
      (and (member operand-type +operand-types-indirect-modrm+)
	   (zerop offset)
	   (member register '(eax ecx edx ebx ebp esi edi))
	   (member register2 '(eax ecx edx ebx esp esi edi))
	   (member scale '(1 2 4 8))
	   resolved-operand))))

(defmethod operand-decode ((operand operand-indirect-register)
			   (encoding (eql 'indirect-register-00-sib))
			   instr-symbolic)
  (assert (= #b00 (slot-value instr-symbolic 'mod)))
  (assert (= #b100 (slot-value instr-symbolic 'r/m)))
  (assert (/= #b100 (slot-value instr-symbolic 'index)))
  (assert (/= #b101 (slot-value instr-symbolic 'base)))
  (with-slots (register register2 offset scale)
      operand
    (setf register2 (decode-set (find-register-decode-set 'sib-base-00)
			       (slot-value instr-symbolic 'base))
	  register (decode-set (find-register-decode-set 'sib-index)
				(slot-value instr-symbolic 'index))
	  scale (expt 2 (slot-value instr-symbolic 'scale))
	  offset 0))
  (values operand))
(defmethod operand-encode ((operand operand-indirect-register)
			   (encoding (eql 'indirect-register-00-sib))
			   operand-type
			   instr-symbolic)
  (declare (ignore operand-type))
  (with-slots (mod r/m base index scale)
      instr-symbolic
    (setf mod #b00
	  r/m #b100
	  base (decode-set (find-register-encode-set 'sib-base-00)
			   (slot-value operand 'register2))
	  index (decode-set (find-register-encode-set 'sib-index)
			    (slot-value operand 'register))
	  scale (cdr (assoc (slot-value operand 'scale)
			    '((0 . 0) (1 . 0) (2 . 1) (4 . 2) (8 . 3))))))
  (values instr-symbolic '(base index scale)))

;;; Indirect register with MOD=#b00, R/M=#b100 in ModR/M
;;; and base=#b101 and index/=#b100 in SIB.

(def-operand-class-imodrm (indirect-register-00-sib-base5) (operand-indirect-register) ())

(defmethod operand-and-encoding-unify ((operand operand-indirect-register)
				       (encoding (eql 'indirect-register-00-sib-base5))
				       operand-type
				       template instr
				       env)
  (declare (ignore template instr))
  (assert (member operand-type +operand-types-indirect-modrm+))
  (let ((resolved-operand (resolve-indirect-register operand env)))
    (with-slots (register register2 scale)
	resolved-operand
      (and (not register2)
	   (member register '(eax ecx edx ebx ebp esi edi))
	   (member scale '(1 2 4 8))
	   resolved-operand))))

(defmethod operand-decode ((operand operand-indirect-register)
			   (encoding (eql 'indirect-register-00-sib-base5))
			   instr-symbolic)
  (assert (= #b00 (slot-value instr-symbolic 'mod)))
  (assert (= #b100 (slot-value instr-symbolic 'r/m)))
  (assert (= #b101 (slot-value instr-symbolic 'base)))
  (assert (/= #b100 (slot-value instr-symbolic 'index)))
  (with-slots (register register2 offset scale)
      operand
    (setf register (decode-set (find-register-decode-set 'sib-index)
			       (slot-value instr-symbolic 'index))
	  register2 nil
	  offset (realpart (slot-value instr-symbolic 'displacement))
	  scale (expt 2 (slot-value instr-symbolic 'scale))))
  (values operand))
(defmethod operand-encode ((operand operand-indirect-register)
			   (encoding (eql 'indirect-register-00-sib-base5))
			   operand-type
			   instr-symbolic)
  (declare (ignore operand-type))
  (with-slots (mod r/m base index scale displacement)
      instr-symbolic
    (setf mod #b00
	  r/m #b100
	  base #b101
	  index (decode-set (find-register-encode-set 'sib-index)
			    (slot-value operand 'register))
	  scale (1- (integer-length (slot-value operand 'scale)))
	  displacement (realpart (slot-value operand 'offset))))
  (values instr-symbolic '(mod r/m base index scale)))


;;; Indirect register with MOD=#b00, R/M=#b100 in ModR/M
;;; and base/=#b101 and index=#b100 in SIB.

(def-operand-class-imodrm (indirect-register-00-sib-index4) (operand-indirect-register) ())

(defmethod operand-and-encoding-unify ((operand operand-indirect-register)
				       (encoding (eql 'indirect-register-00-sib-index4))
				       operand-type
				       template instr
				       env)
  (declare (ignore template instr))
  (assert (member operand-type +operand-types-indirect-modrm+))
  (let ((resolved-operand (resolve-indirect-register operand env)))
    (with-slots (register register2 offset scale)
	resolved-operand
      (and register
	   (zerop offset)
	   (null register2)
	   (= 1 scale)
	   (member register '(eax ecx edx ebx esp esi edi))
	   resolved-operand))))
					    
(defmethod operand-decode ((operand operand-indirect-register)
			   (encoding (eql 'indirect-register-00-sib-index4))
			   instr-symbolic)
  (assert (= #b00 (slot-value instr-symbolic 'mod)))
  (assert (= #b100 (slot-value instr-symbolic 'r/m)))
  (assert (= #b100 (slot-value instr-symbolic 'index)))
  (assert (/= #b101 (slot-value instr-symbolic 'base)))
  (with-slots (register offset scale)
      operand
    (setf register (decode-set (find-register-decode-set 'sib-base-00)
			       (slot-value instr-symbolic 'base))
	  offset 0
	  scale 1))
  (values operand))

(defmethod operand-encode ((operand operand-indirect-register)
			   (encoding (eql 'indirect-register-00-sib-index4))
			   operand-type
			   instr-symbolic)
  (declare (ignore operand-type))
  (with-slots (mod r/m index base scale)
      instr-symbolic
    (setf mod #b00
	  r/m #b100
	  index #b100
	  base (decode-set (find-register-encode-set 'sib-base-00)
			   (slot-value operand 'register))
	  scale 0))
  (values instr-symbolic '(mod r/m index base scale)))
	  

;;; Indirect pointer with MOD=#b00, R/M=#b100 in ModR/M
;;; and base=#b101 and index=#b100 in SIB.

(def-operand-class-imodrm (indirect-pointer-00-sib-index4-base5) (operand-direct) ())

(defmethod operand-and-encoding-unify ((operand operand-direct)
				       (encoding (eql 'indirect-pointer-00-sib-index4-base5))
				       operand-type
				       template instr
				       env)
  (declare (ignore template instr))
  (let ((resolved-operand (resolve-direct operand env)))
    (assert (member operand-type +operand-types-indirect-modrm+))
    (and (null (slot-value resolved-operand 'segment))
	 resolved-operand)))

(defmethod operand-decode ((operand operand-direct)
			   (encoding (eql 'indirect-pointer-00-sib-index4-base5))
			   instr-symbolic)
  (assert (= #b00 (slot-value instr-symbolic 'mod)))
  (assert (= #b100 (slot-value instr-symbolic 'r/m)))
  (assert (= #b101 (slot-value instr-symbolic 'base)))
  (assert (= #b100 (slot-value instr-symbolic 'index)))
  (setf (slot-value operand 'address)
    (realpart (slot-value instr-symbolic 'displacement)))
  (values operand))

(defmethod operand-encode ((operand operand-direct)
			   (encoding (eql 'indirect-pointer-00-sib-index4-base5))
			   operand-type
			   instr-symbolic)
  (declare (ignore operand-type))
  (with-slots (mod r/m base index scale displacement)
      instr-symbolic
    (setf mod #b00
	  r/m #b100
	  base #b101
	  index #b100
	  scale 0 ; don't care
	  displacement (slot-value operand 'address)))
  (values instr-symbolic '(mod r/m base index displacement)))

;;; Indirect pointer with MOD=#b00, R/M=#b101 in ModR/M

(def-operand-class-imodrm (indirect-pointer-00) (operand-direct) ())

(defmethod operand-and-encoding-unify ((operand operand-direct)
				       (encoding (eql 'indirect-pointer-00))
				       operand-type
				       template instr
				       env)
  (declare (ignore template instr))
  (assert (member operand-type +operand-types-indirect-modrm+))
  (let ((resolved-operand (resolve-direct operand env)))
    (and (null (slot-value resolved-operand 'segment))
	 resolved-operand)))

(defmethod operand-decode ((operand operand-direct)
			   (encoding (eql 'indirect-pointer-00))
			   instr-symbolic)
  (assert (= #b00 (slot-value instr-symbolic 'mod)))
  (assert (= #b101 (slot-value instr-symbolic 'r/m)))
  (setf (slot-value operand 'address)
    (realpart (slot-value instr-symbolic 'displacement)))
  (values operand))

(defmethod operand-encode ((operand operand-direct)
			   (encoding (eql 'indirect-pointer-00))
			   operand-type
			   instr-symbolic)
  (declare (ignore operand-type))
  (with-slots (mod r/m displacement)
      instr-symbolic
    (setf mod #b00
	  r/m #b101
	  displacement (realpart (slot-value operand 'address))))
  (values instr-symbolic '(mod r/m displacement)))


;;; Indirect register with MOD=#b01, R/M/=#b100 in ModR/M.

(def-operand-class-imodrm (indirect-register-01) (operand-indirect-register) ())

(defmethod operand-and-encoding-unify ((operand operand-indirect-register)
				       (encoding (eql 'indirect-register-01))
				       operand-type
				       template instr
				       env)
  (declare (ignore template instr))
  (assert (member operand-type +operand-types-indirect-modrm+))
  (let ((resolved-operand (resolve-indirect-register operand env)))
    (with-slots (register register2 offset scale)
	resolved-operand
      (and (= 1 scale)
	   (member register '(eax ecx edx ebx ebp esi edi))
	   (not register2)
	   (<= -128 offset 127)
	   resolved-operand))))
  
(defmethod operand-decode ((operand operand-indirect-register)
			   (encoding (eql 'indirect-register-01))
			   instr-symbolic)
  (assert (= #b01 (slot-value instr-symbolic 'mod)))
  (assert (/= #b100 (slot-value instr-symbolic 'r/m)))
  (with-slots (mod r/m displacement)
      instr-symbolic
    (with-slots (register offset)
	operand
      (setf register (decode-set (find-register-decode-set 'r/m32-01)
				 r/m)
	    offset (realpart displacement))))
  (values operand))

(defmethod operand-encode ((operand operand-indirect-register)
			   (encoding (eql 'indirect-register-01))
			   operand-type
			   instr-symbolic)
  (declare (ignore operand-type))
  (with-slots (mod r/m displacement)
      instr-symbolic
    (setf mod #b01
	  r/m (decode-set (find-register-encode-set 'r/m32-01)
			  (slot-value operand 'register))
	  displacement (realpart (slot-value operand 'offset))))
  (values instr-symbolic '(mod r/m displacement)))

;;; Indirect register with MOD=#b01, R/M=#b100 in ModR/M,
;;; index/=#b100 in SIB.

(def-operand-class-imodrm (indirect-register-01-sib) (operand-indirect-register) ())

(defmethod operand-and-encoding-unify ((operand operand-indirect-register)
				       (encoding (eql 'indirect-register-01-sib))
				       operand-type
				       template instr
				       env)
  (declare (ignore template instr))
  (assert (member operand-type +operand-types-indirect-modrm+))
  (let ((resolved-operand (resolve-indirect-register operand env)))
    (with-slots (register register2 offset scale)
	resolved-operand
      (cond
       ((and (member register '(eax ecx edx ebx ebp esi edi))
	     (member register2 '(eax ecx edx ebx esp ebp esi edi))
	     (member scale '(1 2 4 8) :test #'=)
	     (<= -128 offset 127))
	resolved-operand)
       ((and (member register2 '(eax ecx edx ebx ebp esi edi))
	     (member register '(eax ecx edx ebx esp ebp esi edi))
	     (= scale 1)
	     (<= -128 offset 127))
	;; exchange register and register2
	(make-instance 'operand-indirect-register
	  'offset offset
	  'register register2
	  'register2 register
	  'scale 1))
       (t nil)))))
	  
(defmethod operand-decode ((operand operand-indirect-register)
			   (encoding (eql 'indirect-register-01-sib))
			   instr-symbolic)
  (assert (= #b01 (slot-value instr-symbolic 'mod)))
  (assert (= #b100 (slot-value instr-symbolic 'r/m)))
  (assert (/= #b100 (slot-value instr-symbolic 'index)))
  (with-slots (register register2 scale offset)
      operand
    (setf register (decode-set (find-register-decode-set 'sib-index)
			       (slot-value instr-symbolic 'index))
	  scale (expt 2 (slot-value instr-symbolic 'scale))
	  register2 (decode-set (find-register-decode-set 'sib-base)
				(slot-value instr-symbolic 'base))
	  offset (realpart (slot-value instr-symbolic 'displacement)))) ; disp8
  (values operand))


(defmethod operand-encode ((operand operand-indirect-register)
			   (encoding (eql 'indirect-register-01-sib))
			   operand-type
			   instr-symbolic)
  (declare (ignore operand-type))
  (with-slots (mod r/m scale index base displacement)
      instr-symbolic
    (setf mod #b01
	  r/m #b100
	  index (decode-set (find-register-encode-set 'sib-index)
			    (slot-value operand 'register))
	  scale (1- (integer-length (slot-value operand 'scale)))
	  base (decode-set (find-register-encode-set 'sib-base)
			   (slot-value operand 'register2))
	  displacement (slot-value operand 'offset)))
  (values instr-symbolic '(mod r/m scale index base displacement)))


;;; Indirect register with MOD=#b01, R/M=#b100 in ModR/M,
;;; index=#b100 in SIB.

(def-operand-class-imodrm (indirect-register-01-sib-index4) (operand-indirect-register) ())

(defmethod operand-and-encoding-unify ((operand operand-indirect-register)
				       (encoding (eql 'indirect-register-01-sib-index4))
				       operand-type
				       template instr
				       env)
  (declare (ignore template instr))
  (assert (member operand-type +operand-types-indirect-modrm+))
  (let ((resolved-operand (resolve-indirect-register operand env)))
    (with-slots (register register2 offset scale)
	resolved-operand
      (and (member register '(eax ecx edx ebx esp ebp esi edi))
	   (not register2)
	   (<= -128 offset 127)
	   (= 1 scale)
	   resolved-operand))))

(defmethod operand-decode ((operand operand-indirect-register)
			   (encoding (eql 'indirect-register-01-sib-index4))
			   instr-symbolic)
  (assert (= #b01 (slot-value instr-symbolic 'mod)))
  (assert (= #b100 (slot-value instr-symbolic 'r/m)))
  (assert (= #b100 (slot-value instr-symbolic 'index)))
  (with-slots (register offset scale)
      operand
    (setf register (decode-set (find-register-decode-set 'sib-base)
			       (slot-value instr-symbolic 'base))
	  offset (realpart (slot-value instr-symbolic 'displacement))
	  scale 1))
  (values operand))

(defmethod operand-encode ((operand operand-indirect-register)
			   (encoding (eql 'indirect-register-01-sib-index4))
			   operand-type
			   instr-symbolic)
  (declare (ignore operand-type))
  (with-slots (mod r/m index base scale displacement)
      instr-symbolic
    (setf mod #b01
	  r/m #b100
	  index #b100
	  base (decode-set (find-register-encode-set 'sib-base)
			   (slot-value operand 'register))
	  scale 0			; don't care
	  displacement (slot-value operand 'offset)))
  (values instr-symbolic '(mod r/m index base scale displacement)))


;;; Indirect register with MOD=#b10, R/M/=#b100 in ModR/M.

(def-operand-class-imodrm (indirect-register-10) (operand-indirect-register) ())

(defmethod operand-and-encoding-unify ((operand operand-indirect-register)
				       (encoding (eql 'indirect-register-10))
				       operand-type
				       template instr
				       env)
  (declare (ignore template instr))
  (assert (member operand-type +operand-types-indirect-modrm+))
  (let ((resolved-operand (resolve-indirect-register operand env)))
    (with-slots (register register2 offset scale)
	resolved-operand
      (and (= 1 scale)
	   (member register '(eax ecx edx ebx ebp esi edi))
	   (not register2)
	   (<= #x-80000000 offset #xffffffff)
	   resolved-operand))))

(defmethod operand-decode ((operand operand-indirect-register)
			   (encoding (eql 'indirect-register-10))
			   instr-symbolic)
  (assert (= #b10 (slot-value instr-symbolic 'mod)))
  (assert (/= #b100 (slot-value instr-symbolic 'r/m)))
  (with-slots (mod r/m displacement)
      instr-symbolic
    (with-slots (register offset)
	operand
      (setf register (decode-set (find-register-decode-set 'r/m32-01)
				 r/m)
	    offset (realpart displacement))))
  (values operand))
(defmethod operand-encode ((operand operand-indirect-register)
			   (encoding (eql 'indirect-register-10))
			   operand-type
			   instr-symbolic)
  (declare (ignore operand-type))
  (with-slots (mod r/m displacement)
      instr-symbolic
    (setf mod #b10
	  r/m (decode-set (find-register-encode-set 'r/m32-01)
			  (slot-value operand 'register))
	  displacement (realpart (slot-value operand 'offset))))
  (values instr-symbolic '(mod r/m displacement)))

;;; Indirect register with MOD=#b10, R/M=#b100 in ModR/M,
;;; index/=#b100 in SIB.

(def-operand-class-imodrm (indirect-register-10-sib) (operand-indirect-register) ())

(defmethod operand-and-encoding-unify ((operand operand-indirect-register)
				       (encoding (eql 'indirect-register-10-sib))
				       operand-type
				       template instr
				       env)
  (declare (ignore template instr))
  (assert (member operand-type +operand-types-indirect-modrm+))
  (let ((resolved-operand (resolve-indirect-register operand env)))
    (with-slots (register register2 offset scale)
	resolved-operand
      (and (member register '(eax ecx edx ebx ebp esi edi))
	   (member register2 '(eax ecx edx ebx esp ebp esi edi))
	   (member scale '(1 2 4 8))
	   (<= #x-80000000 offset #xffffffff)
	   resolved-operand))))

(defmethod operand-decode ((operand operand-indirect-register)
			   (encoding (eql 'indirect-register-10-sib))
			   instr-symbolic)
  (assert (= #b10 (slot-value instr-symbolic 'mod)))
  (assert (= #b100 (slot-value instr-symbolic 'r/m)))
  (assert (/= #b100 (slot-value instr-symbolic 'index)))
  (with-slots (register register2 scale offset)
      operand
    (setf register (decode-set (find-register-decode-set 'sib-index)
			       (slot-value instr-symbolic 'index))
	  scale (expt 2 (slot-value instr-symbolic 'scale))
	  register2 (decode-set (find-register-decode-set 'sib-base)
				(slot-value instr-symbolic 'base))
	  offset (realpart (slot-value instr-symbolic 'displacement)))) ; disp8
  (values operand))


(defmethod operand-encode ((operand operand-indirect-register)
			   (encoding (eql 'indirect-register-10-sib))
			   operand-type
			   instr-symbolic)
  (declare (ignore operand-type))
  (with-slots (mod r/m scale index base displacement)
      instr-symbolic
    (setf mod #b10
	  r/m #b100
	  index (decode-set (find-register-encode-set 'sib-index)
			    (slot-value operand 'register))
	  scale (1- (integer-length (slot-value operand 'scale)))
	  base (decode-set (find-register-encode-set 'sib-base)
			   (slot-value operand 'register2))
	  displacement (slot-value operand 'offset)))
  (values instr-symbolic '(mod r/m scale index base displacement)))

;;; Indirect register with MOD=#b10, R/M=#b100 in ModR/M,
;;; index=#b100 in SIB.

(def-operand-class-imodrm (indirect-register-10-sib-index4) (operand-indirect-register) ())

(defmethod operand-and-encoding-unify ((operand operand-indirect-register)
				       (encoding (eql 'indirect-register-10-sib-index4))
				       operand-type
				       template instr
				       env)
  (declare (ignore template instr))
  (assert (member operand-type +operand-types-indirect-modrm+))
  (let ((resolved-operand (resolve-indirect-register operand env)))
    (with-slots (register register2 offset scale)
	resolved-operand
      (and (member register '(eax ecx edx ebx esp ebp esi edi))
	   (not register2)
	   (<= #x-80000000 offset #xffffffff)
	   (= 1 scale)
	   resolved-operand))))

(defmethod operand-decode ((operand operand-indirect-register)
			   (encoding (eql 'indirect-register-10-sib-index4))
			   instr-symbolic)
  (assert (= #b10 (slot-value instr-symbolic 'mod)))
  (assert (= #b100 (slot-value instr-symbolic 'r/m)))
  (assert (= #b100 (slot-value instr-symbolic 'index)))
  (with-slots (register offset scale)
      operand
    (setf register (decode-set (find-register-decode-set 'sib-base)
			       (slot-value instr-symbolic 'base))
	  offset (realpart (slot-value instr-symbolic 'displacement))
	  scale 1))
  (values operand))

(defmethod operand-encode ((operand operand-indirect-register)
			   (encoding (eql 'indirect-register-10-sib-index4))
			   operand-type
			   instr-symbolic)
  (declare (ignore operand-type))
  (with-slots (mod r/m index base scale displacement)
      instr-symbolic
    (setf mod #b10
	  r/m #b100
	  index #b100
	  base (decode-set (find-register-encode-set 'sib-base)
			   (slot-value operand 'register))
	  scale 0			; don't care
	  displacement (slot-value operand 'offset)))
  (values instr-symbolic '(mod r/m index base scale displacement)))

;;; Indirect 16-bit register with MOD=#b00, R/M/=#b110 in ModR/M,

(def-operand-class-imodrm (16bit-indirect-register-mod00) (operand-indirect-register) ())

(defmethod operand-and-encoding-unify ((operand operand-indirect-register)
				       (encoding (eql '16bit-indirect-register-mod00))
				       operand-type
				       template instr
				       env)
  (declare (ignore template instr))
  (assert (member operand-type +operand-types-indirect-modrm+))
  (let ((resolved-operand (resolve-indirect-register operand env)))
    (with-slots (register register2 offset scale)
	resolved-operand
      (and (= offset 0)
	   (= scale 1)
	   (if register2
	       (and (member register '(bx bp))
		    (member register2 '(si di)))
	     (member register '(si di bx)))
	   resolved-operand))))

(defmethod operand-decode ((operand operand-indirect-register)
			   (encoding (eql '16bit-indirect-register-mod00))
			   instr-symbolic)
  (assert (= #b00 (slot-value instr-symbolic 'mod)))
  (assert (/= #b110 (slot-value instr-symbolic 'r/m)))
  (with-slots (register register2 offset scale)
      operand
    (destructuring-bind (r1 . r2)
	(decode-set (find-register-decode-set 'r/m-16bit)
		    (slot-value instr-symbolic 'r/m))
      (setf register r1
	    register2 r2
	    offset 0
	    scale 1)))
  (values operand))

(defmethod operand-encode ((operand operand-indirect-register)
			   (encoding (eql '16bit-indirect-register-mod00))
			   operand-type
			   instr-symbolic)
  (declare (ignore operand-type))
  (with-slots (mod r/m)
      instr-symbolic
    (setf mod #b00
	  r/m (decode-set (find-register-encode-set 'r/m-16bit)
			  (cons (slot-value operand 'register)
				(slot-value operand 'register2)))))
  (values instr-symbolic '(mod r/m)))

;;; Indirect 16bit pointer with MOD=#b00, R/M=#b110 in ModR/M.

(def-operand-class-imodrm (16bit-indirect-pointer) (operand-direct) ())

(defmethod operand-and-encoding-unify ((operand operand-direct)
				       (encoding (eql '16bit-indirect-pointer))
				       operand-type
				       template instr
				       env)
  (declare (ignore template instr))
  (assert (member operand-type +operand-types-indirect-modrm+))
  (let ((resolved-operand (resolve-direct operand env)))
    (with-slots (address segment)
	resolved-operand
      (and (null segment)
	   (<= 0 address #xffff)
	   resolved-operand))))

(defmethod operand-decode ((operand operand-direct)
			   (encoding (eql '16bit-indirect-pointer))
			   instr-symbolic)
  (assert (= #b00 (slot-value instr-symbolic 'mod)))
  (assert (= #b110 (slot-value instr-symbolic 'r/m)))
  (with-slots (address)
      operand
    (setf address (realpart (slot-value instr-symbolic 'displacement))))
  (values operand))

(defmethod operand-encode ((operand operand-direct)
			   (encoding (eql '16bit-indirect-pointer))
			   operand-type
			   instr-symbolic)
  (declare (ignore operand-type))
  (with-slots (mod r/m displacement)
      instr-symbolic
    (setf mod #b00
	  r/m #b110
	  displacement (slot-value operand 'address)))
  (values instr-symbolic '(mod r/m displacement)))

;;; Indirect 16-bit register with MOD=#b01 in ModR/M.

(def-operand-class-imodrm (16bit-indirect-register-mod01) (operand-indirect-register) ())

(defmethod operand-and-encoding-unify ((operand operand-indirect-register)
				       (encoding (eql '16bit-indirect-register-mod01))
				       operand-type
				       template instr
				       env)
  (declare (ignore template instr))
  (assert (member operand-type +operand-types-indirect-modrm+))
  (let ((resolved-operand (resolve-indirect-register operand env)))
    (with-slots (register register2 offset scale)
	resolved-operand
      (and (= scale 1)
	   (<= -128 offset 127)
	   (if register2
	       (and (member register '(bx bp))
		    (member register2 '(si di)))
	     (member register '(si di bp bx)))
	   resolved-operand))))

(defmethod operand-decode ((operand operand-indirect-register)
			   (encoding (eql '16bit-indirect-register-mod01))
			   instr-symbolic)
  (assert (= #b01 (slot-value instr-symbolic 'mod)))
  (with-slots (register register2 offset scale)
      operand
    (destructuring-bind (r1 . r2)
	(decode-set (find-register-decode-set 'r/m-16bit)
		    (slot-value instr-symbolic 'r/m))
      (setf register r1
	    register2 r2
	    offset (sign-extend (realpart (slot-value instr-symbolic 'displacement)) 1)
	    scale 1)))
  (values operand))

(defmethod operand-encode ((operand operand-indirect-register)
			   (encoding (eql '16bit-indirect-register-mod01))
			   operand-type
			   instr-symbolic)
  (declare (ignore operand-type))
  (with-slots (mod r/m displacement)
      instr-symbolic
    (setf mod #b01
	  r/m (decode-set (find-register-encode-set 'r/m-16bit)
			  (cons (slot-value operand 'register)
				(slot-value operand 'register2)))
	  displacement (slot-value operand 'offset)))
  (values instr-symbolic '(mod r/m displacement)))

;;; Indirect 16-bit register with MOD=#b10 in ModR/M.

(def-operand-class-imodrm (16bit-indirect-register-mod10) (operand-indirect-register) ())

(defmethod operand-and-encoding-unify ((operand operand-indirect-register)
				       (encoding (eql '16bit-indirect-register-mod10))
				       operand-type
				       template instr
				       env)
  (declare (ignore template instr))
  (assert (member operand-type +operand-types-indirect-modrm+))
  (let ((resolved-operand (resolve-indirect-register operand env)))
    (with-slots (register register2 offset scale)
	resolved-operand
      (and (= scale 1)
	   (<= 0 offset #xffff)
	   (if register2
	       (and (member register '(bx bp))
		    (member register2 '(si di)))
	     (member register '(si di bp bx)))
	   resolved-operand))))

(defmethod operand-decode ((operand operand-indirect-register)
			   (encoding (eql '16bit-indirect-register-mod10))
			   instr-symbolic)
  (assert (= #b10 (slot-value instr-symbolic 'mod)))
  (with-slots (register register2 offset scale)
      operand
    (destructuring-bind (r1 . r2)
	(decode-set (find-register-decode-set 'r/m-16bit)
		    (slot-value instr-symbolic 'r/m))
      (setf register r1
	    register2 r2
	    offset (realpart (slot-value instr-symbolic 'displacement))
	    scale 1)))
  (values operand))

(defmethod operand-encode ((operand operand-indirect-register)
			   (encoding (eql '16bit-indirect-register-mod10))
			   operand-type
			   instr-symbolic)
  (declare (ignore operand-type))
  (assert (<= 0 (slot-value operand 'offset) #xffff))
  (with-slots (mod r/m displacement)
      instr-symbolic
    (setf mod #b10
	  r/m (decode-set (find-register-encode-set 'r/m-16bit)
			  (cons (slot-value operand 'register)
				(slot-value operand 'register2)))
	  displacement (slot-value operand 'offset)))
  (values instr-symbolic '(mod r/m displacement)))

;;; Absolute pointer encoded in the moffs operand-type

(def-operand-class (abs-pointer-moffs (moffs8 moffs16 moffs32))
    (operand-direct) ())

(defmethod operand-and-encoding-unify ((operand operand-direct)
				       (encoding (eql 'abs-pointer-moffs))
				       operand-type
				       template instr
				       env)
  (declare (ignore template instr))
  (let ((resolved-operand (resolve-direct operand env)))
    (with-slots (address)
	resolved-operand
      (and
       (ecase operand-type
	 (moffs8  (<= 0 address #xff))
	 (moffs16 (<= 0 address #xffff))
	 (moffs32 (<= 0 address #xffffffff)))
       resolved-operand))))
  
(defmethod operand-decode ((operand operand-direct)
			   (encoding (eql 'abs-pointer-moffs))
			   instr-symbolic)
  (with-slots (address)
      operand
    (setf address (realpart (slot-value instr-symbolic 'displacement))))
  (values operand))

(defmethod operand-encode ((operand operand-direct)
			   (encoding (eql 'abs-pointer-moffs))
			   operand-type
			   instr-symbolic)
  (declare (ignore operand-type))
  (with-slots (displacement)
      instr-symbolic
    (setf displacement (slot-value operand 'address)))
  (values instr-symbolic '(displacement)))

;;; Register constants (no encoding)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +constant-register-operands+
      '(al ah ax eax
	bl bh bx ebx
	dl dh dx edx
	cl ch cx ecx
	cs ds es fs gs ss
	cr0 cr2 cr3 cr4
	dr0 dr1 dr2 dr3 dr4 dr5 dr6 dr7)))

(defmacro def-many-constant-registers (cr-list)
  (cons 'cl:progn
	(loop for cr in (symbol-value cr-list)
	    collect `(def-operand-class (register-constant (,cr)) (operand-register) ()))))

(def-many-constant-registers +constant-register-operands+)

(defmethod operand-and-encoding-unify ((operand operand-register)
				       (encoding (eql 'register-constant))
				       operand-type
				       template instr
				       env)
  (declare (ignore template instr env))
  (assert (member operand-type +constant-register-operands+))
  (and (eq operand-type
	   (slot-value operand 'register))
       operand))

(defmethod operand-decode ((operand operand-register)
			   (encoding (eql 'register-constant))
			   instr-symbolic)
  (declare (ignore instr-symbolic))
  (with-slots (register)
      operand
    (setf register (operand-class-register-set (class-of operand))))
  (values operand))

(defmethod operand-encode ((operand operand-register)
			   (encoding (eql 'register-constant))
			   operand-type
			   instr-symbolic)
  (declare (ignore operand-type))
  (values instr-symbolic '()))


;;; Immediate constants (no encoding)

(def-operand-class (register-constant (1)) (operand-immediate) ())

(defmethod operand-and-encoding-unify ((operand operand-immediate)
				       (encoding (eql 'register-constant))
				       operand-type
				       template instr
				       env)
  (declare (ignore template instr env))
  (and (= operand-type
	  (slot-value operand 'value))
   operand))

(defmethod operand-decode ((operand operand-immediate)
			   (encoding (eql 'register-constant))
			   instr-symbolic)
  (declare (ignore instr-symbolic))
  (with-slots (value)
      operand
    (setf value (operand-class-register-set (class-of operand))))
  (values operand))

(defmethod operand-encode ((operand operand-immediate)
			   (encoding (eql 'register-constant))
			   operand-type
			   instr-symbolic)
  (declare (ignore operand-type))
  (values instr-symbolic '()))


;;; Register encoded in the opcode (plus-format).

(def-operand-class (register-plus (+r8)) (operand-register) ())
(def-operand-class (register-plus (+r16)) (operand-register) ())
(def-operand-class (register-plus (+r32)) (operand-register) ())

(defmethod operand-and-encoding-unify ((operand operand-register)
				       (encoding (eql 'register-plus))
				       operand-type
				       template instr
				       env)
  (declare (ignore template instr env))
  (with-slots (register)
      operand
    (and (ecase operand-type
	   ((+r8) (member register '(al cl dl bl ah ch dh bh)))
	   ((+r16) (member register '(ax cx dx bx sp bp si di)))
	   ((+r32) (member register '(eax ecx edx ebx esp ebp esi edi))))
	 operand)))

(defmethod operand-decode ((operand operand-register)
			   (encoding (eql 'register-plus))
			   instr-symbolic)
  (with-slots (register)
      operand
    (setf register
      (decode-set (find-register-decode-set (operand-class-register-set (class-of operand)))
		  (ldb (byte 3 0)
		       (slot-value instr-symbolic 'opcode)))))
  (values operand))

(defmethod operand-encode ((operand operand-register)
			   (encoding (eql 'register-plus))
			   operand-type
			   instr-symbolic)
  (with-slots (opcode)
      instr-symbolic
    (setf (ldb (byte 3 0) opcode)
      (decode-set (find-register-encode-set operand-type)
		  (slot-value operand 'register))))
  (values instr-symbolic '(opcode)))


;;; Immediate values

(def-operand-class (immediate (imm8 simm8 imm16 simm32 imm32)) (operand-immediate) ())

(defmethod operand-and-encoding-unify ((operand operand-immediate)
				       (encoding (eql 'immediate))
				       operand-type
				       template instr
				       env)
  (declare (ignore template instr env))
  (with-slots (value)
      operand
    (and (ecase operand-type
	   (simm8 (<= #x-80 value #x7f))
	   (imm8  (<= 0 value #xff))
	   (imm16 (<= 0 value #xffff))
	   (simm32 (<= #x-80000000 value #xffffffff))
	   (imm32 (<= 0 value #xffffffff)))
	 operand)))

(defmethod operand-and-encoding-unify ((operand abstract-operand)
				       (encoding (eql 'immediate))
				       operand-type
				       template instr
				       env)
  (operand-and-encoding-unify (make-instance 'operand-immediate
				'value (operand-resolve-to-number operand env))
			      encoding operand-type
			      template instr env))

(defmethod operand-decode ((operand operand-immediate)
			   (encoding (eql 'immediate))
			   instr-symbolic)
  (with-slots (value)
      operand
    (setf value (sign-extend (realpart (slot-value instr-symbolic 'immediate))
			     (imagpart (slot-value instr-symbolic 'immediate)))))
  (values operand))

(defmethod operand-encode ((operand operand-immediate)
			   (encoding (eql 'immediate))
			   operand-type
			   instr-symbolic)
  (declare (ignore operand-type))
  (unless (instr-symbolic-reg instr-symbolic)
    (setf (instr-symbolic-reg instr-symbolic) 0)) ; don't care)
  (with-slots (immediate)
      instr-symbolic
    (setf immediate (slot-value operand 'value)))
  (values instr-symbolic '(immediate)))

;;; PC-relative addresses

(def-operand-class (pc-relative (rel8 rel16 rel32)) (operand-rel-pointer) ())

(defmethod operand-and-encoding-unify ((operand operand-rel-pointer)
				       (encoding (eql 'pc-relative))
				       operand-type
				       template instr
				       env)
  (declare (ignore template instr env))
  (with-slots (offset)
      operand
    (and (ecase operand-type
	   ((rel8)  (<= #x-80 offset #x7f))
	   ((rel16) (<= #x-8000 offset #x7fff))
	   ((rel32) (<= #x-80000000 offset #x7fffffff)))
	 operand)))

(defmethod operand-and-encoding-unify ((operand abstract-operand)
				       (encoding (eql 'pc-relative))
				       operand-type
				       template instr
				       env)
  (ecase operand-type
    ((rel8 rel32)
     (operand-and-encoding-unify (make-instance 'operand-rel-pointer
				   'offset (abstract-operand-to-offset operand
								       template
								       instr
								       env))
				 encoding operand-type
				 template instr env))
    ((rel16)
     ;; rel16 operands cause EIP to be masked with #x0000ffff
     (and (<= 0 (operand-resolve-to-number operand env) #x0000ffff)
	  (operand-and-encoding-unify (make-instance 'operand-rel-pointer
					'offset (abstract-operand-to-offset operand
									    template
									    instr
									    env))
				      encoding operand-type
				      template instr env)))))
	  
(defmethod operand-decode ((operand operand-rel-pointer)
			   (encoding (eql 'pc-relative))
			   instr-symbolic)
  (with-slots (offset)
      operand
    (setf offset
      (realpart (slot-value instr-symbolic 'displacement))))
  (values operand))

(defmethod operand-encode ((operand operand-rel-pointer)
			   (encoding (eql 'pc-relative))
			   operand-type
			   instr-symbolic)
  (declare (ignore operand-type))
  (with-slots (displacement)
      instr-symbolic
    (setf displacement
      (slot-value operand 'offset)))
  (values instr-symbolic '(displacement)))

;;; 32-bit Segmented addresses

(def-operand-class (ptr16-32 (ptr16-32)) (operand-direct) ())

(defmethod operand-and-encoding-unify ((operand operand-direct)
				       (encoding (eql 'ptr16-32))
				       operand-type
				       template instr
				       env)
  (declare (ignore template instr))
  (assert (eq operand-type 'ptr16-32))
  (let ((resolved-operand (resolve-direct operand env)))
    (with-slots (address segment)
	resolved-operand
      (and address
	   (<= 0 address #xffffffff)
	   segment
	   (<= 0 segment #xffff)
	   resolved-operand))))

(defmethod operand-decode ((operand operand-direct)
			   (encoding (eql 'ptr16-32))
			   instr-symbolic)
  (with-slots (address segment)
      operand
    (setf
	address (ldb (byte 32 0) (realpart (instr-symbolic-displacement instr-symbolic)))
	segment (ldb (byte 16 32) (realpart (instr-symbolic-displacement instr-symbolic)))))
  (values operand))

(defmethod operand-encode ((operand operand-direct)
			   (encoding (eql 'ptr16-32))
			   operand-type
			   instr-symbolic)
  (declare (ignore operand-type))
  (with-slots (displacement)
      instr-symbolic
    (setf
	(ldb (byte 32 0) displacement) (slot-value operand 'address)
	(ldb (byte 16 32) displacement) (slot-value operand 'segment)))
  (values instr-symbolic '(displacement)))

;;; 16-bit Segmented addresses

(def-operand-class (ptr16-16 (ptr16-16)) (operand-direct) ())

(defmethod operand-and-encoding-unify ((operand operand-direct)
				       (encoding (eql 'ptr16-16))
				       operand-type
				       template instr
				       env)
  (declare (ignore template instr))
  (assert (eq operand-type 'ptr16-16))
  (let ((resolved-operand (resolve-direct operand env)))
    (with-slots (address segment)
	resolved-operand
      (and address
	   (<= 0 address #xffff)
	   segment
	   (<= 0 segment #xffff)
	   resolved-operand))))

(defmethod operand-decode ((operand operand-direct)
			   (encoding (eql 'ptr16-16))
			   instr-symbolic)
  (with-slots (address segment)
      operand
    (setf
	address (ldb (byte 16 0) (realpart (instr-symbolic-displacement instr-symbolic)))
	segment (ldb (byte 16 16) (realpart (instr-symbolic-displacement instr-symbolic)))))
  (values operand))

(defmethod operand-encode ((operand operand-direct)
			   (encoding (eql 'ptr16-16))
			   operand-type
			   instr-symbolic)
  (declare (ignore operand-type))
  (with-slots (displacement)
      instr-symbolic
    (setf displacement 0
	  (ldb (byte 16 0) displacement) (slot-value operand 'address)
	  (ldb (byte 16 16) displacement) (slot-value operand 'segment)))
  (values instr-symbolic '(displacement)))

;;; Two immediate operands (for ENTER)

(def-operand-class (imm16-8 (imm16-8)) (operand-immediate) ())

(defmethod operand-and-encoding-unify ((operand operand-immediate)
				       (encoding (eql 'imm16-8))
				       operand-type
				       template instr
				       env)
  (declare (ignore template instr env))
  (assert (eq operand-type 'imm16-8))
  (with-slots (value)
      operand
    (and (<= 0 value #xffff)
	 operand)))

(defmethod operand-decode ((operand operand-immediate)
			   (encoding (eql 'imm16-8))
			   instr-symbolic)
  (with-slots (value)
      operand
    (setf value (ldb (byte 16 0)
		     (realpart (slot-value instr-symbolic 'immediate)))))
  (values operand))
					   
(defmethod operand-encode ((operand operand-immediate)
			   (encoding (eql 'imm16-8))
			   operand-type
			   instr-symbolic)
  (assert (eq operand-type 'imm16-8)
      (operand-type))
  (unless (instr-symbolic-immediate instr-symbolic)
    (setf (slot-value instr-symbolic 'immediate) 0))
  (with-slots (immediate)
      instr-symbolic
    (setf (ldb (byte 16 0) immediate)
      (slot-value operand 'value)))
  (values instr-symbolic '(immediate)))
					   

;;; Two immediate operands (for ENTER)

(def-operand-class (imm8-0 (imm8-0)) (operand-immediate) ())

(defmethod operand-and-encoding-unify ((operand operand-immediate)
				       (encoding (eql 'imm8-0))
				       operand-type
				       template instr
				       env)
  (declare (ignore template instr env))
  (assert (eq operand-type 'imm8-0))
  (with-slots (value)
      operand
    (and (<= 0 value #x7f)
	 operand)))

(defmethod operand-decode ((operand operand-immediate)
			   (encoding (eql 'imm8-0))
			   instr-symbolic)
  (with-slots (value)
      operand
    (setf value (ldb (byte 8 16)
		     (realpart (slot-value instr-symbolic 'immediate)))))
  (values operand))
					   
(defmethod operand-encode ((operand operand-immediate)
			   (encoding (eql 'imm8-0))
			   operand-type
			   instr-symbolic)
  (assert (eq operand-type 'imm8-0)
      (operand-type))
  (unless (instr-symbolic-immediate instr-symbolic)
    (setf (slot-value instr-symbolic 'immediate) 0))
  (with-slots (immediate)
      instr-symbolic
    (setf (ldb (byte 8 16) immediate)
      (slot-value operand 'value)))
  (values instr-symbolic '(immediate)))
					   


