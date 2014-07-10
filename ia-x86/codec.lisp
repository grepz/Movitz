;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 2000, 2001, 2002, 2004-2005,
;;;;    Department of Computer Science, University of Tromso, Norway
;;;; 
;;;; Filename:      codec.lisp
;;;; Description:   Encoding and decoding of instructions to/from binary.
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Thu May  4 15:16:45 2000
;;;; Distribution:  See the accompanying file COPYING.
;;;;                
;;;; $Id: codec.lisp,v 1.8 2007/02/26 22:14:00 ffjeld Exp $
;;;;                
;;;;------------------------------------------------------------------

(in-package #:ia-x86)

(defparameter *error-on-eof* nil)
(defparameter *error-on-unknown-instruction* nil)

;;; ----------------------------------------------------------------
;;;                   Instruction template match
;;; ----------------------------------------------------------------

;;; A "template" is an object that is used to map op-codes to
;;; instructions. The basic problem it solves is that ia-x86 uses
;;; variable-length op-codes. Consequently, figuring out if the
;;; octets you are currently decoding represents a complete op-code
;;; is non-trivial.
;;;
;;; If you regard the X86 IA as huffman-comression-codes, the templates
;;; would be something like the nodes in the huffman tree.
;;;
;;; Templates are specialized to: 
;;;    * instr-template: represents actual instructions.
;;;    * prefix-template: represents instruction-prefixes.

(defconstant +template-default-priority+ 0)


(defclass template ()
  ((value
    :type unsigned-byte
    :initarg value
    :accessor template-value)
   (numo
    :type (integer 1 12)
    :initarg numo
    :accessor template-numo)
   (priority
    :type integer
    :initarg priority
    :initform +template-default-priority+
    :accessor template-priority)))

(defclass instr-template (template)
  ((mask
    :type signed-byte
    :initarg mask
    :accessor template-mask)
   (not-list
    :type list				; list of value/masks that must *not* match.
    :initform '()
    :initarg not-list
    :accessor template-not-list)
   (cpu-mode
    :type (member :32-bit :16-bit :any-mode)
    :initform :any-mode
    :initarg cpu-mode
    :accessor template-cpu-mode)
   (operand-mode
    :type (member :32-bit :16-bit :any-mode)
    :initform :any-mode
    :initarg operand-mode
    :accessor template-operand-mode)
   (addressing-mode
    :type (member :32-bit :16-bit :any-mode)
    :initform :any-mode
    :initarg addressing-mode
    :accessor template-addressing-mode)
   (req-prefixes
    :type list				; list of prefixes required for a match
    :initform '()
    :initarg req-prefixes
    :accessor template-req-prefixes)
   (not-prefixes
    :type list				; list of prefixes disqualifying any match
    :initform '()
    :initarg not-prefixes
    :accessor template-not-prefixes)
   (instr-classname
    :initarg instr-classname
    :accessor template-instr-classname)
   (instr-numo
    :type (integer 1 16)
    :initarg instr-numo
    :accessor template-instr-numo)
   (instr-operand-types
    :type list
    :initarg instr-operand-types
    :accessor template-instr-operand-types)
   (instr-operand-classes
    :type list				; this template generates operands of these classes (in this order)
    :initarg instr-operand-classes
    :accessor template-instr-operand-classes)
   (instr-operand-base-classes
    :type list	; caching the base-classes of instr-operand-classes
    :initarg instr-operand-base-classes
    :accessor template-instr-operand-base-classes)
   (instr-modr/m-p
    :type boolean			; does instruction have modr/m?
    :initarg modr/m-p
    :accessor template-instr-modr/m-p)
   (instr-sib-p
    :type boolean			; does instruction have SIB?
    :initarg sib-p
    :accessor template-instr-sib-p)
   (instr-displacement-numo
    :type (integer 0 4)			; size of "displacement" field.
    :initarg displacement-numo
    :accessor template-instr-displacement-numo)
   (instr-immediate-numo
    :type (integer 0 4)			; size of "immediate" field.
    :initarg immediate-numo
    :accessor template-instr-immediate-numo)
   (instr-opcode-numo
    :type (integer 1 16)		; size of "opcode" field.
    :initarg opcode-numo
    :accessor template-instr-opcode-numo)))

(defmethod print-object ((obj instr-template) stream)
  (if *print-pretty*
      (progn
	(format stream "#IT(~A)"
		(loop for slot in '(instr-classname cpu-mode operand-mode addressing-mode
				    req-prefixes not-prefixes instr-opcode-numo
				    instr-immediate-numo instr-displacement-numo
				    instr-sib-p instr-modr/m-p
				    instr-operand-classes instr-operand-base-classes
				    instr-operand-types)
		    when (slot-boundp obj slot)
		    collect (list slot (slot-value obj slot))))
	obj)
    (call-next-method obj stream)))

(defclass prefix-template (template)
  ((value
    :type (unsigned-byte 8))))

(defmethod template-numo ((template prefix-template))
  (values 1))

(defmethod template-match-datum-and-prefixes ((template prefix-template) datum datum-numo prefixes)
  (declare (ignore prefixes))
  (and (= 1 datum-numo)
       (= (template-value template)
	  datum)))

(defgeneric template-equal (t1 t2)
  (:documentation "Returns true when two templates are considered to be the same."))

(defmethod template-equal (t1 t2)
  (declare (ignore t1 t2))
  nil)

(defmethod template-equal ((t1 prefix-template) (t2 prefix-template))
  (= (template-value t1)
     (template-value t2)))

(defmethod template-equal ((t1 instr-template) (t2 instr-template))
  (and (= (template-value t1)
	  (template-value t2))
       (= (template-numo t1)
	  (template-numo t2))
       (= (template-mask t1)
	  (template-mask t2))
       (eq (template-operand-mode t1)
	   (template-operand-mode t2))
       (eq (template-addressing-mode t1)
	   (template-addressing-mode t2))
       (equalp (template-not-list t1)
	       (template-not-list t2))
       (equal (template-req-prefixes t1)
	      (template-req-prefixes t2))
       (equal (template-not-prefixes t1)
	      (template-not-prefixes t2))))

(defmethod template-match-opcode ((template prefix-template) datum datum-numo)
  (declare (ignore datum-numo))
  (= (template-value template)
     datum))

(defmethod template-match-opcode ((template instr-template) datum datum-numo)
  (assert (<= datum-numo
	      (template-numo template))
      (datum template)
    "Trying to match a template size ~A with ~A of size ~A"
    (template-numo template)
    datum
    datum-numo)
  (and (= (logand datum			; match value
		  (ldb #0=(byte (* 8 datum-numo)
				(* 8 (- (template-numo template)
					datum-numo)))
		       (template-mask template)))
	  (ldb #0#
	       (template-value template)))
       (every #'(lambda (not-spec)	; check against every element of not-list
		  (destructuring-bind (not-value not-mask)
		      not-spec
		    (let ((effective-not-mask (ldb #1=(byte (* 8 datum-numo)
							    (* 8 (- (template-numo template)
								    datum-numo)))
						   not-mask)))
		      (or (zerop effective-not-mask) ; not match not-value
			  (/= (logand datum
				      effective-not-mask)
			      (ldb #1#
				   not-value))))))
	      (template-not-list template))))

(defvar *cpu-mode* :32-bit
  "The assumed processor mode. One of :32-bit or :16-bit.")

(defun effective-operand-mode (prefixes)
  "Calcuclate the operand mode (:32-bit or :16-bit) of and instruction,
based on the cpu state (i.e. *cpu-mode*) and the active set of prefixes."
  (ecase *cpu-mode*
    ((:32-bit) (if (member '16-bit-operand prefixes) :16-bit :32-bit))
    ((:16-bit) (if (member '16-bit-operand prefixes) :32-bit :16-bit))))

(defun effective-addressing-mode (prefixes)
  "Calcuclate the addressing mode (:32-bit or :16-bit) of and instruction,
based on the cpu state (i.e. *cpu-mode*) and the active set of prefixes."
  (ecase *cpu-mode*
    ((:32-bit) (if (member '16-bit-address prefixes) :16-bit :32-bit))
    ((:16-bit) (if (member '16-bit-address prefixes) :32-bit :16-bit))))

(defmethod template-match-datum-and-prefixes ((template instr-template) datum datum-numo prefixes)
  (assert (< datum (ash 1 (* 8 datum-numo)))
      (datum datum-numo)
    "Datum ~A is too big for datum-numo ~A."
    datum datum-numo)
  (and (template-match-opcode template datum datum-numo)
       (or (eq :any-mode
	       (template-cpu-mode template))
	   (eq *cpu-mode*
	       (template-cpu-mode template)))
       (or (eq :any-mode
	       (template-operand-mode template))
	   (eq (effective-operand-mode prefixes)
	       (template-operand-mode template)))
       (or (eq :any-mode
	       (template-addressing-mode template))
	   (eq (effective-addressing-mode prefixes)
	       (template-addressing-mode template)))
       (every #'(lambda (required-prefix) ; match req-prefixes
		  (member required-prefix prefixes))
	      (template-req-prefixes template))
       (null (intersection prefixes	; not match not-prefixes
			   (template-not-prefixes template)))))

(defvar *template-table* (make-array #x100
				     :element-type 'list
				     :initial-element ())
  "For each 8-bit value (matching the first octet of an one-byte op-code),
this array holds a list of candidate template objects.")

(defvar *template-table-0f* (make-array #x100
					:element-type 'list
					:initial-element ())
  "For each 8-bit value (matching the second octet of a two-byte op-code),
this array holds a list of candidate template objects.")

(defvar *template-by-class-name* (make-hash-table :test #'eq)
  "For every instruction-class, we have a slot in this hash-table
where the key is the class' name and the value is a list of
that class' templates.")

(defun template-forget-all ()
  (setf *template-table* (make-array #x100
				     :element-type 'list
				     :initial-element nil)
	*template-table-0f* (make-array #x100
					:element-type 'list
					:initial-element nil)
	*template-by-class-name* (make-hash-table :test #'eq))
  (mapcar #'(lambda (pp)
	      (templates-remember nil (list (make-instance 'prefix-template
					     'value (car pp)))))
	  +opcode-prefix-map+))

(defun templates-remember (name template-list)
  (when name
    (setf (gethash name *template-by-class-name*) template-list)
    (loop for i from 0 to #xff
	when (typep (aref *template-table* i) 'instr-template)
	do (setf (aref *template-table* i)
	     (delete-if #'(lambda (template-to-delete)
			    (eq name (template-instr-classname template-to-delete)))
			(aref *template-table* i)))
	when (typep (aref *template-table-0f* i) 'instr-template)
	do (setf (aref *template-table-0f* i)
	     (delete-if #'(lambda (template-to-delete)
			    (eq name (template-instr-classname template-to-delete)))
			(aref *template-table-0f* i)))))
  (loop for template in template-list
      if (= #x0f (ldb (byte 8 (* 8 (1- (template-numo template))))
		      (realpart (template-value template))))
      do (loop for o from #x0f00 to #x0fff
	     and i from 0 to #xff
	     when (template-match-opcode template o 2)
	     do (pushnew template (aref *template-table-0f* i) :test #'template-equal))
      else do (loop for i from #x00 to #xff
		  when (template-match-opcode template i 1)
		  do (pushnew template (aref *template-table* i) :test #'template-equal))))

;;;  (cond
;;;   ((= #x0f (ldb (byte 8 (* 8 (1- (template-numo template))))
;;;		 (realpart (template-value template))))
;;;    (loop for o from #x0f00 to #x0fff
;;;	and i from 0 to #xff
;;;	when (template-match-opcode template o 2)
;;;	do (pushnew template (aref *template-table-0f i) #'template-equal)))

;;;	do (setf (aref *template-table-0f* i)
;;;	     (cons template
;;;		   (delete-if #'(lambda (tt) ; remove existing equal templates
;;;				  (and (or (typep template 'prefix-template)
;;;					   (eq (template-instr-classname template)
;;;					       (template-instr-classname tt)))
;;;				       (template-equal template tt)))
;;;			      (aref *template-table-0f* i))))))

;;;   (t
;;;    (dotimes (i #x100)
;;;      (when (template-match-opcode template i 1)
;;;	(setf (aref *template-table* i)
;;;	  (cons template
;;;		(delete-if #'(lambda (tt) ; remove existing equal templates
;;;			       (and (or (typep template 'prefix-template)
;;;					(eq (template-instr-class template)
;;;					(template-instr-class tt)))
;;;				    (template-equal template tt)))
;;;			   (aref *template-table* i)))))))))

(defun template-lookup-next (read-octet-fn)
  (let ((new-octet (funcall read-octet-fn)))
    (case new-octet
      ((nil) (when *error-on-eof*
	       (error "Unexpected EOF")))
      ((#x0f) (let ((newer-octet (funcall read-octet-fn)))
		(when (and (null newer-octet)
			   *error-on-eof*)
		  (error "Unexpected EOF 2"))
		(values (aref *template-table-0f* newer-octet)
			(complex (logior #x0f00 newer-octet) 2))))
      (t (values (aref *template-table* new-octet)
		 (complex new-octet 1))))))

(defun templates-lookup-by-class-name (class-name)
  (gethash class-name *template-by-class-name*))

(defun operand-type-p (operand class)
  (if (typep operand 'abstract-operand)
      (member (class-name class) '(operand-immediate operand-direct operand-rel-pointer))
    (typep operand class)))

;;;(defun template-match-by-operand-classes (template operand-list)
;;;  "This predicate returns true if the template matches the classes
;;;of the list of operands."
;;;  (and (= (length (template-instr-operand-base-classes template))
;;;	  (length operand-list))
;;;       (loop
;;;	   for top-class in (template-instr-operand-base-classes template)
;;;	   and operand in operand-list
;;;	   always (operand-type-p operand top-class))))

(defun template-match-by-operand-classes (template operand-list)
  "This predicate returns true if the template matches the classes
of the list of operands."
  (let ((class-list (template-instr-operand-base-classes template)))
    (or (and (null class-list)
	     (null operand-list))
	(and class-list
	     operand-list
	     (loop
		 for top-classes on class-list
		 and operands on operand-list
		 always (and (operand-type-p (car operands)
					     (car top-classes))
			     (if (null (cdr operands))
				 (null (cdr top-classes))
			       (cdr top-classes))))))))

(defun template-forget-class (class-name operand-types)
  (when (and (boundp '*template-by-class-name*)
	     (templates-lookup-by-class-name class-name))
    (loop for dead-template in (templates-lookup-by-class-name class-name)
;;;	with have-warned = nil
	when (equalp operand-types
		     (template-instr-operand-types dead-template))
;;;	do (unless have-warned
;;;	     (warn "Removing old templates for: ~A~{ ~A~}" class-name operand-types))
;;;	do (setf have-warned t)
	do (loop for i from 0 to #xff
	       do (setf (aref *template-table* i)
		    (delete dead-template (aref *template-table* i)))))
    (setf (gethash class-name *template-by-class-name*) nil)))

(defun template-filter (cdatum prefixes template-list)
  "Given a op-code byte, a list of active prefixes and a list of
templates, return the subset of the templates that are still eligible matches."
  (mapcan #'(lambda (template)
 	      (when (template-match-datum-and-prefixes template
						       (realpart cdatum)
						       (imagpart cdatum)
						       prefixes)
		(list template)))
 	  template-list))

(unless +opcode-prefix-map+
  (warn "+opcode-prefix-map+ is empty."))
(mapcar #'(lambda (pp)
	    (templates-remember nil (list  (make-instance 'prefix-template
					    'value (car pp)))))
	+opcode-prefix-map+)



(defun template-instr-and-prefix-length (template instr env)
  (+ (template-instr-numo template)
     (length (calculate-prefixes instr template))
     (length (compute-instruction-extra-prefixes instr template env))))


;;; ----------------------------------------------------------------
;;;                    Instruction decode
;;; ----------------------------------------------------------------

(defgeneric instruction-decode (instr template)
  (:documentation
   "Decode <instr> according to its datum."))

(defmethod instruction-decode ((instr instruction) template)
  (when (slot-boundp instr 'original-datum)
    (let ((instr-symbolic (decode-instr-symbolic template
						 (realpart (instruction-original-datum instr)))))
      (setf (instruction-operands instr)
	(loop for op-class in (template-instr-operand-classes template)
	    collecting
	      (operand-decode (make-instance op-class)
			      nil
			      instr-symbolic)))))
  instr)

(defun make-decode-instruction (datum prefixes template)
  (instruction-decode (make-instance (template-instr-classname template)
			:datum datum
			:prefixes (set-difference prefixes
						  (template-req-prefixes template)))
		      template))

(defun calculate-prefixes (instr template)
  (remove-duplicates
   (append (if (or (eq :any-mode (template-operand-mode template))
		   (eq *cpu-mode* (template-operand-mode template)))
	       nil
	     '(16-bit-operand))
	   (if (or (eq :any-mode (template-addressing-mode template))
		   (eq *cpu-mode* (template-addressing-mode template)))
	       nil
	     '(16-bit-address))
	   (template-req-prefixes template)
	   (set-difference (instruction-prefixes instr)
			   (template-not-prefixes template)))))

(defun prefix-encode (cdatum prefix-list &optional extra-prefixes)
  "Given an instruction encoded into <cdatum> by <template>,
append the necessary prefix-bytes to cdatum."
  (let ((new-byte (realpart cdatum))
	(byte-pos (imagpart cdatum)))
    (loop for prefix in prefix-list
	  do (setf (ldb (byte 8 (* 8 byte-pos))
			new-byte)
	       (decode-set +prefix-opcode-map+
			   prefix))
	     (incf byte-pos))
    (loop for prefix in extra-prefixes
	  do (setf (ldb (byte 8 (* 8 byte-pos))
			new-byte)
	       prefix)
	     (incf byte-pos))
    (complex new-byte byte-pos)))

(defun make-instr-symbolic-from-template (template)
  (check-type template template)
  (let ((instr-symbolic (make-instr-symbolic
			 :opcode (ldb (byte (* 8 (template-instr-opcode-numo template))
					    (* 8 (- (template-numo template)
						    (template-instr-opcode-numo template))))
				      (template-value template)))))
    (when (and (template-instr-modr/m-p template)
	       (> (template-numo template)
		  (template-instr-opcode-numo template)))
      ;; initialize modr/m from template
      (let ((modrm-octet (ldb (byte 8 (* 8 (- (template-numo template)
					      (template-instr-opcode-numo template)
					      1)))
			      (template-value template))))
	(with-slots (mod reg r/m)
	    instr-symbolic
	  (setf mod (ldb (byte 2 6) modrm-octet)
		reg (ldb (byte 3 3) modrm-octet)
		r/m (ldb (byte 3 0) modrm-octet)))))
    instr-symbolic))

(defstruct assemble-env
  symtab
  current-pc)

(defstruct teo
  template
  encoding-list
  resolved-operand-list)

(defun template-unify-operands (template instr operand-list env)
  "Given a template and a list of operands, determine if the operands
can be encoded in this template. If not, return NIL. Otherwise, return
a teo for encoding later."
  (make-teo
   :resolved-operand-list (loop
			      for operand in operand-list
			      and top-class in (template-instr-operand-classes template)
			      and top-type in (template-instr-operand-types template)
			      if (operand-and-encoding-unify operand
							     (operand-class-encoding top-class)
							     top-type
							     template
							     instr
							     env)
			      collect it
			      else
			      do (return-from template-unify-operands nil)) ; no unify
   :encoding-list (mapcar #'operand-class-encoding
			  (template-instr-operand-classes template))
   :template template))

(defvar *instruction-compute-extra-prefix-map* nil)

(defun instruction-encode-from-teo (instr teo env)
  (check-type instr instruction)
  (let ((template (teo-template teo))
	(resolved-operand-list (teo-resolved-operand-list teo))
	(operand-encoding-list (teo-encoding-list teo)))
    (let ((is (make-instr-symbolic-from-template template)))
      (loop for operand in resolved-operand-list
	  and operand-encoding in operand-encoding-list
	  and operand-type in (template-instr-operand-types template)
	  do (operand-encode operand operand-encoding operand-type is))
      (prefix-encode (encode-instr-symbolic template is)
		     (calculate-prefixes instr template)
		     (compute-instruction-extra-prefixes instr template env)))))

(defun compute-instruction-extra-prefixes (instr template env)
  (funcall (or (instruction-finalizer instr)
	       (cdr (assoc (class-name (class-of instr)) *instruction-compute-extra-prefix-map*
			   :test #'string=))
	       (constantly nil))
	   instr env (+ (template-instr-numo template)
			(length (calculate-prefixes instr template)))))

(defun template-match-by-cpu-mode (template cpu-mode)
  (or (eq :any-mode (template-cpu-mode template))
      (eq cpu-mode (template-cpu-mode template))))

(defun instruction-encode-to-teo (instr &optional env
						  (operand-list
						   (instruction-operands instr)))
  (let ((templates (templates-lookup-by-class-name (type-of instr))))
    (when (null templates)
      (error "No templates for instruction ~A." instr))
    (or (loop for template in templates
	    when (and (template-match-by-cpu-mode template *cpu-mode*)
		      (template-match-by-operand-classes template operand-list)
		      (template-unify-operands template instr operand-list env))
	    collect it)
	(error "Can't unify operands for ~A ~A." instr
	       (mapcar #'type-of operand-list)))))

(defun pairwise-teopt (teo-list instr optimize-teo-fn)
  (loop
      with chosen-teo = (first teo-list)
      for teo in (rest teo-list)
      do (when (funcall optimize-teo-fn teo chosen-teo instr)
	   (setf chosen-teo teo))
      finally (return chosen-teo)))

(defun optimize-teo-smallest (teo-list instr env)
  "Prefer the smallest (as in fewest octets) encodings."
  (declare (ignore env))
  (pairwise-teopt teo-list
		  instr
		  #'(lambda (teo1 teo2 instr)
		      (< (+ (template-instr-numo (teo-template teo1))
			    (length (calculate-prefixes instr (teo-template teo1))))
			 (+ (template-instr-numo (teo-template teo2))
			    (length (calculate-prefixes instr (teo-template teo2))))))))

(defun template-is-16-bit-p (template)
  (or (eq :16-bit (template-addressing-mode template))
      (eq :16-bit (template-operand-mode template))
      (member '16-bit-operand (template-req-prefixes template))
      (member '16-bit-address (template-req-prefixes template))))

(defun optimize-teo-smallest-no16 (teo-list instr env)
  "Prefer the smallest 32-bit encoding."
  (declare (ignore env))
  (pairwise-teopt teo-list
		  instr
		  #'(lambda (teo1 teo2 instr)
		      (let ((t1 (teo-template teo1))
			    (t2 (teo-template teo2)))
			(or (and (not (template-is-16-bit-p t1))
				 (template-is-16-bit-p t2))
			    (< (+ (template-instr-numo t1)
				  (length (calculate-prefixes instr t1)))
			       (+ (template-instr-numo t2)
				  (length (calculate-prefixes instr t2)))))))))

(defun optimize-teo-original-size (teo-list instr env)
  "Find an encoding that matches the size of the instruction's
original size (its instruction-original-datum)."
  (let ((original-size (imagpart (instruction-original-datum instr))))
    (find-if #'(lambda (teo)
		 (= original-size
		    (template-instr-and-prefix-length (teo-template teo) instr env)))
	     teo-list)))

(defun optimize-teo-user-size (teo-list instr env)
  "Find an encoding that matches the user-specified size."
  (find-if #'(lambda (teo)
	       (= (instruction-user-size instr)
		  (template-instr-and-prefix-length (teo-template teo) instr env)))
	   teo-list))

(defun instruction-encode (instr env &optional (optimize-teo-fn #'optimize-teo-smallest))
  (let ((teo-list (instruction-encode-to-teo instr env)))
    (if (null teo-list)
	(error "Unable to encode ~A." instr)
      (let ((teo (if (instruction-user-size instr)
		     (optimize-teo-user-size teo-list instr env)
		   (funcall optimize-teo-fn teo-list instr env))))
	(if (not (teo-p teo))
	    (error "Optimization with ~S of instruction ~S failed for teo-list ~S"
		   optimize-teo-fn instr teo-list)
	  (instruction-encode-from-teo instr teo env))))))

;;;

(defun change-endian (old-byte numo)
  "Returns the alternative endian version of a byte."
  (let ((result 0))
    (dotimes (i numo)
      (setf (ldb (byte 8 (* 8 (- numo i 1))) result)
	(ldb (byte 8 (* 8 i)) old-byte)))
    result))

(defun sign-extend-complex (cdatum)
  "Given a two's complement signed byte (where the most significant
byte represents the sign), return the natural representation of
that byte (i.e. #c(255 1) => -1)."
  (let ((old-byte (realpart cdatum))
	(numo (imagpart cdatum)))
    (cond
      ((= 0 numo)
       0)
      ((zerop (ldb (byte 1 (1- (* 8 numo))) old-byte))
       cdatum)
      (t (complex (- old-byte (dpb 1 (byte 1 (* 8 numo)) 0))
                  numo)))))

(defun sign-extend (old-byte numo)
  "Given a two's complement signed byte (where the most significant
byte represents the sign), return the natural representation of
that byte (i.e. (255 1) => -1)."
  (if (zerop (ldb (byte 1 (1- (* 8 numo))) old-byte))
      old-byte
    (- old-byte (dpb 1 (byte 1 (* 8 numo)) 0))))


(defun decode-instr-symbolic (template datum)
  "Given an instruction-template (a description of an instructions layout),
decode a datum into a symbolic instr-symbolic object."
  (let ((is (make-instr-symbolic))
	(byte-pos 0))
    ;; immediate
    (setf (instr-symbolic-immediate is)
      (complex (change-endian (ldb (byte (* 8 (template-instr-immediate-numo template))
					 (* 8 byte-pos))
				   datum)
			      (template-instr-immediate-numo template))
	       (template-instr-immediate-numo template)))
    (incf byte-pos (template-instr-immediate-numo template))
    ;; displacement
    (setf (instr-symbolic-displacement is)
      (sign-extend-complex
       (complex (change-endian (ldb (byte (* 8 (template-instr-displacement-numo template)) 
					  (* 8 byte-pos))
				    datum)
			       (template-instr-displacement-numo template))
		(template-instr-displacement-numo template))))
    (incf byte-pos (template-instr-displacement-numo template))
    ;; SIB
    (when (template-instr-sib-p template)
      (let ((sib (ldb (byte 8 (* 8 byte-pos)) datum)))
	(setf (instr-symbolic-scale is) (ldb (byte 2 6) sib)
	      (instr-symbolic-index is) (ldb (byte 3 3) sib)
	      (instr-symbolic-base  is) (ldb (byte 3 0) sib))
	(incf byte-pos)))
    ;; ModR/M
    (when (template-instr-modr/m-p template)
      (let ((modr/m (ldb (byte 8 (* 8 byte-pos)) datum)))
	(setf (instr-symbolic-mod is) (ldb (byte 2 6) modr/m)
	      (instr-symbolic-reg is) (ldb (byte 3 3) modr/m)
	      (instr-symbolic-r/m is) (ldb (byte 3 0) modr/m))
	(incf byte-pos)))
    ;; Opcode
    (setf (instr-symbolic-opcode is) (ldb (byte (* 8 (template-instr-opcode-numo template))
						(* 8 byte-pos))
					  datum))
;;;    (assert  (= (realpart datum)
;;;		(realpart (encode-instr-symbolic template is)))
;;;	(datum)
;;;      (error "~&instr-symbolic codec inconsistency: ~A / ~A / ~A / ~A~%"
;;;	     datum
;;;	     (realpart (encode-instr-symbolic template is))
;;;	     is template))
    (values is)))


(defun encode-instr-symbolic (template is)
  (let ((byte-datum 0)
	(byte-pos 0))
    ;; immediate
    (unless (zerop (template-instr-immediate-numo template))
      (setf (ldb (byte (* 8 (template-instr-immediate-numo template))
		       (* 8 byte-pos))
		 byte-datum)
	(change-endian (realpart (instr-symbolic-immediate is))
		       (template-instr-immediate-numo template)))
      (incf byte-pos (template-instr-immediate-numo template)))
    ;; displacement
    (unless (zerop (template-instr-displacement-numo template))
      (setf (ldb (byte (* 8 (template-instr-displacement-numo template))
		       (* 8 byte-pos))
		 byte-datum)
	(change-endian (realpart (instr-symbolic-displacement is))
		       (template-instr-displacement-numo template)))
      (incf byte-pos (template-instr-displacement-numo template)))
    ;; SIB
    (when (template-instr-sib-p template)
      (let ((sib 0))
	(setf (ldb (byte 2 6) sib) (instr-symbolic-scale is)
	      (ldb (byte 3 3) sib) (instr-symbolic-index is)
	      (ldb (byte 3 0) sib) (instr-symbolic-base  is))
	(setf (ldb (byte 8 (* 8 byte-pos)) byte-datum)
	  sib)
	(incf byte-pos 1)))
    ;; ModR/M
    (when (template-instr-modr/m-p template)
      (let ((modr/m 0))
	(setf (ldb (byte 2 6) modr/m) (instr-symbolic-mod is)
	      (ldb (byte 3 3) modr/m) (instr-symbolic-reg is)
	      (ldb (byte 3 0) modr/m) (instr-symbolic-r/m is))
	(setf (ldb (byte 8 (* 8 byte-pos)) byte-datum)
	  modr/m)
	(incf byte-pos 1)))
    ;; Opcode
    (setf (ldb (byte (* 8 (template-instr-opcode-numo template))
		     (* 8 byte-pos))
	       byte-datum)
      (instr-symbolic-opcode is))
    (incf byte-pos (template-instr-opcode-numo template))
    (values (complex byte-datum byte-pos))))   

;;; ----------------------------------------------------------------
;;;                  Disassemble functions
;;; ----------------------------------------------------------------

(defun decode-by-template (read-octet-fn cdatum prefixes template)
  "Given the current datum and list of prefixes and the template which
identifies the type of instruction, finish the job by reading the remaining
octets and creating the instruction (or prefix) object."
  (ecase (type-of template)
    ((instr-template)
     ;; read remaining octets
     (dotimes (i (- (template-instr-numo template)
		    (imagpart cdatum)))
       (let ((fill-octet (funcall read-octet-fn)))
	 (unless fill-octet
	   (error "EOF in middle of an instruction (~W)." cdatum))
	 (setf cdatum (complex (logior (ash (realpart cdatum) 8)
				       fill-octet)
			       (1+ (imagpart cdatum))))))
     (assert (template-match-datum-and-prefixes template
						(ldb (byte (* 8 (template-numo template))
							   (* 8 (- (imagpart cdatum)
								   (template-numo template))))
						     (realpart cdatum))
						(template-numo template)
						prefixes)
	 (cdatum template)
       "Decoding instruction, but ~A-template ~A doesn't match full datum ~A."
       (template-instr-classname template)
       (complex (template-value template)
		(template-numo template))
       cdatum)
     ;; decode
     (make-decode-instruction cdatum prefixes template))
    ((prefix-template)
     (multiple-value-bind (templates cdatum)
	 (template-lookup-next read-octet-fn)
       (decode-filter read-octet-fn
		      cdatum
		      (cons (prefix-symbol (template-value template))
			    prefixes)
		      templates)))))
  
(defun decode-filter (read-octet-fn cdatum prefixes templates)
  "Given an octet-reading function, the currently read datum, the list
of current prefixes and a list of eligible templates, attempt to
decode the instruction (or prefix) and return it.
 
This function works by reading the next octet, filter out which templates
are still eligible and recurse over the reduced list of templates. When
there is just one template left, we are home free."
  (let ((filtered-templates (template-filter cdatum
					     prefixes
					     templates)))
    (cond
     ((= 0 (length filtered-templates))	; no match
      (when *error-on-unknown-instruction*
	(error "No matching instruction for datum ~A." cdatum)))
     ((= 1 (length filtered-templates))	; found single match
      (decode-by-template read-octet-fn
			  cdatum
			  prefixes
			  (first filtered-templates)))
     ((< (imagpart cdatum)		; must read more octets
	 (apply #'min (mapcar #'template-numo
			      filtered-templates)))
      (let ((new-octet (funcall read-octet-fn)))
	(if (not new-octet)
	    (when *error-on-eof*
	      (error "EOF in instruction"))
	  (let ((new-cdatum (complex (dpb new-octet
					  (byte 8 0)
					  (ash (realpart cdatum) 8))
				     (1+ (imagpart cdatum)))))
	    (decode-filter read-octet-fn
			   new-cdatum
			   prefixes
			   filtered-templates)))))
     (t					; many matches, try to prioritize
      (let ((prioritized-templates (sort filtered-templates
					 #'>
					 :key #'template-priority)))
	(assert (> (template-priority (first prioritized-templates))
		   (template-priority (second prioritized-templates)))
	    (cdatum)
	  "Inconsistent template set for datum ~A and prefixes ~A: [~A] v:~A/m:~A"
	  cdatum
	  prefixes
	  (mapcar #'(lambda (tt)
		      (etypecase tt
			(instr-template (template-instr-classname tt))
			(prefix-template 'prefix)))
		  filtered-templates)
	  (mapcar #'template-value
		  filtered-templates)
	  (mapcar #'template-mask
		  filtered-templates))
	;; We found a match by prioritization.
	(decode-by-template read-octet-fn
			    cdatum
			    prefixes
			    (first prioritized-templates)))))))
	

(defun decode-read-octet (read-octet-fn)
  "Disassemble the octets as returned by the (repeated) calling of
the READ-OCTET-FN funarg. The READ-OCTET-FN represents a stream of
octets which ends when the function returns nil."
  (multiple-value-bind (templates cdatum)
      (template-lookup-next read-octet-fn)
    (decode-filter read-octet-fn
		   cdatum
		   nil			; prefixes
		   templates)))
		 
(defun decode-datum (datum datum-numo)
  "Disassemble a datum (i.e. a byte)."
  (decode-read-octet #'(lambda ()	; return consecutive octets of datum
			 (when (plusp datum-numo)
			   (decf datum-numo)
			   (ldb (byte 8 (* 8 datum-numo)) datum)))))

(defun decode-octet-list (&rest octet-list)
  (let ((ol (copy-list octet-list)))
    (decode-read-octet #'(lambda ()
			   (pop ol)))))

(defun decode (&rest octet-list)
  (loop while octet-list
      collect (decode-read-octet #'(lambda ()
				     (pop octet-list)))))

(defun gg (&rest octet-list)
  (let ((i (apply #'ff octet-list)))
    (cons (type-of i)
	  (mapcar #'operand-listform
		  (instruction-operands i)))))

(defun decode-sub-stream (stream length)
  "Disassemble the contents of <stream> from its current position and
<length> octets on."
  (loop
      for counter from 0 by 1
      with loop-length = length
      while (plusp loop-length)
      collecting (handler-bind
		     ((error #'(lambda (condition)
				 (declare (ignore condition))
				 (format t "~&;; file-position: ~A, icnt: ~A~%"
					 (file-position stream)
					 counter))))
		   (decode-read-octet #'(lambda ()
					  (when (plusp length)
					    (decf loop-length)
					    (read-byte stream)))))))
