;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 20012000, 2002, 2004,
;;;;    Department of Computer Science, University of Tromso, Norway
;;;; 
;;;; Filename:      proglist.lisp
;;;; Description:   
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Mon May 15 13:43:55 2000
;;;; Distribution:  See the accompanying file COPYING.
;;;;                
;;;; $Id: proglist.lisp,v 1.6 2004/10/12 09:37:24 ffjeld Exp $
;;;;                
;;;;------------------------------------------------------------------

(in-package #:ia-x86)

(defvar *label-counter* 0)
(defun make-label ()
  (intern (format nil "~A-~D"
		  'label
		  (incf *label-counter*))))

(defun labelize-proglist (proglist &optional (start-address 0))
  "Destructively modifies the instruction-objects in proglist."
  (let ((end-address (+ start-address
			(loop for i in proglist
			    summing (imagpart (instruction-original-datum i)))))
	(label-hash (make-hash-table :test #'eql :size 19)))
    (loop for i in proglist
	with pc = start-address
	do (incf pc (imagpart (instruction-original-datum i)))
	do (loop for operands on (instruction-operands i)
	       when (typep (car operands) 'operand-rel-pointer)
	       do (let ((address (+ pc (slot-value (car operands) 'offset))))
		    (if (<= start-address address end-address)
			(let ((label (or (gethash address label-hash)
					 (setf (gethash address label-hash)
					   (make-label)))))
			  (setf (car operands)
			    (make-instance 'operand-label 'label label)))
		      (setf (car operands)
			(make-instance 'operand-number 'number address))))))
    (loop for i in proglist
	with pc = start-address
	when (gethash pc label-hash)
	collect (gethash pc label-hash)
	collect i
	do (incf pc (imagpart (instruction-original-datum i))))))

(defclass forward-reference ()
  ((labels           :initarg labels :reader forward-reference-labels)
   (referring-pc     :initarg referring-pc)
   (placeholder-cons :initarg placeholder-cons)
   (assumed-length   :initarg assumed-length
		     :accessor forward-reference-assumed-length)))

(defclass forward-reference-instruction (forward-reference)
  ((instruction :initarg instruction)))

(defclass forward-reference-inline-data (forward-reference)
  ((inline-data :initarg inline-data)))

(defmethod print-object ((obj forward-reference) stream)
  (format stream "<unresolved: ~A>" (forward-reference-labels obj)))

(define-condition assumption-failed ()
  ((forward-reference :initarg forward-reference
		      :reader assumption-failed-forward-reference)
   (actual-symtab :initarg actual-symtab
		  :reader assumption-failed-actual-symtab)
   (actual-length :initarg actual-length
		  :reader assumption-failed-actual-length))
  (:report
   (lambda (condition stream)
     (with-slots (forward-reference actual-symtab assumed-length actual-length)
	 condition
       (with-slots (referring-pc assumed-length)
	   forward-reference
	 (format stream
		 "Assumption failed when ~A implied length ~A while ~A was assumed."
		 actual-symtab actual-length assumed-length))))))

(defun try-resolve-forward-reference (fwd-to-resolve env optimize-teo)
  (if (notevery #'(lambda (label)
		    (symtab-try-lookup-label (assemble-env-symtab env) label))
		(forward-reference-labels fwd-to-resolve))
      nil				; this label doesn't (completely) resolve this fwd.
    (prog1 t (resolve-forward-reference fwd-to-resolve env optimize-teo))))

(defmethod resolve-forward-reference ((fwd-to-resolve forward-reference-instruction) env optimize-teo)
  (with-slots (labels instruction referring-pc
		      placeholder-cons assumed-length)
      fwd-to-resolve
;;;    (warn "Resolving at ~D from ~D: ~S" referring-pc (assemble-env-current-pc env)
;;;	  (mapcar #'(lambda (l)
;;;		      (cons l (format nil "~D" (symtab-lookup-label (assemble-env-symtab env) l))))
;;;		  labels))
    (let ((cdatum (instruction-encode instruction
				      (make-assemble-env :symtab (assemble-env-symtab env)
							 :current-pc referring-pc)
				      optimize-teo)))
      (when (< (imagpart cdatum) assumed-length)
	(setf cdatum
	  (instruction-encode instruction
			      (make-assemble-env :symtab (assemble-env-symtab env)
						 :current-pc referring-pc)
			      #'(lambda (teo-list instr env)
				  (or (find-if #'(lambda (teo)
					       (= assumed-length
						  (template-instr-and-prefix-length
						   (teo-template teo)
						   instr env)))
					       teo-list)
				      (error "Unable to find encoding matching size ~D for ~S"
					     assumed-length instr))))))
      (unless (= (imagpart cdatum) assumed-length)
	(error 'assumption-failed
	       'forward-reference fwd-to-resolve
	       'actual-symtab (mapcar #'(lambda (label)
					  (cons label
						(symtab-lookup-label (assemble-env-symtab env)
								     label)))
				      labels)
	       'actual-length (imagpart cdatum)))
      (setf (car placeholder-cons) cdatum))))

(defmethod resolve-forward-reference ((fwd-to-resolve forward-reference-inline-data) env optimize-teo)
  (declare (ignore optimize-teo))
  (with-slots (labels inline-data referring-pc placeholder-cons assumed-length)
      fwd-to-resolve
    (let* ((cdatums (reverse (inline-data-encode inline-data env)))
	   (total-length (loop for cd in cdatums summing (imagpart cd))))
      (unless (= total-length assumed-length)
	(error 'assumption-failed
	       'forward-reference fwd-to-resolve
	       'actual-symtab (mapcar #'(lambda (label)
					  (cons label
						(symtab-lookup-label (assemble-env-symtab env) label)))
				      labels)
	       'actual-length total-length))
      ;; splice in the cdatums list
      (setf (car placeholder-cons) (car cdatums)
	    (cdr (last cdatums)) (cdr placeholder-cons)
	    (cdr placeholder-cons) (cdr cdatums)))))


(defun guess-next-instruction-length (expr missing-labels program-rest env)
  (declare (special *proglist-minimum-expr-size*))
  ;; (let ((minimum-size (max previous-length (gethash expr *proglist-minimum-expr-size*))))
  (or (instruction-user-size expr)
      (car (or (gethash expr *proglist-minimum-expr-size*)
	       (setf (gethash expr *proglist-minimum-expr-size*)
		 (typecase expr
		   ((or ia-x86-instr::jmp #+ignore ia-x86-instr::jcc)
		    ;; educated guess for jumps..
		    (assert (= 1 (length missing-labels)))
		    (let ((instruction-offset (position (first missing-labels) program-rest)))
		      (assert instruction-offset ()
			"Can't find label ~S for instruction ~S." (first missing-labels) expr)
		      (etypecase expr
			(ia-x86-instr::jmp
			 (if (>= instruction-offset 60)
			     '(5 2 4)
			   '(2 5 4)))
			#+ignore (ia-x86-instr::jcc
				  (if (>= instruction-offset 50)
				      (if (eq *cpu-mode* :32-bit) '(6 2 4 5) '(4 2 6 5))
				    (if (eq *cpu-mode* :32-bit) '(2 6 4) '(2 4 6)))))))
		   (t (loop with guesses = nil
			  for template in (templates-lookup-by-class-name (type-of expr))
			  when (template-match-by-operand-classes template (instruction-operands expr))
			  do (let ((l (template-instr-and-prefix-length template expr env)))
			       (unless (member l guesses)
				 (setf guesses
				   (merge 'list guesses (list l) #'<))))
			  finally (return guesses)))))))))

(defun proglist-encode-internal (prog-list env forward-references encoded-proglist-reverse
				 optimize-teo)
  
  (declare (special *proglist-minimum-expr-size*))
  (loop for expr-rest on prog-list
      do (let ((expr (first expr-rest)))
	   (etypecase expr
	     ((or SYMBOL integer)
	      (setf (assemble-env-symtab env)
		(symtab-def-label (assemble-env-symtab env)
				  expr
				  (assemble-env-current-pc env)))
	      (loop for fwd in forward-references
		  when (try-resolve-forward-reference fwd env optimize-teo)
		  collect fwd into resolved-forwards
		  finally (setf forward-references
			    (set-difference forward-references resolved-forwards))))
	     (ALIGNMENT
	      (loop for cbyte in (create-alignment expr (assemble-env-current-pc env))
		  do (push cbyte encoded-proglist-reverse)
		  do (incf (assemble-env-current-pc env)
			   (imagpart cbyte))))
	     (INLINE-DATA
	      ;; this follows pretty much the same structure as the INSTRUCTION case.
	      (handler-case
		  (loop for cbyte in (inline-data-encode expr env)
		      do (push cbyte encoded-proglist-reverse)
		      do (incf (assemble-env-current-pc env)
			       (imagpart cbyte)))
		(unresolved-labels (ul-condition)
		  (push 'unresolved-inline-data-forward-reference encoded-proglist-reverse)
		  (let* ((assumed-length (inline-data-guess-sizeof expr env))
			 (fwd (make-instance 'forward-reference-inline-data
				'labels (unresolved-labels-labels ul-condition)
				'inline-data expr
				'referring-pc (assemble-env-current-pc env)
				'assumed-length assumed-length
				'placeholder-cons encoded-proglist-reverse)))
		    (loop
		      (handler-case
			  (return-from proglist-encode-internal
			    (proglist-encode-internal (rest expr-rest)
						      (make-assemble-env
						       :symtab (symtab-add-frame (assemble-env-symtab env))
						       :current-pc (+ (assemble-env-current-pc env)
								      (forward-reference-assumed-length fwd)))
						      (cons fwd forward-references)
						      encoded-proglist-reverse
						      optimize-teo))
			(assumption-failed (af-condition)
			  (unless (eq fwd (assumption-failed-forward-reference af-condition))
			    (error af-condition)) ; decline
			  ;; (warn af-condition)
			  (setf (forward-reference-assumed-length fwd)
			    (assumption-failed-actual-length af-condition))
			  ;; The assumption failed, we'll retry with the length found at the
			  ;; time the label was actually resolved.
			  (values))))))))
	     (INSTRUCTION
	      (handler-case
		  (let ((datum (instruction-encode expr env optimize-teo)))
		    (push datum encoded-proglist-reverse)
		    (incf (assemble-env-current-pc env)
			  (imagpart datum)))
		(unresolved-labels (ul-condition)
		  ;; we stumbled upon a label-reference that wasn't immediately
		  ;; resolvable. We assume it's a forward reference.
		  ;; First, reserve a cons-cell for this instruction to be encoded later
		  (push 'unresolved-forward-reference encoded-proglist-reverse)
		  ;; Now loop over the possible octet-lengths for this instruction.
		  ;; For each iteration, assume that octet-length, and if no exception is
		  ;; raised, the assumption holds and we break out of the loop with
		  ;; the RETURN-FROM form.
		  (loop for assumed-instr-length =
			(guess-next-instruction-length expr
						       (unresolved-labels-labels ul-condition)
						       (rest expr-rest)
						       env)
		      do
			#+ignore (warn "Trying for ~A at ~D with ~A octets.."
				       expr (assemble-env-current-pc env) assumed-instr-length)
			(let ((fwd (make-instance 'forward-reference-instruction
				     'labels (unresolved-labels-labels ul-condition)
				     'instruction expr
				     'referring-pc (assemble-env-current-pc env)
				     'assumed-length assumed-instr-length
				     'placeholder-cons encoded-proglist-reverse)))
			  (handler-case
			      ;; if assumption holds, break out of the loop
			      (return-from proglist-encode-internal
				(proglist-encode-internal ; attempt to continue by recursion
				 (rest expr-rest)
				 (make-assemble-env
				  :symtab (symtab-add-frame (assemble-env-symtab env))
				  :current-pc (+ (assemble-env-current-pc env)
						 assumed-instr-length))
				 (cons fwd forward-references)
				 encoded-proglist-reverse
				 optimize-teo))
			    (assumption-failed (af-condition)
			      (unless (eq fwd (assumption-failed-forward-reference af-condition))
				(error af-condition)) ; decline
			      ;; (warn "~A" af-condition)
			      ;; pop this length off the list of instr-length guesses
			      (assert (gethash expr *proglist-minimum-expr-size*) (expr)
				"Unable to encode ~A. Is the label too far away?" expr)
			      (pop (gethash expr *proglist-minimum-expr-size*))
			      ;; the assumption failed.
			      ;; now continue the dolist loop
			      (values)))))
		  (error "Unable to encode ~A. Is the label too far away? ~
                          [Should _really_ never get here!!]"
			 expr)))))))
  ;; When we get here, the whole proglist is encoded.
  (unless (null forward-references)
    (error "There were unresolved forward references: ~A"
	   (mapcar #'forward-reference-labels
		   forward-references)))
  ;; success, return.
  (values (nreverse encoded-proglist-reverse)
	  (assemble-env-symtab env)))


(defun cbyte-to-octet-list (cbyte)
  (loop
      with value = (realpart cbyte)
      for i from (1- (imagpart cbyte)) downto 0
      collect (ldb (byte 8 (* 8 i)) value)))
		   
(defun proglist-encode (result-type cpu-mode start-addr prog-list
			&key (optimize-teo #'optimize-teo-smallest-no16)
			     (symtab-lookup nil))
  (let ((*cpu-mode* cpu-mode)
	(*symtab-lookup* symtab-lookup)
	(*proglist-minimum-expr-size* (make-hash-table :test #'eq)))
    (declare (special *symtab-lookup* *proglist-minimum-expr-size*))
    (multiple-value-bind (encoded-proglist symtab)
	(proglist-encode-internal prog-list
				  (make-assemble-env :current-pc start-addr
						     :symtab (make-symtab))
				  nil nil optimize-teo)
      (ecase result-type
	((:cbytes)
	 (values encoded-proglist symtab))
	((:octet-list)
	 (values (mapcan #'cbyte-to-octet-list encoded-proglist)
		 symtab))
	((:octet-vector)
	 (let* ((ep-size (loop for cbyte in encoded-proglist
			     summing (imagpart cbyte)))
		(ep-vector (make-array ep-size
				       :element-type '(unsigned-byte 8)
				       :fill-pointer t)))
	   (loop
	       for cbyte in encoded-proglist
	       with i = 0
	       do (loop for bp from (1- (imagpart cbyte)) downto 0
		      do (setf (aref ep-vector i)
			   (ldb (byte 8 (* 8 bp)) (realpart cbyte)))
		      do (incf i))
	       finally (return (values ep-vector symtab)))))))))
  
		
(defun print-encoded-proglist (epl &optional (base-addr 0))
  (loop for cbyte in epl
      and counter from 0 by 1
      with pc = base-addr
      do (format t "~8,'0X: ~22<~{ ~2,'0X~}~;~> ~4D ~A~%"
		 pc
		 (cbyte-to-octet-list cbyte)
		 counter
		 (apply #'decode-octet-list (cbyte-to-octet-list cbyte)))
      do (incf pc (imagpart cbyte))))

(defun octet-list-to-bioctets (octet-list)
  (loop for oc on octet-list by #'cddr
      collect (let ((msb (first oc))
		    (lsb (or (second oc) 0)))
		(dpb lsb (byte 8 8) msb))))
