;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 2000, 2004,
;;;;    Department of Computer Science, University of Tromso, Norway
;;;; 
;;;; Filename:      instr-mov.lisp
;;;; Description:   MOV instructions
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Thu Jan 27 14:43:05 2000
;;;; Distribution:  See the accompanying file COPYING.
;;;;                
;;;; $Id: instr-mov.lisp,v 1.2 2004/01/16 11:54:14 ffjeld Exp $
;;;;                
;;;;------------------------------------------------------------------

(in-package "IA-X86-INSTR")

;;; ----------------------------------------------------------------
;;;                         MOV classes
;;; ----------------------------------------------------------------


;;; ----------------------------------------------------------------
;;;                 MOV instructions [IISR 11-281]
;;; ----------------------------------------------------------------

(def-instr mov (instruction))
(def-instr movb (mov)
  (:+ #xb0 1 (imm8 +r8))
  (:r #x88 (r8 r/m8))
  (:r #x8a (r/m8 r8))
  (:moffs #xa0 (moffs8 al))
  (:moffs #xa2 (al moffs8))
  (:digit (#xc6 0) 1 (imm8 r/m8)))

(def-instr movw (mov)
  (:r #x89 (r16 r/m16) :operand-mode :16-bit)
  (:r #x8b (r/m16 r16) :operand-mode :16-bit)
  (:r #x8c (sreg r/m16))
  (:r #x8e (r/m16 sreg))
  (:+ #xb8 2 (imm16 +r16) :operand-mode :16-bit)
  (:digit (#xc7 0) 2 (imm16 r/m16) :operand-mode :16-bit)
  (:moffs #xa1 (moffs16 ax))
  (:moffs #xa3 (ax moffs16)))


(def-instr movl (mov)
  (:r #x89 (r32 r/m32) :operand-mode :32-bit)
  (:r #x8b (r/m32 r32) :operand-mode :32-bit)
  (:+ #xb8 4 (imm32 +r32) :operand-mode :32-bit)
  (:digit (#xc7 0) 4 (imm32 r/m32) :operand-mode :32-bit)
  (:moffs #xa1 (moffs32 eax))
  (:moffs #xa3 (eax moffs32)))

;;; ----------------------------------------------------------------
;;;                 MOVSX instructions [IISR 11-291]
;;; ----------------------------------------------------------------

(def-instr movsx (instruction))
(def-instr movsxb (movsx)
  (:r #c(#x0fbe 2) (r/m8 r16) :operand-mode :16-bit)
  (:r #c(#x0fbe 2) (r/m8 r32) :operand-mode :32-bit))
(def-instr movsxw (movsx)
  (:r #c(#x0fbf 2) (r/m16 r32)))

;;; MOV string

(def-instr movs (instruction))
(def-instr movsb (movs) (:simple #xa4))
(def-instr movsw (movs) (:simple #xa5 :operand-mode :16-bit))
(def-instr movsd (movs) (:simple #xa5 :operand-mode :32-bit))

;;; ----------------------------------------------------------------
;;;                 MOVZX instructions [IISR 11-292]
;;; ----------------------------------------------------------------

(def-instr movzx (instruction))
(def-instr movzxb (movzx)
  (:r #c(#x0fb6 2) (r/m8 r16) :operand-mode :16-bit)
  (:r #c(#x0fb6 2) (r/m8 r32) :operand-mode :32-bit))
(def-instr movzxw (movzx)
  (:r #c(#x0fb7 2) (r/m16 r32)))

;;; ----------------------------------------------------------------
;;;                 MOV to/from Control Registers
;;; ----------------------------------------------------------------

(def-instr movcr (instruction)
  (:digit (#x0f22 0) 0 (r/m32 cr0) :direct t)
  (:digit (#x0f22 2) 0 (r/m32 cr2) :direct t)
  (:digit (#x0f22 3) 0 (r/m32 cr3) :direct t)
  (:digit (#x0f22 4) 0 (r/m32 cr4) :direct t)
	 
  (:digit (#x0f20 0) 0 (cr0 r/m32) :direct t)
  (:digit (#x0f20 2) 0 (cr2 r/m32) :direct t)
  (:digit (#x0f20 3) 0 (cr3 r/m32) :direct t)
  (:digit (#x0f20 4) 0 (cr4 r/m32) :direct t))

;;; ----------------------------------------------------------------
;;;                 MOV to/from Debug Registers
;;; ----------------------------------------------------------------

(def-instr movdr (instruction)
  (:digit (#x0f21 0) 0 (r/m32 dr0) :direct t)
  (:digit (#x0f21 1) 0 (r/m32 dr1) :direct t)
  (:digit (#x0f21 2) 0 (r/m32 dr2) :direct t)
  (:digit (#x0f21 3) 0 (r/m32 dr3) :direct t)
  (:digit (#x0f21 4) 0 (r/m32 dr4) :direct t)
  (:digit (#x0f21 5) 0 (r/m32 dr5) :direct t)
  (:digit (#x0f21 6) 0 (r/m32 dr6) :direct t)
  (:digit (#x0f21 7) 0 (r/m32 dr7) :direct t)
  	 
  (:digit (#x0f23 0) 0 (dr0 r/m32) :direct t)
  (:digit (#x0f23 1) 0 (dr1 r/m32) :direct t)
  (:digit (#x0f23 2) 0 (dr2 r/m32) :direct t)
  (:digit (#x0f23 3) 0 (dr3 r/m32) :direct t)
  (:digit (#x0f23 4) 0 (dr4 r/m32) :direct t)
  (:digit (#x0f23 5) 0 (dr5 r/m32) :direct t)
  (:digit (#x0f23 6) 0 (dr6 r/m32) :direct t)
  (:digit (#x0f23 7) 0 (dr7 r/m32) :direct t))

;;; ----------------------------------------------------------------
;;;                 CMOVcc  --  Conditional Move
;;; ----------------------------------------------------------------

(def-instr cmova (instruction)		; if above
  (:r #x0f47 (r/m16 r16) :operand-mode :16-bit :priority 10)
  (:r #x0f47 (r/m32 r32) :operand-mode :32-bit :priority 10))

(def-instr cmovae (instruction) 		; if above or equal
  (:r #x0f43 (r/m16 r16) :operand-mode :16-bit :priority 10)
  (:r #x0f43 (r/m32 r32) :operand-mode :32-bit :priority 10))

(def-instr cmovb (instruction) 		; if below
  (:r #x0f42 (r/m16 r16) :operand-mode :16-bit :priority 10)
  (:r #x0f42 (r/m32 r32) :operand-mode :32-bit :priority 10))

(def-instr cmovbe (instruction) 		; if below or equal
  (:r #x0f46 (r/m16 r16) :operand-mode :16-bit :priority 10)
  (:r #x0f46 (r/m32 r32) :operand-mode :32-bit :priority 10))

(def-instr cmovc (instruction) 		; if carry
  (:r #x0f42 (r/m16 r16) :operand-mode :16-bit :priority 20)
  (:r #x0f42 (r/m32 r32) :operand-mode :32-bit :priority 20))

(def-instr cmove (instruction)		; if equal
  (:r #x0f44 (r/m16 r16) :operand-mode :16-bit :priority 10)
  (:r #x0f44 (r/m32 r32) :operand-mode :32-bit :priority 10))

(def-instr cmovg (instruction) 		; if greater
  (:r #x0f4f (r/m16 r16) :operand-mode :16-bit :priority 10)
  (:r #x0f4f (r/m32 r32) :operand-mode :32-bit :priority 10))

(def-instr cmovge (instruction) 		; if greater or equal
  (:r #x0f4d (r/m16 r16) :operand-mode :16-bit :priority 10)
  (:r #x0f4d (r/m32 r32) :operand-mode :32-bit :priority 10))

(def-instr cmovl (instruction) 		; if less
  (:r #x0f4c (r/m16 r16) :operand-mode :16-bit :priority 10)
  (:r #x0f4c (r/m32 r32) :operand-mode :32-bit :priority 10))

(def-instr cmovle (instruction) 		; if less or equal
  (:r #x0f4e (r/m16 r16) :operand-mode :16-bit :priority 10)
  (:r #x0f4e (r/m32 r32) :operand-mode :32-bit :priority 10))

(def-instr cmovna (instruction) 		; if not above
  (:r #x0f46 (r/m16 r16) :operand-mode :16-bit :priority 0)
  (:r #x0f46 (r/m32 r32) :operand-mode :32-bit :priority 0))

(def-instr cmovnae (instruction) 		; if not above or equal
  (:r #x0f42 (r/m16 r16) :operand-mode :16-bit :priority 0)
  (:r #x0f42 (r/m32 r32) :operand-mode :32-bit :priority 0))

(def-instr cmovnb (instruction) 		; if not below
  (:r #x0f43 (r/m16 r16) :operand-mode :16-bit :priority 0)
  (:r #x0f43 (r/m32 r32) :operand-mode :32-bit :priority 0))

(def-instr cmovnbe (instruction) 		; if not below or equal
  (:r #x0f47 (r/m16 r16) :operand-mode :16-bit :priority 0)
  (:r #x0f47 (r/m32 r32) :operand-mode :32-bit :priority 0))

(def-instr cmovnc (instruction) 		; if not carry
  (:r #x0f43 (r/m16 r16) :operand-mode :16-bit :priority 20)
  (:r #x0f43 (r/m32 r32) :operand-mode :32-bit :priority 20))

(def-instr cmovne (instruction) 		; if not equal
  (:r #x0f45 (r/m16 r16) :operand-mode :16-bit :priority 10)
  (:r #x0f45 (r/m32 r32) :operand-mode :32-bit :priority 10))

(def-instr cmovng (instruction) 		; if not greater
  (:r #x0f4e (r/m16 r16) :operand-mode :16-bit :priority 0)
  (:r #x0f4e (r/m32 r32) :operand-mode :32-bit :priority 0))

(def-instr cmovnge (instruction) 		; if not greater or equal
  (:r #x0f4c (r/m16 r16) :operand-mode :16-bit :priority 0)
  (:r #x0f4c (r/m32 r32) :operand-mode :32-bit :priority 0))

(def-instr cmovnl (instruction) 		; if not less
  (:r #x0f4d (r/m16 r16) :operand-mode :16-bit :priority 0)
  (:r #x0f4d (r/m32 r32) :operand-mode :32-bit :priority 0))

(def-instr cmovnle (instruction) 		; if not less or equal
  (:r #x0f4f (r/m16 r16) :operand-mode :16-bit :priority 0)
  (:r #x0f4f (r/m32 r32) :operand-mode :32-bit :priority 0))

(def-instr cmovno (instruction) 		; if not overflow
  (:r #x0f41 (r/m16 r16) :operand-mode :16-bit :priority 10)
  (:r #x0f41 (r/m32 r32) :operand-mode :32-bit :priority 10))

(def-instr cmovnp (instruction) 		; if not parity
  (:r #x0f4b (r/m16 r16) :operand-mode :16-bit :priority 10)
  (:r #x0f4b (r/m32 r32) :operand-mode :32-bit :priority 10))

(def-instr cmovns (instruction) 		; if not sign
  (:r #x0f49 (r/m16 r16) :operand-mode :16-bit :priority 10)
  (:r #x0f49 (r/m32 r32) :operand-mode :32-bit :priority 10))

(def-instr cmovnz (instruction) 		; if not zero
  (:r #x0f45 (r/m16 r16) :operand-mode :16-bit :priority 0)
  (:r #x0f45 (r/m32 r32) :operand-mode :32-bit :priority 0))

(def-instr cmovo (instruction) 		; if overflow
  (:r #x0f40 (r/m16 r16) :operand-mode :16-bit :priority 10)
  (:r #x0f40 (r/m32 r32) :operand-mode :32-bit :priority 10))

(def-instr cmovp (instruction)		; if parity
  (:r #x0f4a (r/m16 r16) :operand-mode :16-bit :priority 0)
  (:r #x0f4a (r/m32 r32) :operand-mode :32-bit :priority 0))

(def-instr cmovpe (instruction) 		; if parity even
  (:r #x0f4a (r/m16 r16) :operand-mode :16-bit :priority 10)
  (:r #x0f4a (r/m32 r32) :operand-mode :32-bit :priority 10))

(def-instr cmovpo (instruction)		; if parity odd
  (:r #x0f4b (r/m16 r16) :operand-mode :16-bit :priority 20)
  (:r #x0f4b (r/m32 r32) :operand-mode :32-bit :priority 20))

(def-instr cmovs (instruction)		; if sign
  (:r #x0f48 (r/m16 r16) :operand-mode :16-bit :priority 10)
  (:r #x0f48 (r/m32 r32) :operand-mode :32-bit :priority 10))

(def-instr cmovz (instruction)		; if cmovz
  (:r #x0f44 (r/m16 r16) :operand-mode :16-bit :priority 0)
  (:r #x0f44 (r/m32 r32) :operand-mode :32-bit :priority 0))

;;; ----------------------------------------------------------------
;;;                  MOV class PRINT-OBJECT
;;; ----------------------------------------------------------------

(cl:defmethod cl:print-object ((obj mov) stream)
  (cl:if (cl:and cl:*print-pretty*
		 (cl:= 2 (cl:length (instruction-operands obj))))
      (cl:progn
	(cl:format stream
		   "#<asm ~@[~A ~]~A ~A => ~A>"
		   (instruction-prefixes obj)
		   (cl:type-of obj)
		   (cl:first (instruction-operands obj))
		   (cl:second (instruction-operands obj)))
	obj)
    (cl:call-next-method obj stream)))
