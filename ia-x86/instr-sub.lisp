;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 2000-2002,
;;;;    Department of Computer Science, University of Tromso, Norway
;;;; 
;;;; Filename:      instr-sub.lisp
;;;; Description:   
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Tue Feb  1 14:16:58 2000
;;;; Distribution:  See the accompanying file COPYING.
;;;;                
;;;; $Id: instr-sub.lisp,v 1.2 2004/01/16 11:54:14 ffjeld Exp $
;;;;                
;;;;------------------------------------------------------------------

(in-package "IA-X86-INSTR")

;;; ----------------------------------------------------------------
;;;                   SUB   [IISR page 11-375]
;;; ----------------------------------------------------------------

(def-instr sub (instruction))
(def-instr subb (sub)
  (:plain #x2c (0 1) (imm8 al))
  (:digit (#x80 5) 1 (imm8 r/m8))
  (:r #x28 (r8 r/m8))
  (:r #x2a (r/m8 r8)))

(def-instr subw (sub)
  (:plain #x2d (0 2) (imm16 ax) :operand-mode :16-bit)
  (:digit (#x81 5) 2 (imm16 r/m16) :operand-mode :16-bit)
  (:digit (#x83 5) 1 (simm8 r/m16) :operand-mode :16-bit)
  (:r #x29 (r16 r/m16) :operand-mode :16-bit)
  (:r #x2b (r/m16 r16) :operand-mode :16-bit))

(def-instr subl (sub)
  (:plain #x2d (0 4) (simm32 eax) :operand-mode :32-bit)
  (:digit (#x81 5) 4 (simm32 r/m32) :operand-mode :32-bit)
  (:digit (#x83 5) 1 (simm8 r/m32) :operand-mode :32-bit)
  (:r #x29 (r32 r/m32) :operand-mode :32-bit)
  (:r #x2b (r/m32 r32) :operand-mode :32-bit))

;;; ----------------------------------------------------------------
;;;                   SBB   [IISR page 11-349]
;;; ----------------------------------------------------------------

(def-instr sbb (instruction))
(def-instr sbbb (sbb)
  (:plain #x1c (0 1) (simm8 al))
  (:digit (#x80 3) 1 (simm8 r/m8))
  (:r #x18 (r8 r/m8))
  (:r #x1a (r/m8 r8)))

(def-instr sbbw (sbb)
  (:plain #x1d (0 2) (imm16 ax) :operand-mode :16-bit)
  (:digit (#x81 3) 2 (imm16 r/m16) :operand-mode :16-bit)
  (:digit (#x83 3) 1 (imm8 r/m16) :operand-mode :16-bit)
  (:r #x19 (r16 r/m16) :operand-mode :16-bit)
  (:r #x1b (r/m16 r16) :operand-mode :16-bit))

(def-instr sbbl (sbb)
  (:plain #x1d (0 4) (simm32 eax) :operand-mode :32-bit)
  (:digit (#x81 3) 4 (simm32 r/m32) :operand-mode :32-bit)
  (:digit (#x83 3) 1 (simm8 r/m32) :operand-mode :32-bit)
  (:r #x19 (r32 r/m32) :operand-mode :32-bit)
  (:r #x1b (r/m32 r32) :operand-mode :32-bit))

;;; ----------------------------------------------------------------
;;;                DEC   [IISR page 11-84] 
;;; ----------------------------------------------------------------

(def-instr dec (instruction))

(def-instr decb (dec)
  (:digit (#xfe 1) 0 (r/m8)))
(def-instr decw (dec)
  (:+ #x48 0 (+r16) :operand-mode :16-bit)
  (:digit (#xff 1) 0 (r/m16) :operand-mode :16-bit))
(def-instr decl (dec)
  (:+ #x48 0 (+r32) :operand-mode :32-bit)
  (:digit (#xff 1) 0 (r/m32) :operand-mode :32-bit))


;;; ----------------------------------------------------------------
;;;                           NEG
;;; ----------------------------------------------------------------

(def-instr neg (instruction))
(def-instr negb (neg) (:digit (#xf6 3) 0 (r/m8)))
(def-instr negw (neg) (:digit (#xf7 3) 0 (r/m16) :operand-mode :16-bit))
(def-instr negl (neg) (:digit (#xf7 3) 0 (r/m32) :operand-mode :32-bit))
