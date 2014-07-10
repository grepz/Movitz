;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 20012000, 2002,
;;;;    Department of Computer Science, University of Tromso, Norway
;;;; 
;;;; Filename:      instr-cmp.lisp
;;;; Description:   
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Sat Jan 29 20:57:45 2000
;;;; Distribution:  See the accompanying file COPYING.
;;;;                
;;;; $Id: instr-cmp.lisp,v 1.2 2004/01/16 11:54:14 ffjeld Exp $
;;;;                
;;;;------------------------------------------------------------------

(in-package "IA-X86-INSTR")

;;; ----------------------------------------------------------------
;;;                  CMP   [IISR page 11-64] 
;;; ----------------------------------------------------------------

(def-instr cmp (instruction))
(def-instr cmpb (cmp)
  (:plain #x3c (0 1) (imm8 al))
  (:digit (#x80 7) 1 (imm8 r/m8))
  (:r #x38 (r8 r/m8))
  (:r #x3a (r/m8 r8)))

(def-instr cmpw (cmp)
  (:plain #x3d (0 2) (imm16 ax) :operand-mode :16-bit)
  (:digit (#x81 7) 2 (imm16 r/m16) :operand-mode :16-bit)
  (:digit (#x83 7) 1 (simm8 r/m16) :operand-mode :16-bit)
  (:r #x39 (r16 r/m16) :operand-mode :16-bit)
  (:r #x3b (r/m16 r16) :operand-mode :16-bit))

(def-instr cmpl (cmp)
  (:plain #x3d (0 4) (simm32 eax) :operand-mode :32-bit)
  (:digit (#x81 7) 4 (simm32 r/m32) :operand-mode :32-bit)
  (:digit (#x83 7) 1 (simm8 r/m32) :operand-mode :32-bit)
  (:r #x39 (r32 r/m32) :operand-mode :32-bit)
  (:r #x3b (r/m32 r32) :operand-mode :32-bit))

;;; ----------------------------------------------------------------
;;;                     TEST    [IISR page 11-377]
;;; ----------------------------------------------------------------

(def-instr test (instruction))
(def-instr testb (test)
  (:plain #xa8 (0 1) (imm8 al))
  (:digit (#xf6 0) 1 (imm8 r/m8))
  (:r #x84 (r8 r/m8)))

(def-instr testw (test)
  (:plain #xa9 (0 2) (imm16 ax) :operand-mode :16-bit)
  (:digit (#xf7 0) 2 (imm16 r/m16) :operand-mode :16-bit)
  (:r #x85 (r16 r/m16) :operand-mode :16-bit))

(def-instr testl (test)
  (:plain #xa9 (0 4) (imm32 eax) :operand-mode :32-bit)
  (:digit (#xf7 0) 4 (imm32 r/m32) :operand-mode :32-bit)
  (:r #x85 (r32 r/m32) :operand-mode :32-bit))

;;; Compare and Exchange

(def-instr cmpxchg (cmp) 
  (:r #x0fb0 (r8 r/m8))
  (:r #x0fb1 (r16 r/m16) :operand-mode :16-bit)
  (:r #x0fb1 (r32 r/m32) :operand-mode :32-bit))

(def-instr cmpxchg8b (cmpxchg) 
  (:digit (#x0fc7 1) 0 (m64) :indirect t))
