;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 2001-2004,
;;;;    Department of Computer Science, University of Tromso, Norway
;;;; 
;;;; Filename:      instr-and-or.lisp
;;;; Description:   Bitwise boolean operations.
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Tue Feb  1 17:39:09 2000
;;;; Distribution:  See the accompanying file COPYING.
;;;;                
;;;; $Id: instr-and-or.lisp,v 1.3 2004/06/21 07:33:00 ffjeld Exp $
;;;;                
;;;;------------------------------------------------------------------

(in-package #:ia-x86-instr)

;;; ----------------------------------------------------------------
;;;                  AND   [IISR page 11-64] 
;;; ----------------------------------------------------------------

(def-instr and (instruction))

(def-instr andb (and)
  (:plain #x24 (0 1) (imm8 al))
  (:digit (#x80 4) 1 (imm8 r/m8))
  (:r #x20 (r8 r/m8))
  (:r #x22 (r/m8 r8)))

(def-instr andw (and)
  (:plain #x25 (0 2) (imm16 ax) :operand-mode :16-bit)
  (:digit (#x81 4) 2 (imm16 r/m16) :operand-mode :16-bit)
  (:digit (#x83 4) 1 (simm8 r/m16) :operand-mode :16-bit)
  (:r #x21 (r16 r/m16) :operand-mode :16-bit)
  (:r #x23 (r/m16 r16) :operand-mode :16-bit))

(def-instr andl (and)
  (:plain #x25 (0 4) (imm32 eax) :operand-mode :32-bit)
  (:digit (#x81 4) 4 (imm32 r/m32) :operand-mode :32-bit)
  (:digit (#x83 4) 1 (simm8 r/m32) :operand-mode :32-bit)
  (:r #x21 (r32 r/m32) :operand-mode :32-bit)
  (:r #x23 (r/m32 r32) :operand-mode :32-bit))

;;; ----------------------------------------------------------------
;;;                  OR   [IISR page 11-301] 
;;; ----------------------------------------------------------------

(def-instr or (instruction))

(def-instr orb (or)
  (:plain #x0c (0 1) (imm8 al))
  (:digit (#x80 1) 1 (imm8 r/m8))
  (:r #x08 (r8 r/m8))
  (:r #x0a (r/m8 r8)))

(def-instr orw (or)
  (:plain #x0d (0 2) (imm16 ax) :operand-mode :16-bit)
  (:digit (#x81 1) 2 (imm16 r/m16) :operand-mode :16-bit)
  (:digit (#x83 1) 1 (simm8 r/m16) :operand-mode :16-bit)
  (:r #x09 (r16 r/m16) :operand-mode :16-bit)
  (:r #x0b (r/m16 r16) :operand-mode :16-bit))

(def-instr orl (or)  
  (:plain #x0d (0 4) (imm32 eax) :operand-mode :32-bit)
  (:digit (#x81 1) 4 (imm32 r/m32) :operand-mode :32-bit)
  (:digit (#x83 1) 1 (simm8 r/m32) :operand-mode :32-bit)
  (:r #x09 (r32 r/m32) :operand-mode :32-bit)
  (:r #x0b (r/m32 r32) :operand-mode :32-bit))

;;; ----------------------------------------------------------------
;;;                  XOR   [IISR page 11-392] 
;;; ----------------------------------------------------------------

(def-instr xor (instruction))

(def-instr xorb (xor)
  (:plain #x34 (0 1) (imm8 al))
  (:digit (#x80 6) 1 (imm8 r/m8))
  (:r #x30 (r8 r/m8))
  (:r #x32 (r/m8 r8)))

(def-instr xorw (xor)
  (:plain #x35 (0 2) (imm16 ax) :operand-mode :16-bit)
  (:digit (#x81 6) 2 (imm16 r/m16) :operand-mode :16-bit)
  (:digit (#x83 6) 1 (simm8 r/m16) :operand-mode :16-bit)
  (:r #x31 (r16 r/m16) :operand-mode :16-bit)
  (:r #x33 (r/m16 r16) :operand-mode :16-bit))

(def-instr xorl (xor)
  (:plain #x35 (0 4) (imm32 eax) :operand-mode :32-bit)
  (:digit (#x81 6) 4 (imm32 r/m32) :operand-mode :32-bit)
  (:digit (#x83 6) 1 (simm8 r/m32) :operand-mode :32-bit)
  (:r #x31 (r32 r/m32) :operand-mode :32-bit)
  (:r #x33 (r/m32 r32) :operand-mode :32-bit))

;;; ----------------------------------------------------------------
;;;                  NOT [IISR page 11-299]
;;; ----------------------------------------------------------------

(def-instr not (instruction))
(def-instr notb (not) (:digit (#xf6 2) 0 (r/m8)))
(def-instr notw (not) (:digit (#xf7 2) 0 (r/m16) :operand-mode :16-bit))
(def-instr notl (not) (:digit (#xf7 2) 0 (r/m32) :operand-mode :32-bit))

