;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 2001-2004,
;;;;    Department of Computer Science, University of Tromso, Norway
;;;; 
;;;; Filename:      instr-add.lisp
;;;; Description:   Addition-related instructions.
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Sat Jan 29 20:20:18 2000
;;;; Distribution:  See the accompanying file COPYING.
;;;;                
;;;; $Id: instr-add.lisp,v 1.3 2004/06/21 07:32:56 ffjeld Exp $
;;;;                
;;;;------------------------------------------------------------------

(in-package #:ia-x86-instr)

;;; ----------------------------------------------------------------
;;;                   ADD   [IISR page 11-22]
;;; ----------------------------------------------------------------

(def-instr add (instruction))

(def-instr addb (add)
  (:plain #x04 (0 1) (imm8 al))
  (:digit (#x80 0) 1 (imm8 r/m8))
  (:r #x00 (r8 r/m8))
  (:r #x02 (r/m8 r8)))

(def-instr addw (add)  
  (:plain #x05 (0 2) (imm16 ax) :operand-mode :16-bit)
  (:digit (#x81 0) 2 (imm16 r/m16) :operand-mode :16-bit)
  (:digit (#x83 0) 1 (imm8 r/m16) :operand-mode :16-bit) ; sign-extend
  (:r #x01 (r16 r/m16) :operand-mode :16-bit)
  (:r #x03 (r/m16 r16) :operand-mode :16-bit))

(def-instr addl (add)  
  (:plain #x05 (0 4) (simm32 eax) :operand-mode :32-bit)
  (:digit (#x81 0) 4 (simm32 r/m32) :operand-mode :32-bit)
  (:digit (#x83 0) 1 (simm8 r/m32) :operand-mode :32-bit)
  (:r #x01 (r32 r/m32) :operand-mode :32-bit)
  (:r #x03 (r/m32 r32) :operand-mode :32-bit))

;;; ----------------------------------------------------------------
;;;            ADC - add with carry   [IISR page 11-20] 
;;; ----------------------------------------------------------------

(def-instr adc (instruction))
(def-instr adcb (adc)
  (:plain #x14 (0 1) (imm8 al))
  (:digit (#x80 2) 1 (imm8 r/m8))
  (:r #x10 (r8 r/m8))
  (:r #x12 (r/m8 r8)))

(def-instr adcw (adc)
  (:plain #x15 (0 2) (imm16 ax) :operand-mode :16-bit)
  (:digit (#x81 2) 2 (imm16 r/m16) :operand-mode :16-bit)
  (:digit (#x83 2) 1 (imm8 r/m16) :operand-mode :16-bit) ; sign-extend
  (:r #x11 (r16 r/m16) :operand-mode :16-bit)
  (:r #x13 (r/m16 r16) :operand-mode :16-bit))

(def-instr adcl (adc)
  (:plain #x15 (0 4) (simm32 eax) :operand-mode :32-bit)
  (:digit (#x81 2) 4 (simm32 r/m32) :operand-mode :32-bit)
  (:digit (#x83 2) 1 (simm8 r/m32) :operand-mode :32-bit)
  (:r #x11 (r32 r/m32) :operand-mode :32-bit)
  (:r #x13 (r/m32 r32) :operand-mode :32-bit))

;;; ----------------------------------------------------------------
;;;                INC   [IISR page 11-211] 
;;; ----------------------------------------------------------------

(def-instr inc (instruction))

(def-instr incb (inc)
  (:digit (#xfe 0) 0 (r/m8)))

(def-instr incw (inc)
  (:+ #x40 0 (+r16) :operand-mode :16-bit)
  (:digit (#xff 0) 0 (r/m16) :operand-mode :16-bit))

(def-instr incl (inc)
  (:+ #x40 0 (+r32) :operand-mode :32-bit)
  (:digit (#xff 0) 0 (r/m32) :operand-mode :32-bit))

;;; Exchange and add

(def-instr xadd (add))
(def-instr xaddb (xadd) (:r #x0fc0 (r8 r/m8)))
(def-instr xaddw (xadd) (:r #x0fc1 (r16 r/m16) :operand-mode :16-bit))
(def-instr xaddl (xadd) (:r #x0fc1 (r32 r/m32) :operand-mode :32-bit))
