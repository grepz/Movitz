;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 20012000, 2002,
;;;;    Department of Computer Science, University of Tromso, Norway
;;;; 
;;;; Filename:      instr-shift.lisp
;;;; Description:   Shifting operations
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Tue May  2 10:56:33 2000
;;;; Distribution:  See the accompanying file COPYING.
;;;;                
;;;; $Id: instr-shift.lisp,v 1.2 2004/01/16 11:54:14 ffjeld Exp $
;;;;                
;;;;------------------------------------------------------------------

(in-package "IA-X86-INSTR")

;;; ----------------------------------------------------------------
;;;                            SAL
;;; ----------------------------------------------------------------

(def-instr shl (instruction))

(def-instr shlb (shl)
  (:digit (#xd0 4) 0 (1 r/m8))
  (:digit (#xd2 4) 0 (cl r/m8))
  (:digit (#xc0 4) 1 (imm8 r/m8)))

(def-instr shlw (shl)
  (:digit (#xd1 4) 0 (1 r/m16) :operand-mode :16-bit)
  (:digit (#xd3 4) 0 (cl r/m16) :operand-mode :16-bit)
  (:digit (#xc1 4) 1 (imm8 r/m16) :operand-mode :16-bit))

(def-instr shll (shl)
  (:digit (#xd1 4) 0 (1 r/m32) :operand-mode :32-bit)
  (:digit (#xd3 4) 0 (cl r/m32) :operand-mode :32-bit)
  (:digit (#xc1 4) 1 (imm8 r/m32) :operand-mode :32-bit))

;;; ----------------------------------------------------------------
;;;                            SAR
;;; ----------------------------------------------------------------

;;; Shift arithmethic right

(def-instr sar (instruction))

(def-instr sarb (sar)
  (:digit (#xd0 7) 0 (1 r/m8))
  (:digit (#xd2 7) 0 (cl r/m8))
  (:digit (#xc0 7) 1 (imm8 r/m8)))

(def-instr sarw (sar)
  (:digit (#xd1 7) 0 (1 r/m16) :operand-mode :16-bit)
  (:digit (#xd3 7) 0 (cl r/m16) :operand-mode :16-bit)
  (:digit (#xc1 7) 1 (imm8 r/m16) :operand-mode :16-bit))

(def-instr sarl (sar)
  (:digit (#xd1 7) 0 (1 r/m32) :operand-mode :32-bit)
  (:digit (#xd3 7) 0 (cl r/m32) :operand-mode :32-bit)
  (:digit (#xc1 7) 1 (imm8 r/m32) :operand-mode :32-bit))

;;; Shift right

(def-instr shr (instruction))
(def-instr shrb (shr)
  (:digit (#xd0 5) 0 (1 r/m8))
  (:digit (#xd2 5) 0 (cl r/m8))
  (:digit (#xc0 5) 1 (imm8 r/m8)))

(def-instr shrw (shr)
  (:digit (#xd1 5) 0 (1 r/m16) :operand-mode :16-bit)
  (:digit (#xd3 5) 0 (cl r/m16) :operand-mode :16-bit)
  (:digit (#xc1 5) 1 (imm8 r/m16) :operand-mode :16-bit))

(def-instr shrl (shr)
  (:digit (#xd1 5) 0 (1 r/m32) :operand-mode :32-bit)
  (:digit (#xd3 5) 0 (cl r/m32) :operand-mode :32-bit)
  (:digit (#xc1 5) 1 (imm8 r/m32) :operand-mode :32-bit))


;;; Double Precision Shift Left

(def-instr shld (instruction))
(def-instr shldw (shld)
  (:r-imm #x0fa4 1 (imm8 r16 r/m16) :operand-mode :16-bit)
  (:r #x0fa5 (cl r16 r/m16) :operand-mode :16-bit))

(def-instr shldl (shld)
  (:r-imm #x0fa4 1 (imm8 r32 r/m32) :operand-mode :32-bit)
  (:r #x0fa5 (cl r32 r/m32) :operand-mode :32-bit))

;;; Double Precision Shift Right

(def-instr shrd (instruction))
(def-instr shrdw (shrd)
  (:r-imm #x0fac 1 (imm8 r16 r/m16) :operand-mode :16-bit)
  (:r #x0fad (cl r16 r/m16) :operand-mode :16-bit))
(def-instr shrdl (shrd)
  (:r-imm #x0fac 1 (imm8 r32 r/m32) :operand-mode :32-bit)
  (:r #x0fad (cl r32 r/m32) :operand-mode :32-bit))


;;; Rotate left with CF

(def-instr rcl (instruction))

(def-instr rclb (shl)
  (:digit (#xd0 2) 0 (1 r/m8))
  (:digit (#xd2 2) 0 (cl r/m8))
  (:digit (#xc0 2) 1 (imm8 r/m8)))

(def-instr rclw (rcl)
  (:digit (#xd1 2) 0 (1 r/m16) :operand-mode :16-bit)
  (:digit (#xd3 2) 0 (cl r/m16) :operand-mode :16-bit)
  (:digit (#xc1 2) 1 (imm8 r/m16) :operand-mode :16-bit))

(def-instr rcll (rcl)
  (:digit (#xd1 2) 0 (1 r/m32) :operand-mode :32-bit)
  (:digit (#xd3 2) 0 (cl r/m32) :operand-mode :32-bit)
  (:digit (#xc1 2) 1 (imm8 r/m32) :operand-mode :32-bit))

;;; Rotate left without CF

(def-instr rol (instruction))

(def-instr rolb (shl)
  (:digit (#xd0 0) 0 (1 r/m8))
  (:digit (#xd2 0) 0 (cl r/m8))
  (:digit (#xc0 0) 1 (imm8 r/m8)))

(def-instr rolw (rol)
  (:digit (#xd1 0) 0 (1 r/m16) :operand-mode :16-bit)
  (:digit (#xd3 0) 0 (cl r/m16) :operand-mode :16-bit)
  (:digit (#xc1 0) 1 (imm8 r/m16) :operand-mode :16-bit))

(def-instr roll (rol)
  (:digit (#xd1 0) 0 (1 r/m32) :operand-mode :32-bit)
  (:digit (#xd3 0) 0 (cl r/m32) :operand-mode :32-bit)
  (:digit (#xc1 0) 1 (imm8 r/m32) :operand-mode :32-bit))

;;; Rotate right without CF

(def-instr ror (instruction))

(def-instr rorb (shl)
  (:digit (#xd0 1) 0 (1 r/m8))
  (:digit (#xd2 1) 0 (cl r/m8))
  (:digit (#xc0 1) 1 (imm8 r/m8)))

(def-instr rorw (ror)
  (:digit (#xd1 1) 0 (1 r/m16) :operand-mode :16-bit)
  (:digit (#xd3 1) 0 (cl r/m16) :operand-mode :16-bit)
  (:digit (#xc1 1) 1 (imm8 r/m16) :operand-mode :16-bit))

(def-instr rorl (ror)
  (:digit (#xd1 1) 0 (1 r/m32) :operand-mode :32-bit)
  (:digit (#xd3 1) 0 (cl r/m32) :operand-mode :32-bit)
  (:digit (#xc1 1) 1 (imm8 r/m32) :operand-mode :32-bit))

