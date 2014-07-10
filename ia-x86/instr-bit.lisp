;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 2000,
;;;;    Department of Computer Science, University of Tromso, Norway
;;;; 
;;;; Filename:      ia-x86-instr-bit.lisp
;;;; Description:   Bit operations.
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Tue Feb  1 20:03:30 2000
;;;; Distribution:  See the accompanying file COPYING.
;;;;                
;;;; $Id: instr-bit.lisp,v 1.2 2004/01/16 11:54:14 ffjeld Exp $
;;;;                
;;;;------------------------------------------------------------------

(in-package "IA-X86-INSTR")

;;; ----------------------------------------------------------------
;;;                  Bit instructions
;;; ----------------------------------------------------------------

(def-instr bsf (instruction))		; bit scan forward
(def-instr bsfw (bsf) (:r #c(#x0fbc 2) (r/m16 r16) :operand-mode :16-bit))
(def-instr bsfl (bsf) (:r #c(#x0fbc 2) (r/m32 r32) :operand-mode :32-bit))

(def-instr bsr (instruction))		; bit scan reverse
(def-instr bsrw (bsr) (:r #c(#x0fbd 2) (r/m16 r16) :operand-mode :16-bit))
(def-instr bsrl (bsr) (:r #c(#x0fbd 2) (r/m32 r32) :operand-mode :32-bit))

;;; Bit Test

(def-instr bt (instruction))

(def-instr btw (bt)
  (:r #c(#x0fa3 2) (r16 r/m16) :operand-mode :16-bit)
  (:digit (#c(#x0fba 2) 4) 1 (imm8 r/m16) :operand-mode :16-bit))

(def-instr btl (bt)
  (:r #c(#x0fa3 2) (r32 r/m32) :operand-mode :32-bit)
  (:digit (#c(#x0fba 2) 4) 1 (imm8 r/m32) :operand-mode :32-bit))


;;; bit test and complement

(def-instr btc (instruction))

(def-instr btcw (btc)
  (:r #c(#x0fbb 2) (r16 r/m16) :operand-mode :16-bit)
  (:digit (#c(#x0fba 2) 7) 1 (imm8 r/m16) :operand-mode :16-bit))

(def-instr btcl (btc)
  (:r #c(#x0fbb 2) (r32 r/m32) :operand-mode :32-bit)
  (:digit (#c(#x0fba 2) 7) 1 (imm8 r/m32) :operand-mode :32-bit))


;;; bit test and reset

(def-instr btr (instruction))

(def-instr btrw (btr)
  (:r #c(#x0fb3 2) (r16 r/m16) :operand-mode :16-bit)
  (:digit (#c(#x0fba 2) 6) 1 (imm8 r/m16) :operand-mode :16-bit))

(def-instr btrl (btr)
  (:r #c(#x0fb3 2) (r32 r/m32) :operand-mode :32-bit)
  (:digit (#c(#x0fba 2) 6) 1 (imm8 r/m32) :operand-mode :32-bit))

;;; bit test and set

(def-instr bts (instruction))	

(def-instr btsw (bts)
  (:r #c(#x0fab 2) (r16 r/m16) :operand-mode :16-bit)
  (:digit (#c(#x0fba 2) 5) 1 (imm8 r/m16) :operand-mode :16-bit))

(def-instr btsl (bts)
  (:r #c(#x0fab 2) (r32 r/m32) :operand-mode :32-bit)
  (:digit (#c(#x0fba 2) 5) 1 (imm8 r/m32) :operand-mode :32-bit))
