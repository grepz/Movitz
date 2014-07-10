;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 20012000,
;;;;    Department of Computer Science, University of Tromso, Norway
;;;; 
;;;; Filename:      ia-x86-instr-push-pop.lisp
;;;; Description:   Stack instructions.
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Mon Jan 31 20:28:50 2000
;;;; Distribution:  See the accompanying file COPYING.
;;;;                
;;;; $Id: instr-push-pop.lisp,v 1.2 2004/01/16 11:54:14 ffjeld Exp $
;;;;                
;;;;------------------------------------------------------------------

(in-package "IA-X86-INSTR")

;;; ----------------------------------------------------------------
;;;                   PUSH   [IISR page 11-317]
;;; ----------------------------------------------------------------

(def-instr push (instruction))

(def-instr pushw (push)
  (:digit (#xff 6) 0 (r/m16) :operand-mode :16-bit)
  (:+ #x50 0 (+r16) :operand-mode :16-bit)
  (:plain #x6a (0 1) (simm8) :operand-mode :16-bit)
  (:plain #x68 (0 2) (imm16) :operand-mode :16-bit)
  (:plain #x0e (0 0) (cs) :operand-mode :16-bit)
  (:plain #x16 (0 0) (ss) :operand-mode :16-bit)
  (:plain #x1e (0 0) (ds) :operand-mode :16-bit)
  (:plain #x06 (0 0) (es) :operand-mode :16-bit)
  (:plain #c(#x0fa0 2) (0 0) (fs) :operand-mode :16-bit)
  (:plain #c(#x0fa8 2) (0 0) (gs) :operand-mode :16-bit))

(def-instr pushl (push)
  (:digit (#xff 6) 0 (r/m32) :operand-mode :32-bit)
  (:+ #x50 0 (+r32) :operand-mode :32-bit)
  (:plain #x6a (0 1) (simm8) :operand-mode :32-bit)
  (:plain #x68 (0 4) (imm32) :operand-mode :32-bit)
       
  (:plain #x0e (0 0) (cs) :operand-mode :32-bit)
  (:plain #x16 (0 0) (ss) :operand-mode :32-bit)
  (:plain #x1e (0 0) (ds) :operand-mode :32-bit)
  (:plain #x06 (0 0) (es) :operand-mode :32-bit)
  (:plain #c(#x0fa0 2) (0 0) (fs) :operand-mode :32-bit)
  (:plain #c(#x0fa8 2) (0 0) (gs) :operand-mode :32-bit))

;;; PUSHA

(def-instr pusha (instruction))
(def-instr pushaw (pusha) (:simple #x60 :operand-mode :16-bit))
(def-instr pushal (pusha) (:simple #x60 :operand-mode :32-bit))

;;; PUSHF

(def-instr pushf (instruction))
(def-instr pushfw (pushf) (:simple #x9C :operand-mode :16-bit))
(def-instr pushfl (pushf) (:simple #x9C :operand-mode :32-bit))

;;; ----------------------------------------------------------------
;;;                   POP   [IISR page 11-308]
;;; ----------------------------------------------------------------

;;; These top two are described as format /digit in IISR, but in
;;; practice it seems that the reg/opcode field is simply
;;; ignored/redundant.

(def-instr pop (instruction))

(def-instr popw (pop)
  (:r #x8f (r/m16) :operand-mode :16-bit)
  (:+ #x58 0 (+r16) :operand-mode :16-bit)
  (:plain #x1f (0 0) (ds) :operand-mode :16-bit)
  (:plain #x07 (0 0) (es) :operand-mode :16-bit)
  (:plain #x17 (0 0) (ss) :operand-mode :16-bit)
  (:plain #c(#x0fa1 2) (0 0) (fs) :operand-mode :16-bit)
  (:plain #c(#x0fa9 2) (0 0) (gs) :operand-mode :16-bit))

(def-instr popl (pop)
  (:r #x8f (r/m32) :operand-mode :32-bit)
  (:+ #x58 0 (+r32) :operand-mode :32-bit)
  (:plain #x1f (0 0) (ds) :operand-mode :32-bit)
  (:plain #x07 (0 0) (es) :operand-mode :32-bit)
  (:plain #x17 (0 0) (ss) :operand-mode :32-bit)
  (:plain #c(#x0fa1 2) (0 0) (fs) :operand-mode :32-bit)
  (:plain #c(#x0fa9 2) (0 0) (gs) :operand-mode :32-bit))

;;; ----------------------------------------------------------------
;;;                   POPA   [IISR page 11-312]
;;; ----------------------------------------------------------------

(def-instr popa (instruction))
(def-instr popaw (popa) (:plain #x61 (0 0) () :operand-mode :16-bit))
(def-instr popal (popa) (:plain #x61 (0 0) () :operand-mode :32-bit))

;;; ----------------------------------------------------------------
;;;                   POPF   [IISR page 11-314]
;;; ----------------------------------------------------------------

(def-instr popf (instruction))
(def-instr popfw (popf) (:plain #x9d (0 0) () :operand-mode :16-bit))
(def-instr popfl (popf) (:plain #x9d (0 0) () :operand-mode :32-bit))

