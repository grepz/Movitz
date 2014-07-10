;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 2000, 2002,
;;;;    Department of Computer Science, University of Tromso, Norway
;;;; 
;;;; Filename:      instr-mul-div.lisp
;;;; Description:   Multiplication and division instructions
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Mon Jan 31 21:12:11 2000
;;;; Distribution:  See the accompanying file COPYING.
;;;;                
;;;; $Id: instr-mul-div.lisp,v 1.2 2004/01/16 11:54:14 ffjeld Exp $
;;;;                
;;;;------------------------------------------------------------------

(in-package "IA-X86-INSTR")

;;;

(def-instr mul (instruction))
(def-instr mulb (mul) (:digit (#xf6 4) 0 (r/m8 ax al)))
(def-instr mulw (mul) (:digit (#xf7 4) 0 (r/m16 ax dx) :operand-mode :16-bit))
(def-instr mull (mul) (:digit (#xf7 4) 0 (r/m32 eax edx) :operand-mode :32-bit))

;;;

(def-instr imul (instruction))
(def-instr imulb (imul)
  (:digit (#xf6 5) 0 (r/m8 ax al)))

(def-instr imulw (imul)
  (:digit (#xf7 5) 0 (r/m16 ax dx) :operand-mode :16-bit)
  (:r #c(#x0faf 2) (r/m16 r16) :operand-mode :16-bit)
  (:r-imm #x6b 1 (imm8 r/m16 r16) :operand-mode :16-bit)
  (:r-imm #x69 2 (imm16 r/m16 r16) :operand-mode :16-bit))

(def-instr imull (imul)
  (:digit (#xf7 5) 0 (r/m32 eax edx) :operand-mode :32-bit)
  (:r #c(#x0faf 2) (r/m32 r32) :operand-mode :32-bit)
  (:r-imm #x6b 1 (simm8 r/m32 r32) :operand-mode :32-bit)
  (:r-imm #x69 4 (simm32 r/m32 r32) :operand-mode :32-bit))

;;; Unsigned Divide

(def-instr div (instruction))

(def-instr divb (div)
  ;; divide AX by r/m8 into AL, AH
  (:digit (#xf6 6) 0 (r/m8 ax)))
(def-instr divw (div)
  ;; divide DX:AX by r/m16 into AX, DX
  (:digit (#xf7 6) 0 (r/m16 ax dx) :operand-mode :16-bit))
(def-instr divl (div)
  ;; divide EDX:EAX by r/m32 into EAX, EDX
  (:digit (#xf7 6) 0 (r/m32 eax edx) :operand-mode :32-bit))

;;; Signed Divide

(def-instr idiv (instruction))
(def-instr idivb (idiv)
  ;; divide AX by r/m8 into AL, AH
  (:digit (#xf6 7) 0 (r/m8 ax)))
(def-instr idivw (idiv)
  ;; divide DX:AX by r/m16 into AX, DX
  (:digit (#xf7 7) 0 (r/m16 ax dx) :operand-mode :16-bit))
(def-instr idivl (idiv)
  ;; divide EDX:EAX by r/m32 into EAX, EDX
  (:digit (#xf7 7) 0 (r/m32 eax edx) :operand-mode :32-bit))
