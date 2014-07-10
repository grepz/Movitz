;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 20012000, 2002-2003,
;;;;    Department of Computer Science, University of Tromso, Norway
;;;; 
;;;; Filename:      instr-branch.lisp
;;;; Description:   Branch instructions.
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Tue Feb  1 15:05:51 2000
;;;; Distribution:  See the accompanying file COPYING.
;;;;                
;;;; $Id: instr-branch.lisp,v 1.2 2004/01/16 11:54:14 ffjeld Exp $
;;;;                
;;;;------------------------------------------------------------------

(in-package "IA-X86-INSTR")

(defclass branch (instruction) ())	; superclass for all branch instructions.
(defclass conditional-branch (branch) ())
(defclass unconditional-branch (branch) ())

;;; ----------------------------------------------------------------
;;;                     JMP    [IISR page 11-241]
;;; ----------------------------------------------------------------

(def-instr jmp (unconditional-branch)
  (:plain #xEB (1 0) (rel8))
  (:plain #xE9 (2 0) (rel16) :cpu-mode :16-bit)
  (:plain #xE9 (4 0) (rel32) :operand-mode :32-bit)

  (:digit (#xFF 4) 0 (r/m16) :cpu-mode :16-bit)
  (:digit (#xFF 4) 0 (r/m32) :operand-mode :32-bit)
  
  (:plain #xEA (2 2) (imm16 displacement) :cpu-mode :16-bit)
  (:plain #xEA (4 2) (imm16 displacement) :operand-mode :32-bit))

(def-instr jmp-segment (unconditional-branch)
  (:digit (#xff 5) 0 (r/m16) :cpu-mode :16-bit)
  (:digit (#xff 5) 0 (r/m32) :operand-mode :32-bit))
    

;;; ----------------------------------------------------------------
;;;                     CALL    [IISR page 11-43]
;;; ----------------------------------------------------------------


;; 16-bit is unusable in 32-bit mode, because EIP is masked by
;; #x0000ffff in this case.

(def-instr call (branch)		; conditional, unconditional, or not a branch at all??
  (:plain #xe8 (2 0) (rel16) :cpu-mode :16-bit)
  (:plain #xe8 (4 0) (rel32) :operand-mode :32-bit)

  (:digit (#xff 2) 0 (r/m16) :cpu-mode :16-bit) 
  (:digit (#xff 2) 0 (r/m32) :operand-mode :32-bit))

  ;; (:plain calls #x9A (4 0) (ptr16-16) :operand-mode :16-bit)
  ;; (:plain #x9A (6 0) (ptr16-32) :operand-mode :32-bit)
  ;; (:digit calls (#xFF 3) 0 (m16-16) :operand-mode :16-bit)
  ;; (:digit  (#xFF 3) 0 (m16-32) :operand-mode :32-bit))

(def-instr call-segment (branch)
  (:digit (#xff 3) 0 (r/m16) :cpu-mode :16-bit)
  (:digit (#xff 3) 0 (r/m32) :operand-mode :32-bit))

;; TODO

;;; ----------------------------------------------------------------
;;;                   Jcc   [IISR page 11-237]
;;; ----------------------------------------------------------------

(def-instr jcc (conditional-branch))

(def-instr ja (jcc)
  (:jcc #x77)
  (:jcc2 #x87))
(def-instr jae (jcc)
  (:jcc #x73)
  (:jcc2 #x83))
(def-instr jb (jcc)
  (:jcc #x72)
  (:jcc2 #x82))
(def-instr jbe (jcc)
  (:jcc #x76)
  (:jcc2 #x86))
(def-instr jc (jcc)
  (:jcc #x72 :priority -10)
  (:jcc2 #x82 :priority -10))
(def-instr jcxz (jcc)
  (:jcc #xe3 :operand-mode :16-bit))
(def-instr jecxz (jcc)
  (:jcc #xe3 :operand-mode :32-bit))
(def-instr je (jcc)
  (:jcc #x74)
  (:jcc2 #x84))
(def-instr jg (jcc)
  (:jcc #x7f)
  (:jcc2 #x8f))
(def-instr jge (jcc)
  (:jcc #x7d)
  (:jcc2 #x8d))
(def-instr jl (jcc)
  (:jcc #x7c)
  (:jcc2 #x8c))
(def-instr jle (jcc)
  (:jcc #x7e)
  (:jcc2 #x8e))
(def-instr jna (jcc)
  (:jcc #x76 :priority -10)
  (:jcc2 #x86 :priority -10))
(def-instr jnae (jcc)
  (:jcc #x72 :priority -10)
  (:jcc2 #x82 :priority -10))
(def-instr jnb (jcc)
  (:jcc #x73 :priority -10)
  (:jcc2 #x83 :priority -10))
(def-instr jnbe (jcc)
  (:jcc #x77 :priority -10)
  (:jcc2 #x87 :priority -10))
(def-instr jnc (jcc)
  (:jcc #x73 :priority -10)
  (:jcc2 #x83 :priority -10))
(def-instr jne (jcc)
  (:jcc #x75)
  (:jcc2 #x85))
(def-instr jng (jcc)
  (:jcc #x7e :priority -10)
  (:jcc2 #x8e :priority -10))
(def-instr jnge (jcc)
  (:jcc #x7c :priority -10)
  (:jcc2 #x8c :priority -10))
(def-instr jnl (jcc)
  (:jcc #x7d :priority -10)
  (:jcc2 #x8d :priority -10))
(def-instr jnle (jcc)
  (:jcc #x7f :priority -10)
  (:jcc2 #x8f :priority -10))
(def-instr jno (jcc)
  (:jcc #x71)
  (:jcc2 #x81))
(def-instr jnp (jcc)
  (:jcc #x7b)
  (:jcc2 #x8b))
(def-instr jns (jcc)
  (:jcc #x79)
  (:jcc2 #x89))
(def-instr jnz (jcc)
  (:jcc #x75 :priority -10)
  (:jcc2 #x85 :priority -10))
(def-instr jo (jcc)
  (:jcc #x70)
  (:jcc2 #x80))
(def-instr jp (jcc)
  (:jcc #x7a)
  (:jcc2 #x8a))
(def-instr jpe (jcc)
  (:jcc #x7a :priority -10)
  (:jcc2 #x8a :priority -10))
(def-instr jpo (jcc)
  (:jcc #x7b :priority -10)
  (:jcc2 #x8b :priority -10))
(def-instr js (jcc)
  (:jcc #x78)
  (:jcc2 #x88))
(def-instr jz (jcc)
  (:jcc #x74 :priority -10)
  (:jcc2 #x84 :priority -10))

;;; ----------------------------------------------------------------
;;;                 LOOP  [IISR page 11-273]
;;; ----------------------------------------------------------------

(def-instr loop   (conditional-branch) (:plain  #xe2 (1 0) (rel8)))
(def-instr loope  (conditional-branch) (:plain  #xe1 (1 0) (rel8)))
(def-instr loopne (conditional-branch) (:plain  #xe0 (1 0) (rel8)))

;;; ----------------------------------------------------------------
;;;                 Misc. branch related instructions
;;; ----------------------------------------------------------------

(def-instr leave (instruction) (:simple #xc9))
(def-instr enter (instruction) (:plain #xC8 (0 3) (imm16-8 imm8-0)))

(def-instr iret  (unconditional-branch) (:simple #xcf :operand-mode :16-bit))
(def-instr iretd (unconditional-branch) (:simple #xcf :operand-mode :32-bit))

(def-instr ret (unconditional-branch)
  (:plain #xC3 (0 0) ())
  (:plain #xC2 (0 2) (imm16)))

(def-instr lret (unconditional-branch)
  (:plain #xCB (0 0) ())
  (:plain #xCA (0 2) (imm16)))

