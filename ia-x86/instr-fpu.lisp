;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 2000,
;;;;    Department of Computer Science, University of Tromso, Norway
;;;; 
;;;; Filename:      ia-x86-instr-fpu.lisp
;;;; Description:   FPU operations
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Tue May  2 10:06:12 2000
;;;; Distribution:  See the accompanying file COPYING.
;;;;                
;;;; $Id: instr-fpu.lisp,v 1.2 2004/01/16 11:54:14 ffjeld Exp $
;;;;                
;;;;------------------------------------------------------------------

(in-package "IA-X86-INSTR")

(def-instr fp (instruction))

;;; ----------------------------------------------------------------
;;;                   FLD  (Load Real)
;;; ----------------------------------------------------------------

(def-instr fld (fp))
(def-instr fld-m32 (fld) (:digit (#xd9 0) 0 (m32real) :indirect t))
(def-instr fld-m64 (fld) (:digit (#xdd 0) 0 (m64real) :indirect t))
(def-instr fld-m80 (fld) (:digit (#xdb 5) 0 (m80real) :indirect t))

;;; Load constants..
(def-instr fld1 (fp) (:simple-fp #c(#xd9e8 2))) ; push +1.0
(def-instr fldl2t (fp) (:simple-fp #c(#xd9e9 2))) ; push log_2(10)
(def-instr fldl2e (fp) (:simple-fp #c(#xd9ea 2))) ; push log_2(e)
(def-instr fldpi (fp) (:simple-fp #c(#xd9eb 2))) ; push PI
(def-instr fldlg2 (fp) (:simple-fp #c(#xd9ec 2))) ; push log_10(2)
(def-instr fldln2 (fp) (:simple-fp #c(#xd9ed 2))) ; push log_e(2)
(def-instr fldz (fp) (:simple-fp #c(#xd9ee 2))) ; push +0.0

;;; ----------------------------------------------------------------
;;;                   FST (Store Real)
;;; ----------------------------------------------------------------

(def-instr fst (fp)
  (:digit (#xdd 3) 0 (m32real) :indirect t))

;;; ----------------------------------------------------------------
;;;                   FADD (Addition)
;;; ----------------------------------------------------------------

(def-instr fadd (fp))
(def-instr fadd-m32 (fadd) (:digit (#xD8 0) 0 (m32real) :indirect t))
(def-instr fadd-m64 (fadd) (:digit (#xDC 0) 0 (m64real) :indirect t))

;;; ----------------------------------------------------------------
;;;                   FSUB (Subtraction)
;;; ----------------------------------------------------------------

(def-instr fsub (fp))
(def-instr fsub-m32 (fsub) (:digit (#xD8 4) 0 (m32real) :indirect t))
(def-instr fsub-m64 (fsub) (:digit (#xDC 4) 0 (m64real) :indirect t))

;;; ----------------------------------------------------------------
;;;                   FSUBR (Reverse subtraction)
;;; ----------------------------------------------------------------

(def-instr fsubr (fp))
(def-instr fsubr-m32 (fsubr) (:digit (#xD8 5) 0 (m32real) :indirect t))
(def-instr fsubr-m64 (fsubr) (:digit (#xDC 5) 0 (m64real) :indirect t))

;;; ----------------------------------------------------------------
;;;                   FDIV (Division)
;;; ----------------------------------------------------------------

(def-instr fdiv (fp))
(def-instr fdiv-m32 (fdiv) (:digit (#xD8 6) 0 (m32real) :indirect t))
(def-instr fdiv-m64 (fdiv) (:digit (#xDC 6) 0 (m64real) :indirect t))
(def-instr fdivp (fdiv) (:simple #c(#xDEF9 2)))

(def-instr fdivr (fdiv))
(def-instr fdivr-m32 (fdivr) (:digit (#xDA 7) 0 (m32int) :indirect t))
(def-instr fdivr-m16 (fdivr) (:digit (#xDE 7) 0 (m16int) :indirect t))

;;; ----------------------------------------------------------------
;;;                  FCOM (Compare Real)
;;; ----------------------------------------------------------------

(def-instr fcom (fp) (:digit (#xd8 2) 0 (m32real) :indirect t))
(def-instr fcomp (fp) (:digit (#xdc 3) 0 (m64real) :indirect t))

;;; ----------------------------------------------------------------
;;;                  FICOM (Compare FP Integer)
;;; ----------------------------------------------------------------

(def-instr ficom (fp))
(def-instr ficom-m16 (ficom) (:digit (#xDE 2) 0 (m16int) :indirect t))
(def-instr ficom-m32 (ficom) (:digit (#xDA 2) 0 (m32int) :indirect t))

(def-instr ficomp (fp))
(def-instr ficomp-m16 (ficomp) (:digit (#xDE 3) 0 (m16int) :indirect t))
(def-instr ficomp-m32 (ficomp) (:digit (#xDA 3) 0 (m32int) :indirect t))

;;; ----------------------------------------------------------------
;;;                  FUCOM (Unordered Compare Real)
;;; ----------------------------------------------------------------

(def-instr fucompp (fp) (:plain #c(#xdae9 2) (0 0) ()))

;;; ----------------------------------------------------------------
;;;                  FSTSW (Store Status Word)
;;; ----------------------------------------------------------------

(def-instr fnstsw (fp) (:plain #c(#xdfe0 2) (0 0) ()))

;;; ----------------------------------------------------------------
;;;                  Trigonometrics
;;; ----------------------------------------------------------------

(def-instr fsin (fp) (:simple-fp #c(#xD9FE 2)))	; Compute SIN(ST(0))
(def-instr fcos (fp) (:simple-fp #c(#xD9FF 2)))	; Compute COS(ST(0))
(def-instr fsincos (fp) (:simple-fp #c(#xD9FB 2))); Compute SIN and COS
(def-instr fpatan (fp) (:simple-fp #c(#xD9F3 2))) ; Partial ArcTan(ST(1)/ST(0))

;;; ----------------------------------------------------------------
;;;                      Miscellaneous FP
;;; ----------------------------------------------------------------

(def-instr f2xm1 (fp) (:simple-fp  #c(#xD9F0 2))) ; Compute 2^ST(0) - 1
(def-instr fabs (fp) (:simple-fp  #c(#xD9E1 2))) ; ABS(ST(0)))
(def-instr fchs (fp) (:simple-fp  #c(#xD9E0 2))) ; Negate ST(0)
(def-instr finit (fp) (:simple-fp #c(#x9BDBE3 3))) ; Init FPU
(def-instr fninit (fp) (:simple-fp #c(#xDBE3 2))) ; Init FPU, no check
(def-instr fclex (fp) (:simple-fp #c(#x9BDBE2 3))) ; Clear exceptions
(def-instr fnclex (fp) (:simple-fp #c(#xDBE2 2))) ; Clear exceptions, no check
(def-instr fdecstp (fp) (:simple-fp #c(#xD9F6 2))) ; Decrement stack-top pointer
(def-instr fincstp (fp) (:simple-fp #c(#xD9F7 2))) ; Increment stack-top pointer
(def-instr fnop (fp) (:simple-fp #c(#xD9D0 2)))	; FPU no operation
(def-instr fprem (fp) (:simple-fp #c(#xD9F8 2))) ; Remainder of ST(0)/ST(1)
(def-instr fprem1 (fp) (:simple-fp #c(#xD9F5 2))) ; IEEE Remainder of ST(0)/ST(1)
(def-instr fscale (fp) (:simple-fp #c(#xD9FD 2))) ; Scale ST(0) by ST(1)
(def-instr fsqrt (fp) (:simple-fp #c(#xD9FA 2))) ; Replace ST(0) by its square root
(def-instr ftst (fp) (:simple-fp #c(#xD9E4 2)))	; Compare ST(0) with 0.0
(def-instr fxam (fp) (:simple-fp #c(#xD9E5 2)))	; Classify ST(0)
(def-instr fxtract (fp) (:simple-fp #c(#xD9F4 2))) ; Get exp. and significand of ST(0)
(def-instr fyl2x (fp) (:simple-fp #c(#xD9F1 2))) ; Compute y*log_2(x)
(def-instr fyl2xp1 (fp) (:simple-fp #c(#xD9F9 2))) ; Compute y*log_2(x+1)


