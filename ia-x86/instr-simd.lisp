;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 2000,
;;;;    Department of Computer Science, University of Tromso, Norway
;;;; 
;;;; Filename:      ia-x86-instr-simd.lisp
;;;; Description:   Streaming SIMD instructions.
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Thu Aug  3 18:15:02 2000
;;;; Distribution:  See the accompanying file COPYING.
;;;;                
;;;; $Id: instr-simd.lisp,v 1.2 2004/01/16 11:54:14 ffjeld Exp $
;;;;                
;;;;------------------------------------------------------------------

(in-package #:ia-x86-instr)

(def-instr simd (instruction))


;;; Packed Single-FP Add
(def-instr addps (simd) (:r #x0f58 (xmm/m128 xmm) :not-prefixes (repz)))

;;; Scalar Single-FP Add
(def-instr addss (simd) (:r #x0f58 (xmm/m32 xmm) :req-prefixes (repz)))

;;; Bit-wise Logical And Not for Single-FP
(def-instr andnps (simd) (:r #x0f55 (xmm/m128 xmm)))

;;; Bit-wise Logical And for Single-FP
(def-instr andps (simd) (:r #x0f54 (xmm/m128 xmm)))

;;; Packed Single-FP Compare
					; this is one fucked up instruction!
(def-instr cmpps (simd) (:r-imm #x0fc2 1 (imm8 xmm/m128 xmm) :not-prefixes (repz)))

;;; Scalar Single-FP Compare
(def-instr cmpss (simd) (:r-imm #x0fc2 1 (imm8 xmm/m128 xmm) :req-prefixes (repz)))

;;; Scalar Ordered Single-FP Comare and set EFLAGS
(def-instr comiss (simd) (:r #x0f2f (xmm/m32 xmm)))

;;; Packed Signed INT32 to Packed Single-FP Conversion
(def-instr cvtpi2ps (simd) (:r #x0f2a (xmm/m64 xmm) :not-prefixes (repz)))

;;; Scalar signed INT32 to Single-FP Conversion
(def-instr cvtsi2ss (simd) (:r #x0f2a (r/m32 xmm) :req-prefixes (repz)))

;;; Packed Single-FP to Packed INT32 Conversion
(def-instr cvtps2pi (simd) (:r #x0f2d (xmm/m64 xmm) :not-prefixes (repz)))

;;; Scalar Single-FP to Signed INT32 Conversion
(def-instr cvtss2si (simd) (:r #x0f2d (xmm/m32 r32) :req-prefixes (repz)))

;;; Packed Single-FP to Packed INT32 Conversion (truncate)
(def-instr cvttps2pi (simd) (:r #x0f2c (xmm/m64 mm) :not-prefixes (repz)))

;;; Scalar Single-FP to signed INT32 Conversion (truncate)
(def-instr cvttss2si (simd) (:r #x0f2c (xmm/m32 r32) :req-prefixes (repz)))

;;; Packed Single-FP Divide
(def-instr divps (simd) (:r #x0f5e (xmm/m128 xmm) :not-prefixes (repz)))

;;; Scalar Single-FP Divide
(def-instr divss (simd) (:r #x0f5e (xmm/m32 xmm) :req-prefixes (repz)))

;;; Restore FP and MMX state and Streaming SIMD Extension State
(def-instr fxrstor (simd) (:digit (#x0fae 1) 0 (m) :indirect t))

;;; Store FP and MMX State and Streaming SIMD Extension State
(def-instr fxsave (simd) (:digit (#x0fae 0) 0 (m) :indirect t))

;;; Load Streaming SIMD Extension Control/Status
(def-instr ldmxcsr (simd) (:digit (#x0fae 2) 0 (m) :indirect t))

;;; Packed Single-FP Maximum
(def-instr maxps (simd) (:r #x0f5f (xmm/m128 xmm) :not-prefixes (repz)))

;;; Scalar Single-FP Maximum
(def-instr maxss (simd) (:r #x0f5f (xmm/m32 xmm) :req-prefixes (repz)))

;;; Packed Single-FP Minimum
(def-instr minps (simd) (:r #x0f5d (xmm/m128 xmm) :not-prefixes (repz)))

;;; Scalar Single-FP Minimum
(def-instr minss (simd) (:r #x0f5d (xmm/m32 xmm) :req-prefixes (repz)))

;;; Move Aligned Four Packed Single-FP
(def-instr movaps (simd)
  (:r #x0f28 (xmm/m128 xmm))
  (:r #x0f29 (xmm xmm/m128)))

;;; Move High to Low Packed Single-FP
(def-instr movhlps (simd) (:r #x0f12 (xmm/m128 xmm) :direct t))

;;; Move High Packed Single-FP
(def-instr movhps (simd)
  (:r #x0f16 (xmm/m64 xmm) :indirect t)
  (:r #x0f17 (xmm xmm/m64) :indirect t))

;;; Move Low to High Packed Single-FP
(def-instr movlhps (simd)
  (:r #x0f16 (xmm/m64 xmm) :direct t))

;;; Move Low Packed Single-FP
(def-instr movlps (simd)
  (:r #x0f12 (xmm/m64 xmm) :indirect t)
  (:r #x0f13 (xmm xmm/m64) :indirect t))

;;; Move Mask to Integer
(def-instr movmskps (simd) (:r #x0f50 (xmm r/m32) :direct t))

;;; Move Scalar Single-FP
(def-instr movss (simd)
  (:r #x0f10 (xmm/m32 xmm) :req-prefixes (repz))
  (:r #x0f11 (xmm xmm/m32) :req-prefixes (repz)))

;;; Move Unaligned Four Packed Single-FP
(def-instr movups (simd)
  (:r #x0f10 (xmm/m128 xmm) :not-prefixes (repz))
  (:r #x0f11 (xmm xmm/m128) :not-prefixes (repz)))

;;; Packed Single-FP Multiply
(def-instr mulps (simd) (:r #x0f59 (xmm/m128 xmm) :not-prefixes (repz)))

;;; Scalar Single-FP Multiply
(def-instr mulss (simd) (:r #x0f59 (xmm/m32 xmm) :req-prefixes (repz)))

;;; Bit-wise Logical OR for Single-FP Data
(def-instr orps (simd) (:r #x0f56 (xmm/m128 xmm)))

;;; Packed Single-FP Reciprocal
(def-instr rcpps (simd) (:r #x0f53 (xmm/m128 xmm) :not-prefixes (repz)))

;;; Scalar Single-FP Reciprocal
(def-instr rcpss (simd) (:r #x0f53 (xmm/m32 xmm) :req-prefixes (repz)))

;;; Packed Single-FP Square Root Reciprocal
(def-instr rsqrtps (simd) (:r #x0f52 (xmm/m128 xmm) :not-prefixes (repz)))

;;; Scalar Single-FP Square Root Reciprocal
(def-instr rsqrtss (simd) (:r #x0f52 (xmm/m32 xmm) :req-prefixes (repz)))

;;; Suffle Single-FP
(def-instr shufps (simd) (:r-imm #x0fc6 1 (imm8 xmm/m128 xmm)))

;;; Packed Single-FP Square Root
(def-instr sqrtps (simd) (:r #x0f51 (xmm/m128 xmm) :not-prefixes (repz)))

;;; Scalar Single-FP Square Root
(def-instr sqrtss (simd) (:r #x0f51 (xmm/m32 xmm) :req-prefixes (repz)))

;;; Store Streaming SIMD Extension Control/Status
(def-instr stmxcsr (simd) (:digit (#x0fae 3) 0 (r/m32) :indirect t))

;;; Packed Single-FP Subtract
(def-instr subps (simd) (:r #x0f5c (xmm/m128 xmm) :not-prefixes (repz)))

;;; Scalar Single-FP Subtract
(def-instr subss (simd) (:r #x0f5c (xmm/m128 xmm) :req-prefixes (repz)))

;;; Unordered Scalar Single-FP Compare and Set EFLAGS
(def-instr ucomiss (simd) (:r #x0f2e (xmm/m32 xmm)))

;;; Unpack High Packed Single-FP Data
(def-instr unpckhps (simd) (:r #x0f15 (xmm/m128 xmm)))

;;; Unpack Low Packed Single-FP Data
(def-instr unpcklps (simd) (:r #x0f14 (xmm/m128 xmm)))

;;; Bit-wise Logical XOR for Single-FP Data
(def-instr xorps (simd) (:r #x0f57 (xmm/m128 xmm)))


;;;
;;; Cacheability Control
;;;


;;; Move Aligned Four Packed Single-FP Non-temporal
(def-instr movntps (simd) (:r #x0f2b (xmm xmm/m128) :indirect t))

