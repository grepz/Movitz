;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 2000,
;;;;    Department of Computer Science, University of Tromso, Norway
;;;; 
;;;; Filename:      ia-x86-instr-mmx.lisp
;;;; Description:   MMX instructions.
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Wed Aug  2 14:26:16 2000
;;;; Distribution:  See the accompanying file COPYING.
;;;;                
;;;; $Id: instr-mmx.lisp,v 1.2 2004/01/16 11:54:14 ffjeld Exp $
;;;;                
;;;;------------------------------------------------------------------

(in-package #:ia-x86-instr)

(def-instr mmx (instruction))

(def-instr emms (mmx) (:simple #x0f77))

;;; MMX Mov

(def-instr movd (mmx)
  (:r #x0f6e (r/m32 mm))
  (:r #x0f7e (mm r/m32)))

(def-instr movq (mmx)
  (:r #x0f6f (mm/m64 mm))
  (:r #x0f7f (mm mm/m64)))

;;; Packing

(def-instr packed (mmx))
(def-instr packsswb (packed) (:r #x0f63 (mm/m64 mm)))
(def-instr packssdw (packed) (:r #x0f6b (mm/m64 mm)))
(def-instr packuswb (packed) (:r #x0f67 (mm/m64 mm)))

;;; Packed add

(def-instr padd (packed))
(def-instr paddb (padd) (:r #x0ffc (mm/m64 mm)))
(def-instr paddw (padd) (:r #x0ffd (mm/m64 mm)))
(def-instr paddd (padd) (:r #x0ffe (mm/m64 mm)))

;;; Packed add with saturation

(def-instr padds (packed))
(def-instr paddsb (padds) (:r #x0fec (mm/m64 mm)))
(def-instr paddsw (padds) (:r #x0fed (mm/m64 mm)))

;;; Packed add unsigned with saturation

(def-instr paddus (packed))
(def-instr paddusb (paddus) (:r #x0fdc (mm/m64 mm)))
(def-instr paddusw (paddus) (:r #x0fdd (mm/m64 mm)))

;;; Bitwise logic

(def-instr pand (mmx) (:r #x0fdb (mm/m64 mm)))	; AND
(def-instr pandn (mmx) (:r #x0fdf (mm/m64 mm)))	; AND NOT: mm := (NOT mm) AND mm/m64

;;; Packed compare for Equal

(def-instr pcmpeq (packed))
(def-instr pcmpeqb (pcmpeq) (:r #x0f74 (mm/m64 mm)))
(def-instr pcmpeqw (pcmpeq) (:r #x0f75 (mm/m64 mm)))
(def-instr pcmpeqd (pcmpeq) (:r #x0f76 (mm/m64 mm)))

;;; Packed compare for Greater Than

(def-instr pcmpgt (packed))
(def-instr pcmpgtb (pcmpgt) (:r #x0f64 (mm/m64 mm)))
(def-instr pcmpgtw (pcmpgt) (:r #x0f65 (mm/m64 mm)))
(def-instr pcmpgtd (pcmpgt) (:r #x0f66 (mm/m64 mm)))

;;; Packed Multiply and Add

(def-instr pmaddwd (mmx) (:r #x0ff5 (mm/m64 mm)))

;;; Packed Mutliply High

(def-instr pmulhw (mmx) (:r #x0fe5 (mm/m64 mm)))

;;; Packed Mutliply Low

(def-instr pmullw (mmx) (:r #x0fd5 (mm/m64 mm)))

;;; Packed Shift Left Logical

(def-instr psll (mmx))
(def-instr psllw (psll)
  (:r #x0ff1 (mm/m64 mm))
  (:digit (#x0f71 6) 1 (imm8 mm/m64) :direct t))

(def-instr pslld (psll)
  (:r #x0ff2 (mm/m64 mm))
  (:digit (#x0f72 6) 1 (imm8 mm/m64) :direct t))

(def-instr psllq (psll)
  (:r #x0ff3 (mm/m64 mm))
  (:digit (#x0f73 6) 1 (imm8 mm/m64) :direct t))

;;; Packed Shift Right Arithmetic

(def-instr psra (mmx))
(def-instr psraw (psra)
  (:r #x0fe1 (mm/m64 mm))
  (:digit (#x0f71 4) 1 (imm8 mm/m64) :direct t))

(def-instr psrad (psra)
  (:r #x0fe2 (mm/m64 mm))
  (:digit (#x0f72 4) 1 (imm8 mm/m64) :direct t))

;;; Packed Shift Right Logical

(def-instr psrl (mmx))
(def-instr psrlw (psrl)
  (:r #x0fd1 (mm/m64 mm))
  (:digit (#x0f71 2) 1 (imm8 mm/m64) :direct t))

(def-instr psrld (psrl)
  (:r #x0fd2 (mm/m64 mm))
  (:digit (#x0f72 2) 1 (imm8 mm/m64) :direct t))

(def-instr psrlq (psrl)
  (:r #x0fd3 (mm/m64 mm))
  (:digit (#x0f73 2) 1 (imm8 mm/m64) :direct t))

;;; Packed Subtract

(def-instr psub (mmx))
(def-instr psubb (psub) (:r #x0ff8 (mm/m64 mm)))
(def-instr psubw (psub) (:r #x0ff9 (mm/m64 mm)))
(def-instr psubd (psub) (:r #x0ffa (mm/m64 mm)))

;;; Packed Subtract With Saturation

(def-instr psubs (mmx))
(def-instr psubsb (psubs) (:r #x0fe8 (mm/m64 mm)))
(def-instr psubsw (psubs) (:r #x0fe9 (mm/m64 mm)))

;;; Packed Subtract Unsigned with Saturation

(def-instr psubus (mmx))
(def-instr psubusb (psubus) (:r #x0fd8 (mm/m64 mm)))
(def-instr psubusw (psubus) (:r #x0fd9 (mm/m64 mm)))

;;; Unpack High Packed Data

(def-instr punpckh (mmx))
(def-instr punpckhbw (punpckh) (:r #x0f68 (mm/m64 mm)))
(def-instr punpckhwd (punpckh) (:r #x0f69 (mm/m64 mm)))
(def-instr punpckhdq (punpckh) (:r #x0f6a (mm/m64 mm)))

;;; Unpack Low Packed Data

(def-instr punpckl (mmx))
(def-instr punpcklbw (punpckl) (:r #x0f60 (mm/m64 mm)))
(def-instr punpcklwd (punpckl) (:r #x0f61 (mm/m64 mm)))
(def-instr punpckldq (punpckl) (:r #x0f62 (mm/m64 mm)))

;;; Logical Exclusive OR

(def-instr pxor (mmx) (:r #x0fef (mm/m64 mm)))

;;;
;;; Extra MMX instructions that came with SIMD
;;;

;;; Packed Average
(def-instr pavgb (mmx) (:r #x0fe0 (mm/m64 mm)))

(def-instr pavgw (mmx) (:r #x0fe3 (mm/m64 mm)))

;;; Extract Word
(def-instr pextrw (mmx) (:r-imm #x0fc5 1 (imm8 mm r/m32) :direct t))

;;; Insert Word
(def-instr pinsrw (mmx) (:r-imm #x0fc4 1 (imm8 r/m32 mm)))

;;; Packed Signed Integer Word Maximum
(def-instr pmaxsw (mmx) (:r #x0fee (mm/m64 mm)))

;;; Packed Unsigned Integer Byte Maximum
(def-instr pmaxub (mmx) (:r #x0fde (mm/m64 mm)))

;;; Packed Signed Integer Word Minimum
(def-instr pminsw (mmx) (:r #x0fea (mm/m64 mm)))

;;; Packed Unsigned Integer Byte Minimum
(def-instr pminub (mmx) (:r #x0fda (mm/m64 mm)))

;;; Move Byte Mask To Integer
(def-instr pmovmskb (mmx) (:r #x0fd7 (mm r/m32) :direct t))

;;; Packed Multiply High Unsigned
(def-instr pmulhuw (mmx) (:r #x0fe4 (mm/m64 mm)))

;;; Packed Sum of Absolute Differences
(def-instr psadbw (mmx) (:r #x0ff6 (mm/m64 mm)))

;;; Packed Shuffle Word
(def-instr pshufw (mmx) (:r-imm #x0f70 1 (imm8 mm/m64 mm)))


;;;
;;; Cacheability Control
;;;

;;; Byte Mask Write
(def-instr maskmovq (mmx) (:r #x0ff7 (mm/m64 mm) :direct t))

;;; Move 64 Bits Non-temporal
(def-instr movntq (mmx) (:r #x0fe7 (mm mm/m64) :indirect t))

