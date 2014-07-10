;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 2000, 2004,
;;;;    Department of Computer Science, University of Tromso, Norway
;;;; 
;;;; Filename:      registers.lisp
;;;; Description:   
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Tue Aug  1 10:24:59 2000
;;;; Distribution:  See the accompanying file COPYING.
;;;;                
;;;; $Id: registers.lisp,v 1.3 2004/02/10 00:04:13 ffjeld Exp $
;;;;                
;;;;------------------------------------------------------------------

(in-package #:ia-x86)

;;; ----------------------------------------------------------------
;;;                    Register sets
;;; ----------------------------------------------------------------

(defvar *register-decoder-sets* (make-hash-table :test #'eq))
(defvar *register-encoder-sets* (make-hash-table :test #'eq))

(defmacro def-register-set (name-list set)
  `(let ((dec-set (make-decoder-set ,@set))
	 (enc-set (make-encoder-set ,@set)))
     (loop for name in ',name-list
	 do (setf (gethash name *register-decoder-sets*) dec-set)
	    (setf (gethash name *register-encoder-sets*) enc-set))))

(defun find-register-decode-set (name)
  (let ((rs (gethash name *register-decoder-sets*)))
    (unless rs (error "Register set ~A not found." name))
    rs))

(defun find-register-encode-set (name)
  (let ((rs (gethash name *register-encoder-sets*)))
    (unless rs (error "Register set ~A not found." name))
    rs))

(def-register-set (r/m32-00)
    ((0 eax) 
     (1 ecx)
     (2 edx)
     (3 ebx)
     (6 esi)
     (7 edi)))

(def-register-set (r/m32-01 sib-index)
    ((0 eax) 
     (1 ecx)
     (2 edx)
     (3 ebx)
     (5 ebp)
     (6 esi)
     (7 edi)))

(def-register-set (sib-base-00)
    ((0 eax) 
     (1 ecx)
     (2 edx)
     (3 ebx)
     (4 esp)
     (6 esi)
     (7 edi)))

(def-register-set (r32 +r32 r/m32 sib-base)
    ((0 eax) 
     (1 ecx)
     (2 edx)
     (3 ebx)
     (4 esp)
     (5 ebp)
     (6 esi)
     (7 edi)))

(def-register-set (r8 +r8 r/m8)
    ((0 al)
     (1 cl)
     (2 dl)
     (3 bl)
     (4 ah)
     (5 ch)
     (6 dh)
     (7 bh)))

(def-register-set (r16 +r16 r/m16)
    ((0 ax)
     (1 cx)
     (2 dx)
     (3 bx)
     (4 sp)
     (5 bp)
     (6 si)
     (7 di)))

(def-register-set (sreg)		; page 11-9 in IISR
    ((0 es)
     (1 cs)
     (2 ss)
     (3 ds)
     (4 fs)
     (5 gs)))

(def-register-set (r/m-16bit)
    ((0 (bx . si))
     (1 (bx . di))
     (2 (bp . si))
     (3 (bp . di))
     (4 (si . nil))
     (5 (di . nil))
     (6 (bp . nil))
     (7 (bx . nil))))

(def-register-set (mm mm/m64)
    ((0 mm0)
     (1 mm1)
     (2 mm2)
     (3 mm3)
     (4 mm4)
     (5 mm5)
     (6 mm6)
     (7 mm7)))

(def-register-set (xmm xmm/m128 xmm/m64 xmm/m32)
    ((0 xmm0)
     (1 xmm1)
     (2 xmm2)
     (3 xmm3)
     (4 xmm4)
     (5 xmm5)
     (6 xmm6)
     (7 xmm7)))

