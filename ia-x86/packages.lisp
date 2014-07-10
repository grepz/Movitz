;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 2000-2004,
;;;;    Department of Computer Science, University of Tromso, Norway
;;;; 
;;;; Filename:      packages.lisp
;;;; Description:   Operand representation.
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Wed Feb 16 14:02:57 2000
;;;; Distribution:  See the accompanying file COPYING.
;;;;                
;;;; $Id: packages.lisp,v 1.4 2004/09/02 09:02:10 ffjeld Exp $
;;;;                
;;;;------------------------------------------------------------------

(in-package #:cl-user)

(defpackage #:ia-x86
  (:use #:common-lisp)
  (:export
   #:init-instruction-tables
   #:*cpu-mode*
   #:*instruction-compute-extra-prefix-map*
   #:decode-sub-stream
   #:decode-read-octet
   #:decode
   #:def-instr
   #:def-set
   ;; operands
   #:r/m8 
   #:r/m16 
   #:r/m32
   #:r8 
   #:r16 
   #:r32
   #:+r8 
   #:+r16 
   #:+r32
   #:pointer
   #:simm8 
   #:imm8 
   #:imm16 
   #:imm32
   #:rel8 
   #:rel16 
   #:rel32
   #:ptr16-16 
   #:ptr16-32
   #:al 
   #:ax 
   #:eax 
   #:cl 
   #:dx 
   #:sreg
   #:cs 
   #:ds 
   #:es 
   #:fs 
   #:gs 
   #:ss
   #:m32real 
   #:m64real 
   #:m80real 
   #:m16int 
   #:m32int
   ;; instruction class
   #:instruction
   #:instruction-operands
   #:instruction-prefixes
   ;; instruction prefixes
   #:lock 
   #:repne 
   #:repz
   #:cs-override 
   #:ds-override 
   #:es-override
   #:fs-override 
   #:gs-override 
   #:ss-override
   #:16-bit-operand
   #:16-bit-address
   #:*symtab-lookup*
   ;; proglist
   #:proglist-encode
   #:make-label
   ;; read
   #:read-instruction
   #:read-proglist
   #:asm
   ;; symtab
   #:symtab-lookup-label
   #:symtab-try-lookup-label
   ))

(defpackage #:ia-x86-instr
  (:use #:ia-x86)
  (:import-from #:common-lisp t nil defclass load eval compile)
  (:documentation
   "All the specialized classes for ia-x86 instructions are put into
this package, so that we may have instructions named 'and' etc."))
