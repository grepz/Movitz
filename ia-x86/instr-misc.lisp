;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 20012000, 2002-2004,
;;;;    Department of Computer Science, University of Tromso, Norway
;;;; 
;;;; Filename:      instr-misc.lisp
;;;; Description:   Miscellaneous x86 instructions.
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Fri Jan 28 19:25:27 2000
;;;; Distribution:  See the accompanying file COPYING.
;;;;                
;;;; $Id: instr-misc.lisp,v 1.2 2004/01/16 11:54:14 ffjeld Exp $
;;;;                
;;;;------------------------------------------------------------------

(in-package "IA-X86-INSTR")

;;; ----------------------------------------------------------------
;;;                     LEA    [IISR page 11-255]
;;; ----------------------------------------------------------------

(def-instr lea (instruction))
(def-instr leal (lea)
  (:r #x8d (r/m32 r32) :operand-mode :32-bit)
  (:r #x8d (r/m16 r32) :operand-mode :32-bit))
(def-instr leaw (lea)
  (:r #x8d (r/m16 r16) :operand-mode :16-bit)
  (:r #x8d (r/m32 r16) :operand-mode :16-bit))

;;; ----------------------------------------------------------------
;;;            LDS/LES/LFS/LGS/LSS  --  Load Far Pointer
;;; ----------------------------------------------------------------

(def-instr lds (instruction))
(def-instr ldsw (lds) (:r #xC5 (r16 r/m16) :indirect t :operand-mode :16-bit))
(def-instr ldsl (lds) (:r #xC5 (r32 r/m32) :indirect t :operand-mode :32-bit))

(def-instr les (instruction))
(def-instr lesw (les) (:r #xC4 (r16 r/m16) :indirect t :operand-mode :16-bit))
(def-instr lesl (les) (:r #xC4 (r32 r/m32) :indirect t :operand-mode :32-bit))

(def-instr lss (instruction))
(def-instr lssw (lss) (:r #c(#x0fb2 2) (r16 r/m16) :indirect t :operand-mode :16-bit))
(def-instr lssl (lss) (:r #c(#x0fb2 2) (r32 r/m32) :indirect t :operand-mode :32-bit))

(def-instr lfs (instruction))
(def-instr lfsw (lfs) (:r #c(#x0fb4 2) (r16 r/m16) :indirect t :operand-mode :16-bit))
(def-instr lfsl (lfs) (:r #c(#x0fb4 2) (r32 r/m32) :indirect t :operand-mode :32-bit))

(def-instr lgs (instruction))
(def-instr lgsw (lgs) (:r #c(#x0fb5 2) (r16 r/m16) :indirect t :operand-mode :16-bit))
(def-instr lgsl (lgs) (:r #c(#x0fb5 2) (r32 r/m32) :indirect t :operand-mode :32-bit))

;;; ----------------------------------------------------------------
;;;                     XCHG
;;; ----------------------------------------------------------------

(def-instr xchg (instruction))

(def-instr xchgb (xchg)
  (:r #x86 (r8 r/m8))
  (:r #x86 (r/m8 r8)))

(def-instr xchgw (xchg)
  (:+ #x90 0 (ax +r16) :operand-mode :16-bit)
  (:+ #x90 0 (+r16 ax) :operand-mode :16-bit)
  (:r #x87 (r/m16 r16) :operand-mode :16-bit)
  (:r #x87 (r16 r/m16) :operand-mode :16-bit))

(def-instr xchgl (xchg)
  (:+ #x90 0 (eax +r32) :operand-mode :32-bit)
  (:+ #x90 0 (+r32 eax) :operand-mode :32-bit)
  (:r #x87 (r/m32 r32) :operand-mode :32-bit)
  (:r #x87 (r32 r/m32) :operand-mode :32-bit))

;;; ----------------------------------------------------------------
;;;                  INTn [IISR page 11-216]
;;; ----------------------------------------------------------------

(def-instr break (instruction) (:simple #xcc))
(def-instr into (instruction) (:simple #xce))
(def-instr int (instruction) (:plain #xcd (0 1) (imm8)))

;;; ----------------------------------------------------------------
;;;                  In (Input from IO port)
;;; ----------------------------------------------------------------

(def-instr in (instruction))
(def-instr inb (in)
  (:plain #xE4 (0 1) (imm8 al))
  (:plain #xEC (0 0) (dx al)))

(def-instr inw (in)
  (:plain #xE5 (0 1) (imm8 ax) :operand-mode :16-bit)
  (:plain #xED (0 0) (dx ax) :operand-mode :16-bit))

(def-instr inl (in)
  (:plain #xE5 (0 1) (imm8 eax) :operand-mode :32-bit)
  (:plain #xED (0 0) (dx eax) :operand-mode :32-bit))

(def-instr insb (instruction) (:simple #x6c))
(def-instr insw (instruction) (:simple #x6d :operand-mode :16-bit))
(def-instr insd (instruction) (:simple #x6d :operand-mode :32-bit))

;;; ----------------------------------------------------------------
;;;                  Out (Output to IO port)
;;; ----------------------------------------------------------------

(def-instr out (instruction))
(def-instr outb (out)
  (:plain #xE6 (0 1) (al imm8))
  (:plain #xEE (0 0) (al dx)))

(def-instr outw (out)
  (:plain #xE7 (0 1) (ax imm8) :operand-mode :16-bit)
  (:plain #xEF (0 0) (ax dx) :operand-mode :16-bit))

(def-instr outl (out)
  (:plain #xE7 (0 1) (eax imm8) :operand-mode :32-bit)
  (:plain #xEF (0 0) (eax dx) :operand-mode :32-bit))

(def-instr outsb (instruction) (:simple #x6e))
(def-instr outsw (instruction) (:simple #x6f :operand-mode :16-bit))
(def-instr outsd (instruction) (:simple #x6f :operand-mode :32-bit))

;;; ----------------------------------------------------------------
;;;                  ASCII related instructions
;;; ----------------------------------------------------------------

(def-instr aaa (instruction) (:simple #x37))
(def-instr aas (instruction) (:simple #x3f))

(def-instr aad (instruction)
  (:plain #xd5 (0 1) (imm8 ax))
  (:simple #c(#xd50a 2) :priority -5))	; imm8 defaults to 10.
(def-instr aam (instruction)
  (:plain #xd4 (0 1) (imm8 ax))
  (:simple #c(#xd40a 2) :priority -5))	; imm8 defaults to 10.

;;; ----------------------------------------------------------------
;;;                  Decimal related instructions
;;; ----------------------------------------------------------------

(def-instr daa (instruction) (:simple #x27))
(def-instr das (instruction) (:simple #x2f))

;;; ----------------------------------------------------------------
;;;                  Various simple instructions
;;; ----------------------------------------------------------------

(def-instr clc (instruction) (:simple #xF8)) ; clear carry flag
(def-instr stc (instruction) (:simple #xF9)) ; set carry flag
(def-instr cld (instruction) (:simple #xFC)) ; clear direction flag
(def-instr std (instruction) (:simple #xFD)) ; set direction flag
(def-instr cli (instruction) (:simple #xFA)) ; clear interrupt flag
(def-instr sti (instruction) (:simple #xFB)) ; set interrupt flag
(def-instr cmc (instruction) (:simple #xf5)) ; complement carry flag
(def-instr clts (instruction) (:simple #c(#x0f06 2))) ; clear task-switched flag in CR0
(def-instr lahf (instruction) (:simple #x9f :operands (ah))) ; load status flags into %AH
(def-instr cpuid (instruction) (:simple #c(#x0fa2 2))) ; CPU identification

(def-instr halt (instruction) (:simple #xf4 :priority 10))
(def-instr nop (instruction) (:simple #x90 :priority 10)) ; needs priority over XCHG %EAX %EAX

(def-instr invd (instruction) (:simple #c(#x0f08 2))) ; invalidate internal caches

;;; ----------------------------------------------------------------
;;;                  XXX
;;; ----------------------------------------------------------------

(def-instr arpl (instruction) (:r #x63 (r16 r/m16)))

(def-instr bound (instruction)
  (:r #x62 (r/m16 r16) :operand-mode :16-bit)
  (:r #x62 (r/m32 r32) :operand-mode :32-bit))

(def-instr sahf (instruction) (:simple #x9E :operands (ah))) ; Load SF, ZF, AF, PF, CF from AH into EFLAGS

(def-instr xlatb (instruction) (:simple #xD7 :operands (ebx al al))) ; Exchange AL with [EBX + AL]
	       			 

(def-instr cbtw (instruction)
  (:simple #x98 :operands (al ax) :operand-mode :16-bit)) ; sign-extend al into ah (ax)
(def-instr cwtl (instruction)
  (:simple #x98 :operands (ax eax) :operand-mode :32-bit)) ; sign-extend ax into eax
(def-instr cwd (instruction)
  (:simple #x99 :operands (ax dx) :operand-mode :16-bit)) ; sign-extend ax into dx
(def-instr cdq (instruction)
  (:simple #x99 :operands (eax edx) :operand-mode :32-bit)) ; sign-extend eax into edx

(def-instr bswap (instruction) (:+ #c(#x0fc8 2) 0 (+r32)))


;;; Store Local Descriptor Table Register

(def-instr sldt (instruction)
  (:digit (#x0f00 0) 0 (r/m16) :operand-mode :16-bit)
  (:digit (#x0f00 0) 0 (r/m32) :operand-mode :32-bit))

;;; Store Global/Interrupt Descriptor Table Register

(def-instr sgdt (instruction)
  (:digit (#x0f01 0) 0 (m) :indirect t))

(def-instr sidt (instruction)
  (:digit (#x0f01 1) 0 (m) :indirect t))

;;; Load Access Rights Byte

(def-instr lar (instruction)
  (:r #x0f02 (r/m16 r16) :operand-mode :16-bit)
  (:r #x0f02 (r/m32 r32) :operand-mode :32-bit))

;;; Load Segment Limit

(def-instr lsl (instruction)
  (:r #x0f03 (r/m16 r16) :operand-mode :16-bit)
  (:r #x0f03 (r/m32 r32) :operand-mode :32-bit))

;;; Write-Back and Invalidate Cache

(def-instr wbinvd (instruction) (:simple #x0f09))

;;; Write to Model Specific Register

(def-instr wrmsr (instruction) (:simple #x0f30)) ; Write EDX:EAX to MSR in ECX

;;; Read from Model Specific Register

(def-instr rdmsr (instruction) (:simple #x0f32)) ; Read from MSR in ECX into EDX:EAX

;;; Read Time-Stamp Counter

(def-instr rdtsc (instruction) (:simple #x0f31)) ; Read into EDX:EAX

;;; Read Performance-Monitoring Counters

(def-instr rdpmc (instruction) (:simple #x0f33)) ; Read counter in ECX into EDX:EAX

;;; Fast Transition to/from System Call

(def-instr sysenter (instruction) (:simple #x0f34))
(def-instr sysexit (instruction) (:simple #x0f35))

;;; Load Global/Interrupt Descriptor Table Register

(def-instr lgdt (instruction) (:digit (#x0f01 2) 0 (r/m32) :indirect t))

(def-instr lidt (instruction) (:digit (#x0f01 3) 0 (r/m32)))

;;; Load Local Descriptor Table Register

(def-instr lldt (instruction) (:digit (#x0f00 2) 0 (r/m16)))

;;; Load Machine Status Word
(def-instr lmsw (instruction) (:digit (#x0f01 6) 0 (r/m16)))


;;; ----------------------------------------------------------------
;;;                 SETcc  --  Set Byte on Condition
;;; ----------------------------------------------------------------

(def-instr set (instruction))

(def-instr seta   (set) (:set #x0f97))			; if above
(def-instr setae  (set) (:set #x0f93 :priority 10))	; if above or equal
(def-instr setb   (set) (:set #x0f92 :priority 10))	; if below
(def-instr setbe  (set) (:set #x0f96))			; if below or equal
(def-instr setc   (set) (:set #x0f92 :priority 10))	; if carry
(def-instr sete   (set) (:set #x0f94))			; if equal
(def-instr setg   (set) (:set #x0f9f :priority 10))	; if greater
(def-instr setge  (set) (:set #x0f9d :priority 10))	; if greater or equal
(def-instr setl   (set) (:set #x0f9c))			; if less
(def-instr setle  (set) (:set #x0f9e))			; if less or equal
(def-instr setna  (set) (:set #x0f96))			; if not above
(def-instr setnae (set) (:set #x0f92))			; if not above or equal
(def-instr setnb  (set) (:set #x0f93))			; if not below
(def-instr setnbe (set) (:set #x0f97))			; if not below or equal
(def-instr setnc  (set) (:set #x0f93 :priority 20))	; if not carry
(def-instr setne  (set) (:set #x0f95 :priority 10))	; if not equal
(def-instr setng  (set) (:set #x0f9e))			; if not greater
(def-instr setnge (set) (:set #x0f9c))			; if not greater or equal
(def-instr setnl  (set) (:set #x0f9d))			; if not less
(def-instr setnle (set) (:set #x0f9f))			; if not less or equal
(def-instr setno  (set) (:set #x0f91))			; if not overflow
(def-instr setnp  (set) (:set #x0f9b))			; if not parity
(def-instr setns  (set) (:set #x0f99))			; if not sign
(def-instr setnz  (set) (:set #x0f95))			; if not zero
(def-instr seto   (set) (:set #x0f90))			; if overflow
(def-instr setp   (set) (:set #x0f9a))			; if parity
(def-instr setpe  (set) (:set #x0f9a :priority 10))	; if parity even
(def-instr setpo  (set) (:set #x0f9b :priority 10))	; if parity odd
(def-instr sets   (set) (:set #x0f98))			; if sign
(def-instr setz   (set) (:set #x0f94))			; if zero


;;; ----------------------------------------------------------------
;;;                             PREFETCH
;;; ----------------------------------------------------------------

(def-instr prefetch-t0 (instruction)	; t0 hint
  (:digit (#x0f18 1) 0 (r/m8) :indirect t))

(def-instr prefetch-t1 (instruction)	; t1 hint
  (:digit (#x0f18 2) 0 (r/m8) :indirect t))

(def-instr prefetch-t2 (instruction)	; t2 hint
  (:digit (#x0f18 3) 0 (r/m8) :indirect t))

(def-instr prefetch-nta (instruction)	; nta hint
  (:digit (#x0f18 0) 0 (r/m8) :indirect t))


;;; Store Fence

(def-instr sfence (instruction) (:simple #c(#x0faef8 3)))

;;;

