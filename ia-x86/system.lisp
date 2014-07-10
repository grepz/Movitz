;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Copyright (C) 19991998,
;;    Department of Computer Science, University of Tromso, Norway
;; 
;; Filename:      Defsystem.lisp
;; Description:   ACL defsystem calls relates files to eachother.
;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;; Created at:    Wed Oct 20 23:13:46 1999
;;                
;; $Id: system.lisp,v 1.2 2004/01/16 11:54:14 ffjeld Exp $
;;                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#-allegro (error "This system definition is Allegro CL only. ~
              See ia-x86.asd for an ASDF-defsystem.")

(in-package "USER")

(defsystem :ia-x86 ()
  (:serial "packages"
	   "ia-x86"
	   "symtab"
	   "prefixes"
	   "registers"
	   "operands"
	   "inline-data"
	   "alignment"
	   "read"
	   "codec"
	   "proglist"
	   "def-instr"
	   "postload"))

(defsystem :ia-x86-instr ()
  (:definitions :ia-x86
      (:serial
       "instr-mov"
       "instr-add"
       "instr-sub"
       "instr-cmp"
       "instr-push-pop"
       "instr-mul-div"
       "instr-branch"
       "instr-and-or"
       "instr-bit"
       "instr-shift"
       "instr-string"
       "instr-misc"
       "instr-fpu"
       "instr-mmx"
       "instr-simd")))

