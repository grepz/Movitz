;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 2003-2004, 
;;;;    Department of Computer Science, University of Tromsoe, Norway.
;;;; 
;;;;    For distribution policy, see the accompanying file COPYING.
;;;; 
;;;; Filename:      load.lisp
;;;; Description:   
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Thu Jan 15 15:42:34 2004
;;;;                
;;;; $Id: load.lisp,v 1.1 2004/01/15 17:39:18 ffjeld Exp $
;;;;                
;;;;------------------------------------------------------------------


#+allegro (progn
	    (load "system.lisp")
	    (compile-system :ia-x86)
	    (load-system :ia-x86)
	    (load-system :ia-x86-instr :interpreted t))

#-allegro (progn
	    (with-compilation-unit ()
	      (mapcar (lambda (path)
			(load (compile-file path :print nil)))
		      '("packages"
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
			"postload")))
	    (mapcar 'load
		    '("instr-mov"
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

(ia-x86:init-instruction-tables)

