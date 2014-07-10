;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 20012000, 2003-2004,
;;;;    Department of Computer Science, University of Tromso, Norway
;;;; 
;;;; Filename:      prefixes.lisp
;;;; Description:   X86 Instruction prefixes.
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Tue Aug 15 22:34:30 2000
;;;; Distribution:  See the accompanying file COPYING.
;;;;                
;;;; $Id: prefixes.lisp,v 1.3 2004/02/10 00:03:56 ffjeld Exp $
;;;;                
;;;;------------------------------------------------------------------

(in-package #:ia-x86)

;;; ----------------------------------------------------------------
;;;                   Instruction prefixes
;;; ----------------------------------------------------------------

(defparameter +opcode-prefix-map-lock-and-repeat+
    (make-decoder-set (#xf0 lock)
		      (#xf2 repne)
		      (#xf3 repz)))

(defparameter +prefix-opcode-map-lock-and-repeat+
    (make-encoder-set (#xf0 lock)
		      (#xf2 repne)
		      (#xf3 repz)))

(defparameter +opcode-prefix-map-segment-override+
    (make-decoder-set (#x2e cs-override) (#x36 ss-override)
		      (#x3e ds-override) (#x26 es-override)
		      (#x64 fs-override) (#x65 gs-override)))

(defparameter +prefix-opcode-map-segment-override+
    (make-encoder-set (#x2e cs-override) (#x36 ss-override)
		      (#x3e ds-override) (#x26 es-override)
		      (#x64 fs-override) (#x65 gs-override)))

(defparameter +opcode-prefix-map-operand-size-override+
    (make-decoder-set (#x66 16-bit-operand)))

(defparameter +prefix-opcode-map-operand-size-override+
    (make-encoder-set (#x66 16-bit-operand)))

(defparameter +opcode-prefix-map-address-size-override+
    (make-decoder-set (#x67 16-bit-address)))

(defparameter +prefix-opcode-map-address-size-override+
    (make-encoder-set (#x67 16-bit-address)))

(defparameter +opcode-prefix-map+
    (append +opcode-prefix-map-lock-and-repeat+
	    +opcode-prefix-map-segment-override+
	    +opcode-prefix-map-operand-size-override+
	    +opcode-prefix-map-address-size-override+))

(defparameter +prefix-opcode-map+
    (append +prefix-opcode-map-lock-and-repeat+
	    +prefix-opcode-map-segment-override+
	    +prefix-opcode-map-operand-size-override+
	    +prefix-opcode-map-address-size-override+))

(defun prefix-symbol (opcode)
  (cdr (assoc opcode +opcode-prefix-map+)))
