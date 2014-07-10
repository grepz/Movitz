;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 2000,
;;;;    Department of Computer Science, University of Tromso, Norway
;;;; 
;;;; Filename:      ia-x86-instr-string.lisp
;;;; Description:   
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Tue May  2 11:20:22 2000
;;;; Distribution:  See the accompanying file COPYING.
;;;;                
;;;; $Id: instr-string.lisp,v 1.2 2004/01/16 11:54:14 ffjeld Exp $
;;;;                
;;;;------------------------------------------------------------------

(in-package "IA-X86-INSTR")

;;; ----------------------------------------------------------------
;;;                  SCAS  --  Scan String Data
;;; ----------------------------------------------------------------

(def-instr scas (instruction))
(def-instr scasb (scas) (:plain #xae (0 0) (al)))
(def-instr scasw (scas) (:plain #xaf (0 0) (ax) :operand-mode :16-bit))
(def-instr scasl (scas) (:plain #xaf (0 0) (eax) :operand-mode :32-bit))

;;; ----------------------------------------------------------------
;;;                CMPS  --  Compare String Operands
;;; ----------------------------------------------------------------

(def-instr cmps (instruction))
(def-instr cmpsb (cmps) (:plain #xa6 (0 0) ()))
(def-instr cmpsw (cmps) (:plain #xa7 (0 0) () :operand-mode :16-bit))
(def-instr cmpsd (cmps) (:plain #xa7 (0 0) () :operand-mode :32-bit))

;;; ----------------------------------------------------------------
;;;                STOS  --  Store String Data
;;; ----------------------------------------------------------------

(def-instr stos (instruction))
(def-instr stosb (stos) (:simple #xAA))
(def-instr stosw (stos) (:simple #xAB :operand-mode :16-bit))
(def-instr stosd (stos) (:simple #xAB :operand-mode :32-bit))

;;; ----------------------------------------------------------------
;;;                LODS  --  Load String Operand
;;; ----------------------------------------------------------------

(def-instr lods (instruction))
(def-instr lodsb (lods) (:simple #xAC))
(def-instr lodsw (lods) (:simple #xAD :operand-mode :16-bit))
(def-instr lodsd (lods) (:simple #xAD :operand-mode :32-bit))

