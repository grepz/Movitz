;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Copyright (C) 2003, Frode Vatvedt Fjeld <frodef@acm.org>
;; 
;; Filename:      ia-x86.asd
;; Description:   ASDF defsystem for los assembler
;; Author:        Nikodemus Siivola
;; Created at:    Wed Oct 20 23:13:46 1999
;;                
;; $Id: ia-x86.asd,v 1.4 2004/01/12 20:22:52 ffjeld Exp $
;;                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:ia-x86-system
  (:use :cl :asdf))

(in-package #:ia-x86-system)

(defsystem :ia-x86    
  :serial t ; FIXME: proper dependencies would be nice...
  :components
  ((:file "packages")
   ;; (:file "utilities")
   ;; (:file "asm")
   (:file "ia-x86")
   (:file "symtab")
   (:file "prefixes")
   (:file "registers")
   (:file "operands")
   (:file "inline-data")
   (:file "alignment")
   (:file "read")
   (:file "codec")
   (:file "proglist")
   (:file "def-instr")
   (:file "postload")))

(defmethod perform :after ((o load-op) (c (eql (find-system :ia-x86))))
  (when (y-or-n-p "Initialize instruction tables? It may take a while, ~
                   espcially on memory starved boxes.")
    (operate 'load-op :ia-x86-instr)
    (funcall (intern (string '#:init-instruction-tables) :ia-x86))))

(defclass conf-file (cl-source-file) ())

(defmethod perform ((o compile-op) (c conf-file))
  nil)

(defmethod output-files ((o compile-op) (c conf-file))
  (list (component-pathname c)))

(defsystem :ia-x86-instr
    :components
    ((:conf-file "instr-mov")
     (:conf-file "instr-add")
     (:conf-file "instr-sub")
     (:conf-file "instr-cmp")
     (:conf-file "instr-push-pop")
     (:conf-file "instr-mul-div")
     (:conf-file "instr-branch")
     (:conf-file "instr-and-or")
     (:conf-file "instr-bit")
     (:conf-file "instr-shift")
     (:conf-file "instr-string")
     (:conf-file "instr-misc")
     (:conf-file "instr-fpu")
     (:conf-file "instr-mmx")
     (:conf-file "instr-simd")))

(defsystem :ia-x86-test
    :depends-on (:ia-x86 :rt)
    :components ((:file "ia-x86-test")))

(defmethod perform ((o test-op) (c (eql (find-system :ia-x86))))
  (operate 'load-op :ia-x86-test)
  (operate 'test-op :ia-x86-test))

(defmethod perform ((o test-op) (c (eql (find-system :ia-x86-test))))
  (or (funcall (intern (string '#:do-tests) :rt))
      (error "test-op failed")))
