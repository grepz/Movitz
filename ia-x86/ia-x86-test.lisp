;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Copyright (C) 2003, Frode Vatvedt Fjeld <frodef@acm.org>
;; 
;; Filename:      ia-x86-test.lisp
;; Description:   Regression tests for the assembler
;; Author:        Nikodemus Siivola
;; Created at:    Wed Oct 20 23:13:46 1999
;;                
;; $Id: ia-x86-test.lisp,v 1.2 2004/01/16 11:54:14 ffjeld Exp $
;;                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :ia-x86-test
  (:use :cl :ia-x86 :rt))

(in-package :ia-x86-test)

(deftest asm.1
    (asm :movl :eax :ebx)
  #C(#x89C3 #x2))

(defvar *proglist-1* '((:movl 10 :eax) loop 
		       (:decl :eax) 
		       (:je 'exit) 
		       (:jmp 'loop) exit))

(deftest read-proglist.1
    ;; KLUDGE: it's just simpler to compare the printed version here...
    (equal (with-output-to-string (s)
	     (prin1 (read-proglist *proglist-1*) s))
	  "(#<asm MOVL #xA => %EAX> LOOP #<asm DECL %EAX> #<asm JE 'EXIT> #<asm JMP 'LOOP>
 EXIT)")
  t)

(deftest proglist-encode.1
    (multiple-value-bind (a b) 
	(proglist-encode :octet-list :32-bit 0 (read-proglist *proglist-1*))
      (and (equal a '(#xB8 #xA #x0 #x0 #x0 #x48 #x74 #x2 #xEB #xFB))
	   (equal b '((EXIT . #xA) (LOOP . #x5)))))
  t)
