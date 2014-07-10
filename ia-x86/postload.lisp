;;;;------------------------------------------------------------------
;;;; 
;;;;    Copyright (C) 20012000, 2003-2004,
;;;;    Department of Computer Science, University of Tromso, Norway
;;;; 
;;;; Filename:      postload.lisp
;;;; Description:   
;;;; Author:        Frode Vatvedt Fjeld <frodef@acm.org>
;;;; Created at:    Mon Jan 31 16:33:23 2000
;;;; Distribution:  See the accompanying file COPYING.
;;;;                
;;;; $Id: postload.lisp,v 1.4 2004/02/10 00:03:52 ffjeld Exp $
;;;;                
;;;;------------------------------------------------------------------

(in-package #:ia-x86)

(defun table-stats (prefix table)
  (format t "~&;; ~A ~D x86 instruction templates loaded.~%"
	  prefix
	  (reduce #'+ (map 'list #'length
			   table)))

  (format t "~&;; ~A Longest lookup list (#x~2,'0X) has ~D elements.~%"
	  prefix
	  (position #0=(reduce #'max (map 'list #'length
					 table))
		    (map 'list #'length
			 table))
	  #0#)
  
  (format t "~&;; ~A Average lookup list length is ~,1F.~%"
	  prefix
	  (/ (reduce #'+ (map 'list #'length
			      table))
	     (length table)))

  (let ((*print-base* 16)
	(*print-radix* t))
    (format t "~&;; ~A There are ~D empty lookup lists (~D%):~%;; ~A~%"
	    prefix
	    #1=(count-if #'null table)
	    (round (* 100 (/ #1# #x100)))
	    (let (r)
	      (dotimes (i #x100 (nreverse r))
		(when (null (aref table i))
		  (push i r)))))))
