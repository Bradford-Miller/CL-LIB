(in-package cl-lib)

(version-reporter "CL-LIB-FNS-Array Fns" 5 0 ";; Time-stamp: <2012-01-05 17:44:49 millerb>" 
                  "CVS: $Id: cl-array-fns.lisp,v 1.2 2012/01/05 22:47:53 millerb Exp $
restructured version")

;;; 
;;; Copyright (C) 1996--1992 by Bradford W. Miller, miller@cs.rochester.edu
;;;                             and the Trustees of the University of Rochester
;;;
;;; Portions Copyright (C) 2002 by Bradford W. Miller, bradford.w.miller@gmail.com

;;; Right of use & redistribution is granted as per the terms of the 
;;; GNU LIBRARY GENERAL PUBLIC LICENCE version 2 which is incorporated here by
;;; reference. 

;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; version 2.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Library General Public License for more details.

;;; You should have received a copy of the GNU Library General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;


;;;
;;; The following is contributed by miller@cs.rochester.edu

;; additions for other versions of lisp are welcome!

;;; the following is contributed by baldwin@cs.geneseo.edu

(defun Prefix? (Prefix Seq)
  "Prefix? - Checks to see if Prefix is really a prefix of Seq. Returns
T if it is, NIL otherwise. Just checks that Prefix is no longer than
Seq, then checks to see if the the initial subsequence of Seq that is
the same length as Prefix is equal to Prefix. Prefix is a real prefix
if and only if both conditions hold."

  (and (<= (length Prefix) (length Seq))
       (equalp (subseq Seq 0 (length Prefix)) Prefix)))


;; duplicate an array. 12/29/93 by miller
(defun copy-array (array &optional (element-copier #'identity))
  "Returns an (exact) copy of the passed array, with same size, fill
pointer (if there is one), adjustable quality, element type, and
contents. Uses element-copier to copy elements (default #'identity)."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (progfoo (make-array (array-dimensions array) 
                       :element-type (array-element-type array)
                       :adjustable (adjustable-array-p array)
                       :fill-pointer (and (array-has-fill-pointer-p array)
                                          (fill-pointer array)))
    (dotimes (i (array-total-size array))
      (setf (row-major-aref foo i)
            (funcall element-copier (row-major-aref array i))))))

;;;; The following functions were contributed by Mark Kantrowitz:
;;;;   circular-list, dofile, seq-butlast, seq-last, firstn, in-order-union
;;;;   parse-with-delimiter, parse-with-delimiters, string-search-car,
;;;   string-search-cdr, parallel-substitute, lisp::nth-value,
;;;;   parse-with-string-delimiter, parse-with-string-delimiter*,
;;;;   member-or-eq, number-to-string, null-string, time-string.
;;;;   list-without-nulls, cartesian-product, cross-product, permutations
;;;;   powerset, occurs, split-string, format-justified-string, 
;;;;   eqmemb, neq, car-eq, dremove, displace, tailpush, explode,
;;;;   implode, crush, listify-string, and-list, or-list, lookup,
;;;;   make-variable, variablep, make-plist, make-keyword
;;;;   

;;; ********************************
;;; Sequences **********************
;;; ********************************
(defun seq-butlast (sequence &optional (n 1))
  (let* ((length (length sequence))
	 (delta (- length n)))
    (when (plusp delta)
      (subseq sequence 0 delta))))

(defun seq-last (sequence &optional (n 1))
  (let* ((length (length sequence))
	 (delta (- length n)))
    (when (plusp delta)
      (subseq sequence delta length))))


