(IN-PACKAGE CL-LIB)
(version-reporter "CL-LIB-FNS-Boolean Fns" 5 16 ";; Time-stamp: <2019-03-16 16:32:58 Bradford Miller(on Aragorn.local)>" 
                  "fix doc on nand")

;; 5.16. 3/16/19 fix doc on nand

;;;; ****************************************************************
;;;; Extensions to Common Lisp **************************************
;;;; ****************************************************************
;;;;
;;;; This file is a collection of extensions to Common Lisp. 
;;;;
;;;; It is a combination of the CL-LIB package copyleft by Brad Miller
;;;; <miller@cs.rochester.edu> and a similar collection by
;;;; Mark Kantrowitz <mkant+@cs.cmu.edu>.
;;;;
;;;; The following functions were originally from CL-LIB:
;;;;   let-if, factorial, update-alist, truncate-keywords, while,
;;;;   defclass-x, copy-hash-table, defflag, round-to, extract-keyword
;;;;   let*-non-null, mapc-dotted-list, 
;;;;   mapcar-dotted-list, mapcan-dotted-list, some-dotted-list, 
;;;;   every-dotted-list, msetq, mlet, dosequence, force-string, prefix?,
;;;;   elapsed-time-in-seconds, bit-length, flatten, 
;;;;   sum-of-powers-of-two-representation, 
;;;;   difference-of-powers-of-two-representation,
;;;;   ordinal-string, between, 
;;;;   cond-binding-predicate-to <quiroz@cs.rochester.edu>
;;;;   remove-keywords <baldwin@cs.rochester.edu>
;;;;
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
;; This portion of CL-LIB Copyright (C) 1994-2008 by Bradford W. Miller and the 
;;                                                   Trustees of the University of Rochester
;; 
;; This library is free software; you can redistribute it and/or modify it under the terms of the GNU 
;; Lesser General Public License as published by the Free Software Foundation; either version 3.0 of 
;; the License, or (at your option) any later version.

;; This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
;; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;; See the GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License along with this library; 
;; if not, see <http://www.gnu.org/licenses/>.
;;;;
;
;;; this is to support a field in a clos structrure called "flags",
;;; which is bit encoded. The testname can be used to see if the bit
;;; (defined by flagname - a constant) is set. It can also be setf to
;;; set or clear it. The type is the type of structure this test will
;;; handle, allowing multiple encodings of the flags field for
;;; different structures.
(DEFMACRO DEFFLAG (TESTNAME (TYPE FLAGNAME))
  `(PROGN (DEFMETHOD ,TESTNAME ((TERM ,TYPE))
	    (LOGTEST ,FLAGNAME (FLAGS TERM)))
	  (DEFMETHOD (SETF ,TESTNAME) (NEW-FLAG (TERM ,TYPE))
	    (SETF (FLAGS TERM) (IF NEW-FLAG
				   (LOGIOR (FLAGS TERM) ,FLAGNAME)
				   (LOGAND (FLAGS TERM) (LOGNOT ,FLAGNAME)))))))

;; similar to above, but for defstruct type thingos; we assume the
;; accessor is "typename"-flags

;; fix to make sure we use typename in constant to avoid name
;; collisions 7/30/92 bwm
(DEFMACRO DEFFLAGS (TYPENAME &BODY FLAGNAMES)
  (LET ((ACCESSOR (INTERN (FORMAT NIL "~A-FLAGS" TYPENAME)))
	(VARNAME1 (GENSYM))
	(VARNAME2 (GENSYM)))
    (DO* ((FLAG FLAGNAMES (CDR FLAG))
	  (FUNNAME (INTERN (FORMAT NIL "~A-~A-P" TYPENAME (CAR FLAG))) (INTERN (FORMAT NIL "~A-~A-P" TYPENAME (CAR FLAG))))
          (CONSTNAME (INTERN (FORMAT NIL "+~A-~A+" TYPENAME (CAR FLAG))) (INTERN (FORMAT NIL "+~A-~A+" TYPENAME (CAR FLAG))))
	  (COUNT 1 (* COUNT 2))
	  (CODE))
	((NULL FLAG) `(PROGN ,@CODE))
      (PUSH `(DEFSETF ,FUNNAME (,VARNAME1) (,VARNAME2)
	       `(SETF (,',ACCESSOR ,,VARNAME1) (IF ,,VARNAME2
						(LOGIOR (,',ACCESSOR ,,VARNAME1) ,',constname)
						(LOGAND (,',ACCESSOR ,,VARNAME1) (LOGNOT ,',constname)))))
	    CODE)
      (PUSH `(DEFUN ,FUNNAME (,VARNAME1)
	       (LOGTEST ,constname (,ACCESSOR ,VARNAME1)))
	    CODE)
      (PUSH `(DEFCONSTANT ,constname ,COUNT) CODE))))

(macro-indent-rule defflags 1)

;; define boolean control extensions to and, or, not... the bit
;; operators are there, but not the more general short-circuting ones.
;; Some of these will only make sense on two operands, and many can't
;; short circuit (exclusive ops, for instance).

(defmacro xor (&rest predicates)
  "True only if exactly one predicate is true. Short circutes when it
finds a second one is true. Returns the true predicate"
  (let ((result (gensym))
        (temp (gensym))
        (block-name (gensym)))
    `(block ,block-name
       (let ((,result ,(car predicates))
             ,temp)
         ,@(let (code-result)
             (dolist (pred (cdr predicates))
               (push `(cond
                       ((and (setq ,temp ,pred)
                             ,result)
                        (return-from ,block-name nil))
                       (,temp
                        (setq ,result ,temp)))
                     code-result))
             (nreverse code-result))
         ,result))))
         
(macro-indent-rule xor (like and))

(defmacro eqv (&rest predicates)
  "True iff all predicates produce the same result according to eql,
or passed :test (exclusive nor if binary)"
  (let ((result (gensym))
        (real-preds (truncate-keywords predicates))
        (test-key (extract-keyword :test predicates #'eql))
        (block-name (gensym)))
      `(block ,block-name
         (let ((,result ,(car real-preds)))
           ,@(let (code-result)
               (dolist (pred (cdr real-preds))
                 (push `(if (not (funcall ,test-key ,pred ,result))
                            (return-from ,block-name nil))
                       code-result))
               (nreverse code-result))
           (or ,result t)))))

(macro-indent-rule eqv (like and))

(defmacro nand (&rest predicates)
  "True only if at least one predicate is not true. Short circutes when it
finds one is false."
  (let ((block-name (gensym)))
    `(block ,block-name
       ,@(let (code-result)
           (dolist (pred predicates)
             (push `(if (not ,pred)
                        (return-from ,block-name t))
                   code-result))
             (nreverse code-result))
       nil)))
         
(macro-indent-rule nand (like and))

(defmacro nor (&rest predicates)
  "True only if all predicates are false. Short circutes when it finds
a one is true."
  (let ((block-name (gensym)))
    `(block ,block-name
       ,@(let (code-result)
           (dolist (pred predicates)
             (push `(if ,pred
                        (return-from ,block-name nil))
                   code-result))
             (nreverse code-result))
       t)))
         
(macro-indent-rule nor (like and))

#-(or LISPM MCL)
(defun neq (x y)
  "not eq"
  (not (eq x y)))
