;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: CL-LIB; Base: 10 -*-
(IN-PACKAGE CL-LIB)
(version-reporter "CL-LIB-FNS-File Fns" 5 0 ";; Time-stamp: <2008-05-03 13:35:04 gorbag>" 
                  "CVS: $Id: files.lisp,v 1.2 2008/05/03 17:42:01 gorbag Exp $
restructured version")

;; This portion of CL-LIB Copyright (C) 1984-2008 Bradford W. Miller and the
;;                                                Trustees of the University of Rochester
;; 
;; This library is free software; you can redistribute it and/or modify it under the terms of the GNU 
;; Lesser General Public License as published by the Free Software Foundation; either version 3.0 of 
;; the License, or (at your option) any later version.

;; This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
;; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;; See the GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License along with this library; 
;; if not, see <http://www.gnu.org/licenses/>.


;;;; Note the local extensions remain in cl-extensions. <miller>
;
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
;;;; The GNU Emacs distribution agreement is included by reference.
;;;; Share and Enjoy!
;;;;

(let (loaded-pathnames)
  (defun clear-load-once ()
    (setq loaded-pathnames nil))
  
  (defun load-once (pathname &rest load-opts)
    (let ((preloaded (assoc pathname loaded-pathnames :test #'equalp)))
      (cond
       ((and preloaded
             (> (file-write-date pathname) (cdr preloaded)))
        (apply #'load pathname load-opts)
        (setf (cdr preloaded) (file-write-date pathname)))
       ((null preloaded)
        (apply #'load pathname load-opts)
        (push (cons pathname (file-write-date pathname)) loaded-pathnames))))))

(defmacro dofile ((var filename &optional return-form) &body body)
  "Opens the specified file for input, reads successive lines 
   from the file, setting the specified variable <var> to 
   each line. When end of file is reached, the value of <return-form>
   is returned."
  (let ((eof (gensym "EOF"))
	(stream (gensym "STREAM")))
    `(with-open-file (,stream ,filename :direction :input)
       (do ((,var (read-line ,stream nil ,eof)
		  (read-line ,stream nil ,eof)))
	   ((eq ,var ,eof)
	    ,return-form)
	 ,@body))))

#+allegro-v4.1
(add-initialization "lep-init for dofile"
                    '(lep::eval-in-emacs "(put 'dofile 'fi:lisp-indent-hook '(like with-open-file))")
                    '(:lep))

