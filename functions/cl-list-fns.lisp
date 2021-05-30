(in-package cl-lib)

(version-reporter "CL-LIB-FNS-List Fns" 5 13 ";; Time-stamp: <2019-02-18 16:00:00 Bradford Miller(on Aragorn.local)>" 
                  ";; use lispworks' version of dotted-list-p")

;; 5.13 10/19/11 use lispworks' version of dotted-list-p
;; 5.12 7/11/08 New: de-alistify, dotted-list-p, dotted-list
;; 5.9  2/19/08 New: tail-equalp
;; 5.4  5/18/07 New: alistify
;; 5.3          Fixed: update-alist-alist
;;; 
;;; Copyright (C) 1996--1992 by Bradford W. Miller, miller@cs.rochester.edu
;;;                             and the Trustees of the University of Rochester
;;;
;;; Portions Copyright (C) 2002-2011 by Bradford W. Miller, bradford.w.miller@gmail.com

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

;; some type discriminators (at least for documentation purposes)

(deftype alist () 'list)

(defun true-list-p (term)
  "Returns t if the term is a non-dotted list. Note that nil is a true list."
  (declare (optimize (speed 3) (safety 0)))
  (and (listp term)
       (endp (cdr (last term)))))

(deftype true-list () '(and list (satisfies true-list-p)))

#-lispworks 
(defun dotted-list-p (term)
  "Returns t if the term is a dotted list."
  (declare (optimize (speed 3) (safety 0)))
  (and (listp term)
       (not (endp (cdr (last term))))))

(deftype dotted-list () '(and list (satisfies dotted-list-p)))

;;; The #'eql has to be quoted, since this is a macro. Also, when
;;; binding variables in a macro, use gensym to be safe.
(defmacro update-alist (item value alist &key (test '#'eql) (key '#'identity))
  "If alist already has a value for Key, it is updated to be Value. 
   Otherwise the passed alist is updated with key-value added as a new pair."
  (let ((entry (gensym))
        (itemv (gensym))
        (valuev (gensym)))              ; to assure proper evaluation order
                                        ; and single expansion
    `(let* ((,itemv ,item)
            (,valuev ,value)
            (,entry (assoc ,itemv ,alist :test ,test :key ,key)))
       (if ,entry
	   (progn (setf (cdr ,entry) ,valuev)
		  ,alist)
	   (setf ,alist (acons ,itemv ,valuev ,alist))))))

(macro-indent-rule update-alist 1)

(defmacro update-alist-alist (outer-item inner-item inner-item-value alist-alist 
                              &key (outer-test '#'eql) (inner-test '#'eql) (outer-key '#'identity) (inner-key '#'identity))
  "Alists of alists comes up so often in my code, I've decided to just macroize it.
They're great for cheap and dirty (early pass) KBs, where the outer item is the object id and the inner-item is an attribute, 
retrieving the value of the attribution on the object."
  (let ((outer-itemv (gensym))
        (inner-list (gensym)))
    `(let* ((,outer-itemv ,outer-item)
            (,inner-list (cdr (assoc ,outer-itemv ,alist-alist :test ,outer-test :key ,outer-key))))
       (update-alist ,outer-itemv
                     (update-alist ,inner-item ,inner-item-value 
                                   ,inner-list
                                   :test ,inner-test :key ,inner-key)
                     ,alist-alist
                     :test ,outer-test
                     :key ,outer-key))))

(defun reverse-alist (alist &key (test #'eql))
  "Takes an alist of uniqe keys and non-unique values, and returns an
alist of unique keys based on the values, whose values are lists of
the original keys."
  (declare (type alist alist))

  (let (result)
    (dolist (x alist)
      (let ((assoc-result (assoc (cdr x) result :test test)))
	(if assoc-result
	    (setf (cdr assoc-result) (cons (car x) (cdr assoc-result)))
	    (setq result (acons (cdr x) (list (car x)) result)))))
    result))


(defun alistify (list)
  "Turns a list of the form '(:key :value :key :value) as one might get with a lambda list into an alist
\'((:key :value) (:key :value))"
  (declare (type list list))

  (cond
    ((endp list)
     nil)
    (t
     (cons (cons (car list) (cadr list))
           (alistify (cddr list))))))

(defun de-alistify (alist &optional true-list-p)
  "Turns an alist of of the form '((key value) (key . value)) into a lambda-list type form '(:key (value) :key value)
If true-list-p is non-nil, then '((key value)) becomes '(:key value), but '((key . value)) will generate an error.
In either case '((key value1 value2 value3)) becomes '(:key (value1 value2 value3))"
  (declare (type alist alist))
  
  (list* (make-keyword (caar alist))
         (if (and true-list-p
                  (endp (cddar alist)))
             (cadar alist)
             (cdar alist))
         (if (cdr alist)
           (de-alistify (cdr alist) true-list-p))))
      
(defun tail-equalp (list1 list2)
  "Return non-nil if list1 is equalp to some tail of list2. Like tailp, but cons structure does not have to be shared."
  (cond
    ((null list1)
     t)
    ((or (not (listp list2)) ; check list2 is a true list
         (endp list2))
     nil)
    ((equalp list1 list2))
    (t
     (tail-equalp list1 (cdr list2)))))

(defun force-list (foo)
  (if (listp foo)
      foo
    (list foo)))

(defun delete-n (list n)
  "Delete the nth element (0-based) of the list. Destructive."
  (declare (type list list)
           (type fixnum n))

  ;; find the cell to change
  (cond
    ((zerop n)
     (cdr list))
    (t
     (let ((cell (nthcdr (1- n) list)))
       (when cell ; didn't go beyond the end of the list
         (setf (cdr cell) (cddr cell))))
     list)))

(defun remove-n (list n)
  "Remove the nth element (0-based) of the list. Non-destructive."
  (declare (type list list)
           (type fixnum n))

  (cond
    ((zerop n)
     (cdr list))
    ((< n (list-length list))
     (append
      (subseq list 0 n)
      (nthcdr (1+ n) list)))
    (t
     list))) ; nothing to remove

;;; the following is contributed by baldwin@cs.geneseo.edu

(defun Flatten (L)
  "Flattens list L, i.e., returns a single list containing the
 same atoms as L but with any internal lists 'dissolved'. For example,
 (flatten '(a (b c) d))  ==>  (a b c d)

 Recursively flattens components of L, according to the following rules-
  - an atom is already flattened.
  - a list whose CAR is also a list is flattened by appending the
    flattened CAR to the flattened CDR (this is what dissolves internal
    lists).
  - a list whose CAR is an atom is flattened by just flattening the CDR
    and CONSing the original CAR onto the result.
 These rules were chosen with some attention to minimizing CONSing."

  (cond
    ((null L )   '() )
    ((atom L)    L)
    ((consp L)
     (if (consp (car L))
	 (append (Flatten (car L)) (Flatten (cdr L)))
	 (cons (car L) (Flatten (cdr L)))))
    (t   L)))

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
(defun eqmemb (item list &key (test #'equal))
  "Checks whether ITEM is either equal to or a member of LIST."
  (if (listp list)
      (member item list :test test)
      (funcall test item list)))

(defun car-eq (x y)
  "Checks whether Y is eq to the car of X."
  (and (listp x) ; consp?
       (eq (car x) y)))


(defun dremove (item list)
  "Destructive remove which replaces the original list with the list
   that results when ITEM is deleted from LIST."
  ;; This is safe only because of the way delete works.
  (displace list (delete item list :test #'eq)))

(defun displace (list val)
  "Replaces LIST with VAL by destructively modifying the car and cdr of LIST.
   Warning: VAL must not share list structure with LIST or you'll be sorry."
  (when list
    ;; Can't alter NIL.
    (rplaca list (car val))
    (rplacd list (cdr val))))

(defun tailpush (item list)
  "Pushes ITEM onto the tail of LIST. Does not work if the list is null."
  (when list
    (rplacd (last list) (list item))))

(defun explode (symbol)
  (map 'list #'identity (symbol-name symbol)))

(defun implode (list &optional (package *package*))
  (intern (map 'string #'identity list) package))

(defun crush (a b &optional (package *package*))
  (implode (append (explode a) (explode b)) package))

(defun listify-string (string)
  "Turns a string into a list of symbols."
  (declare (type string string))

  (let ((eof (gensym))
	(result nil)
	(start 0)
	item)
    (loop
     (multiple-value-setq (item start)
	 (read-from-string string nil eof :start start))
     (when (eq item eof)
       (return result))
     (setq result (nconc result (list item))))))

(defun and-list (list)
  (declare (type list list))

  (dolist (item list t)
    (unless item
      (return nil))))

(defun or-list (list)
  (declare (type list list))

  (dolist (item list nil)
    (unless item
      (return t))))

(defun make-plist (keys data &optional (plist '()))
  "Constructs a property list from keys and data (addition to plist)."
  (cond ((and (null data) (null keys))
	 plist)
	((or  (null data) (null keys))
	 (error "The lists of keys and data are of unequal length."))
	(t
	 (list* (car keys)
		(car data)
		(make-plist (cdr keys) (cdr data) plist)))))

