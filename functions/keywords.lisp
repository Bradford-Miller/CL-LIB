(in-package cl-lib)

(version-reporter "CL-LIB-FNS-Keyword Fns" 5 19 ";; Time-stamp: <2021-02-08 11:44:30 gorbag>" 
                  ";; remove-keyword-arg more robust")

;; 5.19 2/ 8/21 make remove-keyword-arg more robust to odd numbers of arguments in argument-list
;; 5.16 2/23/19 use sbcl's sb-int:*Keyword-package* instead of consing another
;; 5.8 1/30/08 key-value-list-p
;; 5.2 6/28/07 make arglist of make-keyword more illuminating

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


;;; Faster definition. In old definition, length may wind up cdring
;;; down the list (depends on the lisp).
(defun truncate-keywords (input-list)
  "Many functions take multiple arguments, via &rest, that can cause
   problems when keyword arguments are also supplied. This function
   truncates a list at the first top-level keyword. Thus, '(A B C :FOO D)
   is returned as (A B C). Note that the new list is freshly consed to 
   avoid any stack problems with destroying a &rest argument."
  (declare (type list input-list)
	   (optimize (speed 3) (safety 0)))
  (ldiff input-list (member-if #'keywordp input-list)))

;; 2/8/21 - no longer goes down input-list 2 entries at a time, which allows us to handle odd lambda-lists (where not
;;             all arguments are keywords!) Now O(n) instead of O(n/2) but that's fine.
;; changed to return multiple values 7/23/02 BWM
(defun remove-keyword-arg (keyword input-list)
  "from a set of keyword arguments, remove a particular pair marked by
the passed keyword, i.e.  \(remove-keyword-arg :a '(:b 2 :a 3 :d 7))
-> (:b 2 :d 7). Return multiple values, the first being the new list,
the second being the argument to the keyword removed, and the third
indicating if there were a removal (useful if the second value is
null)."
  (declare (type keyword keyword) 
           (type list input-list)
           (values new-list keyword-value removed-p))
  ;; remember a keyword could be an argument, so we can't just find
  ;; the first occurance, we have to treat the list in pairs.
  (cond
   ((null input-list)
    (values nil nil nil))
   ((eql (car input-list) keyword)
    (values (cddr input-list) (cadr input-list) t))
   ((endp (cdr input-list))
    (values input-list nil nil))
   (t
    (mlet (new-list keyword-value removed-p)
          (remove-keyword-arg keyword (cdr input-list))
      (if removed-p
          (values (list* (car input-list) new-list)
                  keyword-value
                  removed-p)
          (values input-list nil nil))))))


;;
;;

;;; any benefit of the position's :from-end is lost by the calls to length,
;;; so use member.
;; 2/1/07 - as a second value, return the full binding - allows destructive update
;;        - clarify and fix semantics of default and no-value. Not sure no-value is useful anymore.
(defun extract-keyword (key arglist &optional (default nil) 
                        &key (no-value nil))
  "Searches the arglist for keyword key, and returns the following mark,
   or the default if supplied. If no-value is non-nil, then if the key is present and nothing follows it,
   the no-value parameter it is returned."
  (declare (type list arglist)
	   (type t default)
	   (type keyword key)
	   (optimize (speed 3) (safety 0)))
  (let ((binding (member key arglist)))
    (values (cond ((null binding)
                   default) ; nothing there
                  ((cdr binding)
                   (cadr binding)) ;found it
                  (t
                   no-value)) ;it was there, but doesn't have a value (was the last item in the list)
            binding)))

;; sbcl port 2/23/19 BWM
#-(or EXCL sbcl) (defvar *keyword-package* (find-package 'keyword))
(defun make-keyword (symbol-or-string)
  (intern (IF (SYMBOLP SYMBOL-or-string)
	      (symbol-name symbol-or-string)
	      SYMBOL-or-string)
	  #-(or excl sbcl) *keyword-package*
          #+excl excl:*keyword-package*
          #+sbcl sb-int:*keyword-package*))

(defun key-value-list-p (key-value-pairs)
  "Assure that the passed argument is an alternating list of keywords and something"
  (or (null key-value-pairs) ; nil is fine (and allows tail recursion!)
      (and (consp key-value-pairs) ; otherwise, better be a real list
           (keywordp (car key-value-pairs)) 
           (listp (cdr key-value-pairs)) ; check for . case
           (not (endp (cdr key-value-pairs))) 
           (key-value-list-p (cddr key-value-pairs))))) 
        
