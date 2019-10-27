(in-package cl-lib-tests)

(version-reporter "CL-LIB-Func Tests" 5 17
                  ";; Time-stamp: <2019-10-26 18:18:28 Bradford Miller(on Aragorn.local)>"
                  ";; new 5am testing")

;; 5.17  10/26/19 move etest here so we don't have problems later; fix various typos
;; 5.16.  3/16/19 5am testing (new)

;;; Copyright (C) 2019 by Bradford W. Miller, bradfordmiller@mac.com
;;;
;; This library is free software; you can redistribute it and/or modify it under the terms of the GNU 
;; Lesser General Public License as published by the Free Software Foundation; either version 3.0 of 
;; the License, or (at your option) any later version.

;; This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
;; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;; See the GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License along with this library; 
;; if not, see <http://www.gnu.org/licenses/>.

;; miller - Brad Miller (bradfordmiller@mac.com)

;; tests for a number of the functions in this directory, to help with github releases.
;; note that packages have tests either as part of their package or in that directory.

;; used later
(defmacro etest (expected-error error-generator)
  `(handler-case ,error-generator
     (,expected-error (condition) (declare (ignore condition)) t) ; had an error so OK
     (:no-error (condition) (declare (ignore condition)) nil))) ; fail

(def-suite function-tests :description "Test suite for cl-lib functions")

(in-suite function-tests)

;; cl-array-fns

(test array-fns
  "test fns in the cl-array-fns file"

  (let ((foo (list '(a b) '(c d) '(e f)))
        (a1 (make-array '(2 3) :initial-contents foo))
        a2)
             
    (is (Prefix? "foobar" "foobarbletch"))
    (is (not (Prefix? "foo123" "foo321ccd")))
    (is (not (Prefix? "foo123" "foo12")))
    (is (equalp (setq a2 (copy-arry a1)) a1))
    (is (not (eq (car foo) (aref a1 0))))
    (is (equalp (seq-butlast foo) '((a b) (c d))))
    (is (equalp (seq-last foo) '(e f)))))

;; cl-boolean

(defstruct (boolean-test-term)
  (flags 0 :type fixnum))

(defflags boolean-test-term
    frotz
    bletch)

(test boolean-fns
  "test fns in the cl-boolean file"
  (let ((aaa (make-boolean-test-term)))      
    (setf (boolean-test-term-frotz-p aaa) t)
    (setf (boolean-test-term-bletch-p aaa) nil)
    (is (boolean-test-term-frotz-p aaa))
    (is (not (boolean-test-term-bletch-p aaa))))
  (is (xor (= 2 3) (= 4 4) (= 7 9)))
  (is (not (xor (= 2 3) (= 4 4) (= 7 9) (= 8 8))))
  (is (eqv (+ 2 3) (+ 3 2) (+ 4 1) (+ 1 4)))
  (is (nand (= 2 3) (plusp -7) (= 17 17)))
  (is (nor (= 2 3) (plusp -7) (= 17 31))))
  
;; cl-extensions

(test extensions
  "test fns in the cl-extensions file"
  (let ((foo nil)
        (count 0))
    ;; functions:
    ;; copy-hash-table

    ;; tbd
    
    ;; let-maybe
    (is (let-maybe t ((foo t)) foo))
    (is (let-maybe nil ((foo t)) (not foo)))
    
    ;; while
    (while (< count 100)
      (incf count))
    (is (= count 100))
    
    ;; while-not
    (while-not (zerop count)
      (decf count))
    (is (= count 0))
    
    ;; let*-non-null
    (is (= (let*-non-null ((a t) (b 12) (c 42)) c) 42))
    (is (null (let*-non-null ((a t) (b 12) (d nil) (c 42)) c)))
    ;; msetq (trivial)
    ;; mlet (trivial)
    ;; cond-binding-predicate-to
    (is (= 49 (cond-binding-predicate-to foo
                ((+ 17 31)
                 foo)
                (t
                 foo))))
    ;; elapsed-time-in-seconds

    ;; tbd
    
    ;; progfoo

    (is (= (progfoo 49
             (incf foo))
           50))
    ;; with-rhyme (trivial)
    ;; mv-progfoo
    ;; get-compiled-function-name
    ;; comment (trivial)
    ;; command-line-arg
    ;; getenv
    ;; if*
    (is (= (if nil t 1 2 3) 3))
    ;; make-variable
    (setq foo (make-variable "foo"))
    ;; variablep
    (is (variablep foo))
    ;; noting-progress

    ;; TBD
    ))

;; cl-lib-essentials - note these should be obvious as when CL-LIB is loaded, the versions for each file should print.
;; macro-indent-rule (not testable, allegro only)
;; interactive-lisp-p
;; version-reporter
;; detailed-version-reporter
;; report-version

;; cl-list-fns
(test list-fns
  "test functions in the file cl-list-fns"    

  ;; types
  ;; type alist ;; alists are just lists so not a particularly interesting test
  ;; type true-list
  (is (typep '(a b c) 'true-list))
  (is (not (typep '(a b . c) 'true-list)))
  ;; type dotted-list
  (is (not (typep '(a b c) 'dotted-list)))
  (is (typep '(a b . c) 'dotted-list))
  
  ;; functions
  ;; true-list-p ;; tested with type true-list
  ;; dotted-list-p ;; tested with type dotted-list
  ;; update-alist
  (let ((a '((a . b) (c . d) (e . f))))
    (is (eql (cdr (assoc 'c a)) 'd))
    (update-alist 'c 'q a)
    (is (eql (cdr (assoc 'c a)) 'q))
    ;; update-alist-alist
    ;; reverse-alist
    (is (eql (cadr (assoc 'f (reverse-alist a))) 'e)))
  ;; alistify
  (is (equalp (alistify '(foo bar bletch quux)) '((foo . bar) (bletch . quux))))
  ;; de-alistify
  (is (equalp (de-alistify '((foo bar) (bletch quux))) '(:foo (bar) :bletch (quux))))
  ;; tail-equalp
  (is (tail-equalp '(f g) '(a b c d e f g)))
  (is (not (tail-equalp '(f g) '(a b c d e f g h))))
  ;; force-list
  (is (listp (force-list 'a)))
  ;; delete-n
  (let ((b '(a b c d e)))
    (delete-n b 2)
    (is (equalp b '(a b d e)))
    ;; remove-n
    (is (equalp (remove-n b 1) '(a d e))))
  ;; flatten
  (is (equalp '(a b c d e) (flatten '(((a) b (c ((d)) e))))))
  ;; eqmemb
  (is (eqmemb 'a 'a))
  (is (eqmemb 'a '(b c a d e)))
  ;; car-eq
  (is (car-eq 'a '(a b c d e)))
  ;; dremove
  (let ((c '(a b c d e)))
    (dremove 'c c)
    (is (equalp c '(a b d e))))
  ;; displace
  ;; tailpush
  (let ((d '(a b c d e)))
    (tailpush 'f d)
    (is (equalp d '(a b c d e f))))
  ;; explode
  ;; implode
  ;; crush

  ;; TBD
  
  ;; listify-string
  ;; and-list
  ;; or-list
  ;; make-plist
  )

;; cl-map-fns
(test map-fns
   "test functions and macros in file cl-map-fns"
   ;; functions and macros
   ;; mapatoms

   ;; tbd
   (let ((test-list '(a b . c)))
     ;; mapc-dotted-list
     ;; mapcar-dotted-list
     (is (equalp test-list (mapcar-dotted-list #'identity test-list)))
     ;; mapcan-dotted-list
     ;; maplist-dotted-list
   
     ;; some-dotted-list
     (is (some-dotted-list #'(lambda (x) (eql x 'c)) test-list))
     ;; every-dotted-list
     (is (every-dotted-list #'symbolp test-list))
     ;; dosequence
     ))

;; cl-sets

(test set-fns
  "test functions and macros in file cl-sets"   
  ;; functions
  (let ((test-list-a '(a b c nil d e nil (f g nil h)))
        result-a)
    
    ;; list-without-nulls
    (setq result-a (list-without-nulls test-list-a))
    (is (not (every-dotted-list #'identity test-list-a)))
    (is (every-dotted-list #'identity result-a)) ; no nulls in top level list
    
    ;; cartesian-product
    ;; check the set difference is equal to the cross product (because just two lists)
    (is (null (set-difference (cartesian-product '(a b) '(c d)) (cross-product '(a b) '(c d)))))
    ;; more general cross-product
    (is (null (set-difference (cross-product '(c d) '(e f) '(a b)) 
                              (cross-product '(a b) '(c d) '(e f)))))
    ;; permutations
    (is (= (length (permutations '(a b c))) 6))
    ;; powerset
    (is (= (length (powerset '(a b c))) 7))
    ;; circular-list
    (is (equalp (mapcar #'+ '(1 2 3 4 5) (circular-list 3)) '(4 5 6 7 8)))
    ;; occurs
    (is (occurs 'a '(((b c) d) e (f (g (h a))))))
    (is (not (occurs 'i '(((b c) d) e (f (g (h a)))))))
    ;; firstn
    (is (equalp (firstn '(a b c d e) 3) '(a b c)))
    ;; in-order-union
    (is (equalp (in-order-union '(a b c) '(a d e f)) '(a b c d e f)))
    ;; fast-union
    (is (equalp (union '(a b c) '(a d e f)) (fast-union '(a b c) '(a d e f))))
    ;; fast-intersection
    (is (equalp (intersection '(a b c) '(a d e f)) (fast-intersection '(a b c) '(a d e f))))
    ))

    ;; clos-extensions

;; functions and macros
;; defclass-x
;; make-load-form-with-all-slots
;; determine-slot-readers (tested via make-load-form-with-all-slots)
;; determine-slot-writers (ditto)
;; determine-slot-initializers
;; generate-legal-slot-initargs
;; slot-for-initarg

;; files
;; functions and macros
;; load-once
;; do-file

;; keywords
;; functions and macros
;; truncate-keywords
;; remove-keyword-arg
;; extract-keyword
;; make-keyword
;; key-value-list-p

;; number-handling
;; functions and macros
;; bit-length
;; sum-of-powers-of-two-representation
;; difference-of-powers-of-two-representation
;; ordinal-string
;; between
;; round-to
;; factorial
;; round-off

;; strings
;; functions and macros
;; string-search-car
;; string-search-cdr
;; parse-with-delimiter
;; parse-with-delimiters
;; parallel-substitute
;; parse-with-string-delimiter
;; parse-with-string-delimiter*
;; split-string
;; format-justified-string
;; number-to-string
;; null-string
;; time-string
;; read-delimited-string
;; force-string

