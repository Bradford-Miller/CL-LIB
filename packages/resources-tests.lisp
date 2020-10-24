(in-package CL-LIB)

(cl-lib:version-reporter "CL-LIB-Resources Tests" 5 18 ";; Time-stamp: <2020-01-05 17:17:17 Bradford Miller(on Aragorn.local)>" 
                         "new")

;; 5.18   1/ 5/20 In lispworks, make sure we specifically refer to cl-lib version of resources
;; 5.16.  2/17/19 5am testing (new)

;; This portion of CL-LIB Copyright (C) 2019 Bradford W. Miller
;; 
;; This library is free software; you can redistribute it and/or modify it under the terms of the GNU 
;; Lesser General Public License as published by the Free Software Foundation; either version 3.0 of 
;; the License, or (at your option) any later version.

;; This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
;; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;; See the GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License along with this library; 
;; if not, see <http://www.gnu.org/licenses/>.

;; miller - Brad Miller (bradfordmiller@mac.com)

(5am:def-suite resources-tests :description "Test suite for resources package")

(5am:in-suite resources-tests)

;; following the API spec from resources.lisp.

;; for the purposes of testing, we will create two kinds of resources, one without args (homogenious resource)
;; and one with two arguments. The former will cons up vectors of a fixed size, and the latter will cons up arrays of
;; the specified dimensions.

(defun clear-fixed-resource (r)
  (setf (aref r 0) nil)
  (setf (aref r 1) nil)
  (setf (aref r 2) nil)
  (setf (aref r 3) nil)
  (setf (aref r 4) nil))

(cl-lib::defresource (test-fixed-resource () :initial-copies 5 :initializer clear-fixed-resource)
    (make-array (list 5) :element-type t :initial-element nil))

(5am:test allocate-fixed-resources "Test that we can allocate the fixed resources, in excess of the pool"
      (let ((foo1 (allocate-test-fixed-resource))
            (foo2 (allocate-test-fixed-resource))
            (foo3 (allocate-test-fixed-resource))
            (foo4 (allocate-test-fixed-resource))
            (foo5 (allocate-test-fixed-resource))
            (foo6 (allocate-test-fixed-resource)))
        ;; make them unique
        (setf (aref foo1 2) 1)
        (setf (aref foo2 2) 2)
        (setf (aref foo3 2) 3)
        (setf (aref foo4 2) 4)
        (setf (aref foo5 2) 5)
        (setf (aref foo6 2) 6)
        ;; make sure they are different
        (is (not (eq foo1 foo2)))
        (is (not (eq foo2 foo3)))
        (is (not (eq foo3 foo4)))
        (is (not (eq foo4 foo5)))
        (is (not (eq foo5 foo6)))
        ;; at this point, all resources should be in use
        (cl-lib::map-resource #'(lambda (object in-use)
                          (is in-use))
                      'test-fixed-resource)
        ;; deallocate one resource and check that it was the only one marked to not be in use
        (deallocate-test-fixed-resource foo4)
        (cl-lib::map-resource #'(lambda (object in-use)
                          (cond
                            ((eq foo4 object)
                             (is (not in-use))
                             (is (null (aref object 2)))) ; should have been cleared!
                            (t
                             (is in-use)))) ; everyone else should still be in-use
                      'test-fixed-resource)
        ;; now when we allocate an object, we should get back that one
        (progfoo (allocate-test-fixed-resource)
           ;; it should be eq to our old foo4 (not a new one)
           (is (eq foo4 foo))
           ;; and all should be in-use again
           (cl-lib::map-resource #'(lambda (object in-use)
                             (is in-use))
                         'test-fixed-resource))))

;;
(5am:test clear-fixed-resource
      (clear-test-fixed-resource)
      ;; should all be unallocated
      (cl-lib::map-resource #'(lambda (object in-use)
                        (is (not in-use)))
                    'test-fixed-resource))

;; ok, now lets try the same with a variable resource. 

(defun clear-variable-resource (r a1 a2)
  (dotimes (i1 a1)
    (dotimes (i2 a2)
      (setf (aref r i1 i2) 0))))

(cl-lib::defresource (test-variable-resource (a1 a2) :initializer clear-variable-resource)
    (make-array (list a1 a2) :element-type t :initial-element 0))

(5am:test allocate-variable-resources "Test that we can allocate the variable resources, in excess of the pool"
      (let ((foo1 (allocate-test-variable-resource 2 3))
            (foo2 (allocate-test-variable-resource 3 2))
            (foo3 (allocate-test-variable-resource 4 3))
            (foo4 (allocate-test-variable-resource 3 4))
            (foo5 (allocate-test-variable-resource 1 1))
            (foo6 (allocate-test-variable-resource 7 2)))
        ;; sign them
        (setf (aref foo1 0 0) 1)
        (setf (aref foo2 0 0) 2)
        (setf (aref foo3 0 0) 3)
        (setf (aref foo4 0 0) 4)
        (setf (aref foo5 0 0) 5)
        (setf (aref foo6 0 0) 6)
        ;; make sure they are different
        (is (not (eq foo1 foo2)))
        (is (not (eq foo2 foo3)))
        (is (not (eq foo3 foo4)))
        (is (not (eq foo4 foo5)))
        (is (not (eq foo5 foo6)))
        ;; at this point, all resources should be in use
        (cl-lib::map-resource #'(lambda (object in-use)
                          (is in-use))
                      'test-variable-resource)
        ;; deallocate one resource and check that it was the only one marked to not be in use
        (deallocate-test-variable-resource foo4)
        (cl-lib::map-resource #'(lambda (object in-use)
                          (cond
                            ((eq foo4 object)
                             (is (not in-use))
                             (is (null (aref object 0 0)))) ; should have been cleared!
                            (t
                             (is in-use)))) ; everyone else should still be in-use
                      'test-variable-resource)
        ;; now allocate an object with a DIFFERENT size, and we should get something new
        (progfoo (allocate-test-variable-resource 7 2)
           ;; it should NOT be eq to our old foo4 (not a new one)
           (is (not (eq foo4 foo)))
           ;; and original foo4 should not be in use
           (cl-lib::map-resource #'(lambda (object in-use)
                             (if (eq object foo4)
                                 (is (not in-use))
                                 (is in-use)))
                         'test-variable-resource))

        ;; now when we allocate an object that has foo4's spec, we should get back that one
        (progfoo (allocate-test-variable-resource 3 4)
           ;; it should be eq to our old foo4 (not a new one)
           (is (eq foo4 foo))
           ;; and all should be in-use again
           (cl-lib::map-resource #'(lambda (object in-use)
                             (is in-use))
                         'test-variable-resource))))

;;
(5am:test clear-variable-resource
      (clear-test-variable-resource)
      ;; should all be unallocated
      (cl-lib::map-resource #'(lambda (object in-use)
                        (is (not in-use)))
                    'test-variable-resource))
