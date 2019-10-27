(in-package :clos-facets)

(cl-lib:version-reporter "CL-LIB-CLOS-Facets-Tests" 5 17 
                         ";; Time-stamp: <2019-10-26 18:14:11 Bradford Miller(on Aragorn.local)>" 
                         "CVS: $Id: clos-facets-tests.lisp,v 1.3 2011/11/04 14:10:52 gorbag Exp $
;; development - note warnings on compile are OK")

;; This portion of CL-LIB Copyright (C) 2003-2008 Bradford W. Miller
;; 
;; This library is free software; you can redistribute it and/or modify it under the terms of the GNU 
;; Lesser General Public License as published by the Free Software Foundation; either version 3.0 of 
;; the License, or (at your option) any later version.

;; This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
;; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;; See the GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License along with this library; 
;; if not, see <http://www.gnu.org/licenses/>.

;; 5.17 10/26/19 Move definitions to cl-lib-tests package instead of
;;               clos-facets (note that the package should support that)

;; test facets library

;; some hack classes that illustrate facet uasage

;; mumble lets us test the basics: value-type and multiple slot-values.

;; convert to 5am 3/3/19 BWM
(in-package :cl-lib-tests)

(deftype hack-valid-symbol () (member 'a 'b 'c 'd 'e))

(defclass mumble ()
  ((mumble-slota :initarg :a :accessor slota :value-type hack-valid-symbol) ; check value-type
   (mumble-slotb :initarg :b :reader slotb-reader :writer slotb-writer ; check readers and writers
                 :slot-values :single :value-type hack-valid-symbol)
   ;; check many-values
   (mumble-slotc :initarg :c :accessor slotc :slot-values :multiple :value-type hack-valid-symbol)))

;; someplace to put instances we will create
(defvar *frotz* ())
(defvar *foo* ())
(defvar *bar* ())
(defvar *bletch* ())

;; frotz adds cardinality checks

;; foo and bar check slot inverses

;; bletch makes use of denotational functions, testing everything together.

(eval-when (:compile-toplevel)
  (format *error-output* "Expect two compiler warnings when compiling facet-tester:
;;;*** Warning in (SUBFUNCTION 3 CLOS-FACETS::FACET-TESTER): CLOS-FACETS::SLOTB-READER is called with the wrong number of arguments: Got 2 wanted 1
;;;*** Warning in (SUBFUNCTION 3 CLOS-FACETS::FACET-TESTER): (SETF CLOS-FACETS::SLOTA) is called with the wrong number of arguments: Got 3 wanted 2
--- this is due to the test intentionally attempting to perform a multi-value operation on a single value slot."))

(def-suite facet-tests :description "tests for the facet functions")

(in-suite facet-tests)

(defvar *mumble* )

(test basic-facet
  (setq *mumble* (make-instance 'cl-lib-tests::mumble :a 'a :b 'b))
  (is (equal (cl-lib-tests::slota *mumble*) 'a))
  (is (equal (cl-lib-tests::slotb-reader *mumble*) 'b)))

(test facet-access
  (slotb-writer 'e *mumble*)
  (setf (slota *mumble*) 'd)
  (is (equal (slota *mumble*) 'd))
  (is (equal (slotb-reader *mumble*) 'e)))

(test facet-multi-value
  ;;fill up the multi-value slot
  (setf (slotc *mumble*) 'a)
  (setf (slotc *mumble*) 'b)
  (setf (slotc *mumble*) 'c)
  (is (slot-boundp *mumble* 'mumble-slotc 2))
  (is (slot-boundp *mumble* 'mumble-slotc 0))
  (is (equal (slotc *mumble* 0) 'c))
  (is (equal (slotc *mumble* 1) 'b))
  (is (equal (slotc *mumble* 2) 'a))
  (is (equal (slot-value *mumble* 'mumble-slotc 2) 'a)))

(test facet-multi-value-change
  (setf (slotc *mumble* 1) 'd)
  (is (equal (slotc *mumble* 0) 'c))
  (is (equal (slotc *mumble* 1) 'd))
  (is (equal (slotc *mumble* 2) 'a)))

(test facet-multi-value-unbind-one
  (slot-makunbound *mumble* 'mumble-slotc 0)
  (is (equal (slotc *mumble* 0) 'd))
  (is (equal (slotc *mumble* 1) 'a))
  (is (etest unbound-slot (slotc *mumble* 2))))

(test facet-value-type-errors
  (is (etest value-type-error (slotb-writer 'g *mumble*)))
  (is (etest value-type-error (setf (slota *mumble*) 7)))
  (is (etest value-type-error (setf (slotc *mumble*) 9)))
  (is (etest unbound-slot (slotc *mumble* 8)))
  (is (etest no-multiple-slotvalues (slotb-reader *mumble* 2))) ;; should give an error on compile!
  (is (etest no-multiple-slotvalues (setf (slota *mumble* 3) 'c)))) ;; should give an error on compile! (wrong number of args - slota is not multi-valued)

