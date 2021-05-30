(cl-lib:version-reporter "CL-LIB-Tests" 5 18 ";; Time-stamp: <2020-01-04 17:12:45 Bradford Miller(on Aragorn.local)>" 
                         "5am testing initializations")

(defpackage :CL-LIB-TESTS
  (:shadowing-import-from :clos-facets
                          #:defclass #:slot-makunbound #:slot-unbound #:slot-value #:slot-boundp #:unbound-slot
                          #:slot-makunbound-using-class #:slot-value-using-class #:slot-boundp-using-class)

  (:use :Common-lisp :it.bese.FiveAM :cl-lib :clos-facets)
  (:export #:etest))

(in-package :cl-lib-tests)

;; 5.18  1/ 3/20 Fix some symbol/pacakge issues
;; 5.17 10/26/19 Typo in comments
;; 5.16  3/ 1/19 New!

;;; Copyright (C) 2019, 2020 by Bradford W. Miller (bradfordmiller@mac.com)

;; This library is free software; you can redistribute it and/or modify it under the terms of the GNU 
;; Lesser General Public License as published by the Free Software Foundation; either version 3.0 of 
;; the License, or (at your option) any later version.

;; This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
;; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;; See the GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License along with this library; 
;; if not, see <http://www.gnu.org/licenses/>.

;; miller - Brad Miller (bradfordmiller@mac.com)

;; note that this function would only be loaded if cl-lib-tests.asd
;; is. It requires FiveAM, which is distributed separately. See:

;; https://common-lisp.net/project/fiveam/

(defun test-cl-lib-all ()

  ;; basic functions
  (run! 'function-tests)

  ;; package defined in cl-lib-essentials.asd
  (run! 'initialization-tests)

  ;; package-defined-in cl-lib-clos-facets.asd
  (run! 'facet-tests)
  ;(clos-facets::facet-tester t) 

  ;; package defined in cl-lib-locatives.asd
  ;;  (cl-lib::test-setters) ; old version (pre-5am)
  (run! 'cl-lib::locatives)

  ;; package defined in cl-lib-resources.asd
  (run! 'cl-lib::resources-tests)
  
  (lispdoc::generate-docfiles)) ; not really a test, but generates the documentation needed for a release


