;; if FiveAM is loaded, then set up some tests.

(in-package :cl-lib-tests)
(cl-lib:version-reporter "CL-LIB-Initializations Tests" 5 17 ";; Time-stamp: <2019-10-26 20:28:02 Bradford Miller(on Aragorn.local)>" 
                         "5am testing")

;; 5.17   10/26/19 make sure we generate a new initialization name each time we run the test
;; 5.16.   2/17/19 5am testing (new)

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

(def-suite initialization-tests :description "Test suite for initializations package")

(in-suite initialization-tests)

;; modify example from our original code to allow for us to check that it works.
(defvar *test-var* 0)

(defvar *test-init-list* nil)

(test init-once
  "test that :once initialization runs once (by name)"
  (let ((test-name (string (gensym "test-init-once"))))
    (setq *test-var* 0)
    (add-initialization test-name '(setq *test-var* 1) '(:once))
    (is (= 1 *test-var*))
    (add-initialization test-name '(setq *test-var* 2) '(:once))
    (is (= 1 *test-var*))))

(test init-lists
  "test that user initialization lists work"
  (let ((test-name (string (gensym "test-init-lists"))))
    (setq *test-var* 40)
    (add-initialization test-name '(incf *test-var* 2) () '*test-init-list*)

    ;; haven't run initializations yet, so should be 40
    (is (= 40 *test-var*))
    (initializations '*test-init-list*)

    ;; now incremented
    (is (= 42 *test-var*))
    (initializations '*test-init-list*)

    ;; since we haven't reset, it shouldn't run a second time
    (is (= 42 *test-var*))
    (reset-initializations '*test-init-list*)
    (is (= 42 *test-var*))
    (initializations '*test-init-list*)
    (is (= 44 *test-var*))))
