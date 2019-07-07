;; Time-stamp: <2019-02-17 10:16:28 Bradford Miller(on Aragorn.local)>     

;; This portion of CL-LIB Copyright (C) 2000-2017 Bradford W. Miller
;; 
;; This library is free software; you can redistribute it and/or modify it under the terms of the GNU 
;; Lesser General Public License as published by the Free Software Foundation; either version 3.0 of 
;; the License, or (at your option) any later version.

;; This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
;; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;; See the GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License along with this library; 
;; if not, see <http://www.gnu.org/licenses/>.

;; miller - Brad Miller (miller@cs.rochester.edu) (now bradfordmiller@mac.com)
;; new fractured cl-lib, with multiple defsystems for each package. So the "right ones" can be added as needed.


;; NB: ADSF discourages use of logical pathnames, so we've rewritten to only use relative pathnames here.

;;; For a History of recent changes, see the file cl-lib-news in this directory.
(in-package :asdf)

(defsystem :cl-lib-essentials
  :name "cl-lib-essentials"
  :components
  ((:module PACKAGE-DEF :pathname "functions"
            :components
            ((:file "cl-lib-defpackage")))
   (:module "packages"
            :depends-on (PACKAGE-DEF)
            :components
            ((:file "initializations")
             #+5am
             (:file "initializations-tests")))

   (:module "functions"
            :depends-on (PACKAGE-DEF "packages")
            :components
            ((:file "cl-lib-essentials")
             ;; moved here because of sbcl 
             (:file "cl-extensions" :depends-on ("cl-lib-essentials"))
             (:file "clos-extensions" :depends-on ("cl-extensions"))
             ))))
