;; Time-stamp: <2017-04-14 13:31:25 Brad Miller(on ubuntu-vm-on-lobotomy)>     

;; This portion of CL-LIB Copyright (C) 2000-2008 Bradford W. Miller
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

(in-package :asdf)

(defsystem :cl-lib
    :depends-on (:cl-lib-essentials)
    :components
    ((:module "functions"
              :serial t
              :components
              ((:file "keywords" )
               (:file "cl-list-fns")
               (:file "cl-sets")
               (:file "cl-array-fns")
               (:file "cl-map-fns")
               (:file "cl-boolean")
               ;(:file "clos-extensions")
               (:file "strings")
               (:file "number-handling")
               (:file "files")))
     (:file "cl-lib-version")))
