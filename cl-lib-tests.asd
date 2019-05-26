;; Time-stamp: <2019-02-17 15:39:23 Bradford Miller(on Aragorn.local)>     

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

(in-package :asdf)

(defsystem :cl-lib-tests
    :depends-on (:cl-lib-all :fiveam)
    :components
    ((:file "cl-lib-tests")
     (:module "packages"
              :serial t
              :components
              ((:file "initializations-tests" )))))

