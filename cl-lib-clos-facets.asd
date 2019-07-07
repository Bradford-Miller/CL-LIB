;; Time-stamp: <2019-07-07 13:36:20 Bradford Miller(on Aragorn.local)>     

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


;; (defsystem :cl-lib-clos-facets
;;     (:default-pathname "CL-LIB:packages;")
;;   :members ((:cl-lib-essentials :type :system)
;;             "clos-facets"
;;             "clos-facet-defs"
;;             ;; while debugging

;;             "clos-facets-tests")
;;   :rules ((:in-order-to :compile :all (:requires (:load :previous)))
;;           (:in-order-to :compile ("clos-facet-defs" "clos-facets-tests")
;;                         (:caused-by (:compile "clos-facets"))
;;                         (:requires (:load "clos-facets")))))
(in-package :asdf)

(defsystem :cl-lib-clos-facets
    :name "cl-lib-clos-facets"
    :serial t
    :depends-on (:cl-lib-essentials)
    :components
    ((:module "packages"
      :serial t
      :components
      ((:file "clos-facets")
       (:file "clos-facet-defs")))))
