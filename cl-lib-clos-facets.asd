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
       (:file "clos-facet-defs")
       #+5am
       (:file "clos-facets-tests")))))
