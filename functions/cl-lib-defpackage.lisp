(in-package cl-user)
(defpackage :cl-lib-initializations (:use common-lisp)  (:export #:add-initialization #:initializations #:delete-initialization #:reset-initializations #:*cold-initialization-list* #:*warm-initialization-list* #:*once-initialization-list* #:*gc-initialization-list* #:*before-cold-initialization-list* #:*after-gc-initialization-list* #:*initialization-keywords*) (:documentation "Symbolics compatible initializations package for common lisp"))
(defpackage :cl-lib-essentials (:use common-lisp :cl-lib-initializations)  (:export #:macro-indent-rule #:version-reporter #:detailed-version-reporter #:report-version #:interactive-lisp-p #:*cl-lib-version-announce-p* #:*cl-lib-version-reporter-string* #:*cl-lib-detailed-version-reporter-string*) (:documentation "Essential functions for implementing the cl-lib"))

(defparameter *cl-lib-defpackage-version*  '(cl-lib-essentials:version-reporter "CL-LIB-Defpackage" 5 18 
                                             ";; Time-stamp: <2020-09-12 17:46:00 gorbag>" 
                                             "fix version"))

;; the ugliness above (putting the defpackages first) is to support the auto-update of the timestamp on the file (default top 5 lines).

;; 5.18  1/ 3/20 firstn wasn't exported?! Also create :clos-facet-tests package
;; 5.17 11/ 8/19 occurs wasn't exported?!
;; 5.16  2/16/19 Start to add FiveAM testing capabilities
;; 5.14 12/16/11 detailed-version-reporter
;; 5.13 10/19/11 use lispworks' version of dotted-list-p
;; 5.12  7/11/08 export de-alistify, true-list-p, true-list
;; 5.10  4/23/08 lispworks - export *keyword-package*
;; 5.9   2/19/08 tail-equalp
;; 5.8   1/30/08 key-value-list-p
;; 5.7  11/26/07 documentation on packages
;; 5.5-5.6 skipped
;; 5.4  10/10/07 export getenv if necessary
;; 5.2           export slot-for-initarg

;; This portion of CL-LIB Copyright (C) 1984-2011 Bradford W. Miller and the
;;                                                Trustees of the University of Rochester
;; Some portions are Copyright (C) 2019, 2020 by Bradford W. Miller
;; 
;; This library is free software; you can redistribute it and/or modify it under the terms of the GNU 
;; Lesser General Public License as published by the Free Software Foundation; either version 3.0 of 
;; the License, or (at your option) any later version.

;; This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
;; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;; See the GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License along with this library; 
;; if not, see <http://www.gnu.org/licenses/>.


;; the first few lines are crammed together to get the version stamp
;; into the cl-lib-defpackage-version line while still defining an
;; essential package.

;; miller - Brad Miller (miller@cs.rochester.edu) (now bradfordmiller@mac.com)
;; new fractured cl-lib, with multiple defsystems for each package. So the "right ones" can be added as needed.


(defpackage :cl-lib
  (:use common-lisp :cl-lib-initializations :cl-lib-essentials)
  #+lispworks (:shadowing-import-from hcl #:getenv)
  #+lispworks (:shadowing-import-from lispworks #:dotted-list-p)
  (:documentation "A useful collection of functions that keep coming
  up in general CL programming. Note that some are loaded by their own
  system file (e.g., cl-lib-scheme-streams), so one can use only the
  base system (cl-lib-essentials) plus those parts that one really
  needs for a particular application.")
  (:export 
   ;; re-export from :cl-lib-initializations
   #:add-initialization #:initializations #:delete-initialization #:reset-initializations
   #:*cold-initialization-list* #:*warm-initialization-list* #:*once-initialization-list*
   #:*gc-initialization-list* #:*before-cold-initialization-list* #:*after-gc-initialization-list*
   #:*initialization-keywords*

   ;; re-export from :cl-lib-essentials

   #:macro-indent-rule
   #:version-reporter #:report-version #:detailed-version-reporter

   #:command-line-arg #:getenv 
   #:force-list  #:flatten  #:alistify #:de-alistify #:tail-equalp
   #:remove-n #:delete-n

   ;; clim-extensions.lisp
   #+(AND CLIM (NOT LISPWORKS))
   #:frame-pane
   
   ;;           cl-sets.lisp
   #:list-without-nulls #:cartesian-product #:cross-product 
   #:permutations #:powerset #:circular-list #:occurs
   #:firstn #:in-order-union #:fast-union #:fast-intersection

   #:seq-butlast #:seq-last #:dosequence
   #:prefix?
   #:force-string #:elapsed-time-in-seconds
   #:factorial #:round-to #:round-off
   #:extract-keyword #:truncate-keywords #:remove-keyword-arg #:key-value-list-p
   #:update-alist #:update-alist-alist #:msetq #:mlet #:while #:while-not #:let*-non-null
   #:cond-binding-predicate-to 
   #:mapc-dotted-list #:mapcar-dotted-list #:mapcan-dotted-list #:maplist-dotted-list
   #:some-dotted-list #:every-dotted-list
   #:copy-hash-table
   #:defclass-x #:defflag #:defflags #:let-maybe
   #:eqmemb #-MCL #:neq #:car-eq #:dremove #:displace #:tailpush 
   #:explode #:implode #:crush
   #:listify-string #:listify 
   #:and-list #:or-list
   #:make-variable #:variablep
   #:dofile #:copy-array
           
   #-excl #:if*
   #-excl #:*keyword-package*
   #+excl #:raw-read-char
   #+excl #:raw-peek-char
   #+excl #:raw-read-char-no-hang
   #:make-plist 
   #:make-keyword
   #:internal-real-time-in-seconds #:read-char-wait 
           
   #:flags 
   #:read-delimited-string #:mapatoms
   #:reverse-alist 
   #:true-list-p #:dotted-list-p #:progfoo #:foo #:mv-progfoo #:mv-foo #:with-rhyme

   #:get-compiled-function-name #:fast-read-char #:fast-read-file-char
           
   ;; better-errors.lisp
   #:warn-or-error #:*general-warning-list* #:*warn-or-error-cleanup-initializations*
   #:check
   #:parser-error

   ;; resources.lisp
   #-LispWorks #:defresource 
   #-LispWorks #:allocate-resource 
   #-LispWorks #:deallocate-resource 
   #-LispWorks #:clear-resource 
   #-LispWorks #:map-resource 
   #-LispWorks #:with-resource
           
   #+excl #:edit-system 

   #:alist #:true-list #:dotted-list #:comment
   #:xor #:eqv #:nand #:nor
   #:load-once #:clear-load-once
           
   ;; locatives.lisp
   #:locf #:location-contents #:locative-p #:locative

   ;; nregex.lisp
   #:regex #:regex-compile

   ;; prompt-and-read
   #+clim #:popup-read-form #+clim #:popup-error #:prompt-and-read #:prompt-for 
   #+clim #:convert-to-presentation-type #+clim #:convert-satisfies-to-presentation-type 
   #+clim #:*default-presentation-type* #+clim #:clim-prompt-for #+clim #:clim-prompt-for-with-default
   #:*suppress-clim*

   ;; clos-extensions
   #:make-load-form-with-all-slots #:determine-slot-readers #:determine-slot-writers #:determine-slot-initializers
   #:generate-legal-slot-initargs #:*load-form-quote-p-fn* #:slot-for-initarg
           
   ;; syntax
   #:add-syntax #:with-syntax #:set-syntax

   ;; scheme-streams
   #:scheme-delay #:scheme-delay-p #:scheme-force #:scheme-stream
   #:ss-head #:ss-tail #:cons-scheme-stream #:make-scheme-stream 
   #:list-scheme-stream #:rlist-to-scheme-stream #:fake-scheme-delay
   #:scheme-stream-p #:scheme-stream-head #:scheme-stream-tail
   #:scheme-stream-tail-closure-p
           
   ;; queues
   #:make-queue #:queue-elements #:empty-queue-p #:queue-front #:dequeue #:enqueue #:safe-dequeue
   ))

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


(defpackage :clos-facets
  (:use #:common-lisp #:cl-lib)
  (:shadow #:defclass #:slot-makunbound #:slot-unbound #:slot-value #:slot-boundp #:unbound-slot
           #:slot-makunbound-using-class #:slot-value-using-class #:slot-boundp-using-class)
  (:export #:defclass #:slot-makunbound #:slot-unbound #:slot-value #:slot-boundp
           #:slot-makunbound-using-class #:slot-value-using-class #:slot-boundp-using-class
           #:defslot #:deffacet #:slot-definition-facets #:slot-facet-value #:slot-facet-value-using-class
           #:clos-facets-error #:clos-facets-incompatible #:clos-facets-program-error #:value-type-decl-incompatible
           #:cardinality-bounds-incompatible #:clos-facets-cell-error #:cardinality-bounds-error #:cardinality-bounds-underrun
           #:cardinality-bounds-overrun #:value-type-error #:no-multiple-slotvalues #:unbound-slot #:unbound-slot-place
           #:clos-facets-warning #:facet-missing #:facet-missing-error #:facet-unbound

           #:no-multiple-slotvalues-instance #:no-multiple-slotvalues-place

           ;; support fns
           #:clear-slot-value-cache)
  (:documentation "See the file 'Extending CLOS with Facets.doc' for extensive documentation."))
