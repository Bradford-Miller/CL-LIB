(in-package :clos-facets)
(cl-lib:version-reporter "CL-LIB-CLOS-Facets-Defs" 0 2 ";; Time-stamp: <2011-10-21 10:36:10 millerb>" 
                         "CVS: $Id: clos-facet-defs.lisp,v 1.3 2011/11/04 14:10:52 gorbag Exp $
;; development (fix comment)")

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

;;(deffacet name (superfacets &key all) body)

(deffacet value-type (() :all t :default t)
  "Similar to a type declaration, but allows for reasoning about the
type, and in particular, stating the type of the eqivalence class of
the slot" )

(deffacet slot-values (() :all t :default :single)
  "By default, slots are :slot-values :single, which means that they
have normal common-lisp semantics. If :slot-values :multiple, however,
the slot has multiple-values, that is, setting a slot multiple times
does NOT overwrite prior values, and one can read off the different
values (or rewrite specific values) by using the place optional
parameter on the various CLOS slot functions, such as slot-value)" )
