(cl-lib:version-reporter "CL-LIB-CLOS-Facets" 5 17 ";; Time-stamp: <2019-10-27 16:39:13 Bradford Miller(on Aragorn.local)>" 
                         "CVS: $Id: clos-facets.lisp,v 1.2 2008/05/03 17:42:02 gorbag Exp $
;; documentation")

;; 5.17  10/27/19 minor fixes to 5.16
;; 5.16   3/ 3/19 finish implementation of multiple values (to pass tests)
;; 0.3    5/18/07 parial implementation of multiple values

;; This portion of CL-LIB Copyright (C) 2003-2019 Bradford W. Miller
;; 
;; This library is free software; you can redistribute it and/or modify it under the terms of the GNU 
;; Lesser General Public License as published by the Free Software Foundation; either version 3.0 of 
;; the License, or (at your option) any later version.

;; This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
;; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;; See the GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License along with this library; 
;; if not, see <http://www.gnu.org/licenses/>.

;; because a number of the following symbols likely collide with common-lisp definitions, you will probably want to copy
;; and paste the following into your defpackage
#||
 (:shadowing-import-from #:clos-facets
                         #:defclass #:slot-makunbound #:slot-unbound #:slot-value #:slot-boundp #:unbound-slot
;; in some cases you may also need the following (e.g., you :use the clos class in LispWorks)
                         #:slot-makunbound-using-class #:slot-value-using-class #:slot-boundp-using-class
  )
||#
;; alternatively you can explicitly reference this version using clos-facets:defclass for instance, for every relevant reference.

(in-package :clos-facets)

;; Our database to allow us to track what facets have been defined, etc.
;; For now, since we have relatively few facets, Alists should be sufficient (For something like a hash table to make sense
;; we need to expect on the order of 100 entries - I don't expect we'll have more than a couple dozen facets defined).

(defvar *facet-definition-alist* nil "Alist of (facet-name . definition entry) fields for all defined facets (see deffacet)")

(defstruct facet-definition 
  "Internal structure for defining a facet"
  (super-facets nil :type list)
  (option-value-pairs nil :type list) ; right now, only :all and :default are supported
  ;; list of the slots that this facet is valid for, if :all option is non-nil on option-value-pairs,
  ;; then all slots support this facet (useful for system-defined facets, like value-type)
  (supported-slots nil :type list) 
  (comment "" :type string)
  (facet-body nil :type list)) ; the guts of the frob

(define-condition clos-facets-error (error)
  ()
  (:documentation "All clos-facets package related errors inherit from this condition"))

(define-condition clos-facets-incompatible (clos-facets-error)
  ()
  (:documentation "Specialization of Clos-Facets-Error where there is
some incompatibility (constraint conflict) with some other
declaration. Generally this is due to some child class having a facet
declaration that is not a proper subtype of an ancester class."))

(define-condition clos-facets-program-error (clos-facets-error program-error)
  ()
  (:documentation "Specialization of program-error for facets"))

(define-condition value-type-decl-incompatible (clos-facets-incompatible clos-facets-program-error)
  ()
  (:documentation "the value-type declared on the slot is not
compatible with the value-type of the slot on a parent
class. Specifically, it must be the same or a proper subtype."))

(define-condition cardinality-bounds-incompatible (clos-facets-incompatible clos-facets-program-error)
  ()
  (:documentation "The cardinality bounds are not a proper subinterval of some ancestor declaration."))

(define-condition clos-facets-cell-error (clos-facets-error cell-error)
  ((instance :initarg :instance :reader clos-facets-cell-error-instance)
   (class :initarg :class :reader clos-facets-cell-error-class)
   (facet :initarg :facet :reader clos-facets-cell-error-facet)
   (operation :initarg :operation :reader clos-facets-cell-error-operation)
   (new-value :initarg :new-value :reader clos-facets-cell-error-new-value))
  (:documentation "Specialization of cell-error for faceted slots. 
The type cell-error consists of error conditions that occur during a location access. 
The name of the offending cell is initialized by the :name initialization argument to make-condition, 
and is accessed by the function cell-error-name.")) 

(define-condition facet-missing-error (clos-facets-cell-error)
  ()
  (:documentation "Signalled when a facet was expected"))
  
(define-condition cardinality-bounds-error (clos-facets-cell-error)
  ()
  (:documentation "Either fewer or more values have been declared than the bounds allow"))

(define-condition cardinality-bounds-underrun (cardinality-bounds-error)
  ()
  (:documentation "Too few values have been declared for the slot's cardinality"))

(define-condition cardinality-bounds-overrun (cardinality-bounds-error)
  ()
  (:documentation "Too may values have been declared for the slot's cardinality"))

(define-condition value-type-error (clos-facets-cell-error)
  ()
  (:documentation "an attempt to directly write a value to a cell that conflicts with the declared value-type."))

(define-condition no-multiple-slotvalues (clos-facets-cell-error)
  ((place :initarg :place :reader no-multiple-slotvalues-place :documentation "The place used on a slot that is single valued"))
  (:documentation "An attempt was made to read or write a specific
slotvalue (using place on a supported function such as a slot
accessor), when the slot was declared or defaulted to be single
valued."))

(define-condition unbound-slot (clos-facets-cell-error common-lisp:unbound-slot)
  ((place :initarg :place :reader unbound-slot-place :documentation "The place (within a multiple-value slot) that is unbound"))
  (:documentation "An attempt was made to read an unbound multiple value, generally using the place argument"))

(define-condition unbound-facet (clos-facets-cell-error)
  ()
  (:documentation "An attempt was made to read a facet that is defined for the slot, but is not bound."))

(define-condition clos-facets-warning (warning)
  ()
  (:documentation "All clos-facets package related warnings inherit from this warning"))

;; internal functions
(defun defined-facets ()
  "Return a list of all the facets that have been defined"
  (mapcar #'car *facet-definition-alist*))

(defun parse-slot-specifier (slot-specifier)
  "Parse the slot specifier, sorting out the various options based on our interests in defclass, below."
  (declare (values slot-name readers writers accessors facets rest))
  (let ((unparsed-remainder (cdr slot-specifier))
        (slot-name (car slot-specifier))
        readers
        writers
        accessors
        facets
        rest)
    (while unparsed-remainder
      (case (car unparsed-remainder)
        (:reader
         (push (cadr unparsed-remainder) readers))
        (:writer
         (push (cadr unparsed-remainder) writers))
        (:accessor
         (push (cadr unparsed-remainder) accessors))
        (t
         (cond 
           ((member (car unparsed-remainder) (defined-facets))
            (setq facets (nconc (list (car unparsed-remainder) (cadr unparsed-remainder)) facets)))
           (t
            (setq rest (nconc (list (car unparsed-remainder) (cadr unparsed-remainder)) rest))))))
      (setq unparsed-remainder (cddr unparsed-remainder)))
    (values slot-name readers writers accessors facets rest)))

;; currently we aren't type specializing and we probably should (on the class formal)
(defun construct-facetized-slot-accessors (class-name slot-name 
                                           user-readers actual-reader 
                                           user-writers actual-writer 
                                           user-accessors actual-accessor
                                           facets)
  "Internal function that does the grunt work of putting together the code that will act as the 
reader, writer, or accessor for slots on the class being defined."
  ;; the user-readers/writers etc. are the symbols the user has defined, the actual reader/writer etc. is the gentemp we handed to
  ;; the original defclass macro. So now we can hook that version (if we want to) or construct our own version afresh (e.g., for
  ;; multiple-values)
  (declare (ignore class-name)) ;; for now
  (let ((class-formal (gensym))
        (writer-formal (gensym))
        (place-formal (gensym))
        (multiple-value-p (equal (extract-keyword :slot-values facets) :multiple))
        deferred-code)
    (flet ((make-accessor-internal (user-functions real-function writer-p accessor-p)
             (when user-functions
               (cond
                ((and real-function (not multiple-value-p))
                 ;; just call the defclass version; later on we may add additional code.
                 (dolist (fn user-functions)
                   (when accessor-p
                     (push `(defmethod (setf ,fn) (,writer-formal ,class-formal)
                              (setf (,real-function ,class-formal) ,writer-formal))
                           deferred-code))
                   (push `(defmethod ,fn (,@(if writer-p `(,writer-formal)) ,class-formal)
                            (,real-function ,@(if writer-p `(,writer-formal)) ,class-formal)) ; for now
                         deferred-code)
                   (push `(defgeneric ,fn (,@(if writer-p `(,writer-formal)) ,class-formal))
                         deferred-code)))
                (multiple-value-p
                 ;; we can't actually use the defclass version because it doesn't handle the optional place
                 (dolist (fn user-functions)
                   (when accessor-p
                     (push `(defmethod (setf ,fn) (,writer-formal ,class-formal &optional ,place-formal)
                              (setf (clos-facets:slot-value ,class-formal ',slot-name ,place-formal) ,writer-formal))
                           deferred-code))
                   (push `(defmethod ,fn (,@(if writer-p `(,writer-formal)) ,class-formal &optional ,place-formal)
                            ,(if writer-p
                               `(setf (clos-facets:slot-value ,class-formal ',slot-name ,place-formal) ,writer-formal)
                               `(clos-facets:slot-value ,class-formal ',slot-name ,place-formal)))
                         deferred-code)
                   (push `(defgeneric ,fn (,@(if writer-p `(,writer-formal)) ,class-formal &optional ,place-formal))
                         deferred-code)))
                (t
                 ;; no defclass version ?!
                 (warn "Incorrect expansion of clos-facets defclass form for user functions: ~S" user-functions))))))

      (make-accessor-internal user-readers actual-reader nil nil)
      (make-accessor-internal user-writers actual-writer t nil)
      (make-accessor-internal user-accessors actual-accessor nil t))

    `(progn ,@deferred-code)))

;; when we define new classes, they all inherit from this class
(common-lisp:defclass facet-support-class ()
  ((class-slot-facet-value-alist-alist :initform nil 
                                       :accessor class-slot-facet-value-alist-alist
                                       :allocation :class ;; note that every new class should have their own
                                       :documentation "An alist of slots whose values are themselves 
alists of facets and their values at class definition time")
   (slot-facet-value-alist-alist :initform nil :accessor slot-facet-value-alist-alist
                                 :documentation "An alist of slots whose values are themselves alists of facets and their dynamic
values, initialized from class-slot-facet-value-alist-alist")))

;; Patches of CL/CLOS functions... (first part - more toward end of file)
;; (For defclass - to support facets at all! Yes, we could be using the MOP here, but while that would give us flexibility in 
;; some ways it is at the cost of compatibility - most CLs don't yet support the MOP (fully or correctly). We will be as 
;; MOPlike as possible, while avoiding anything that requires that slots be represented as objects (which is not supported in 
;; MCL, at least, as of 5.0)
(defmacro defclass (class-name superclass-names slot-specifiers &rest class-options)
  "A shadowing of the usual implementation of defclass, that intercepts the slot-specifiers in order to support facets.
Note that it is possible that slot accessors will not be compatible with the host implementation for classes not defined
using this macro."

  ;; the main things that have to be pulled apart are 
  ;; slot specifiers - we want to yank out the facet related specifications and handle them separately... this also
  ;; lets us ultimately call the real defclass to do the work of defining the class - without having to have it get bogged
  ;; down in implemented facets using the MOP machinery (not that this is particularly bad, but this may be simpler
  ;; particularly for lisps without a full MOP).

  ;; The next thing we have to do is record what the user thinks are going to be the accessor functions for the slots. We'll
  ;; capture those with our own functions, reassign the ones defclass sees, so we can use them internally. Here, there may be
  ;; some implementation-specific code, depending on if an implementation implements accessors, readers, and writers as generic
  ;; functions or non CLOS functions (standard functions).

  ;; Next, we'll implement the various accessor functions, which will obey any needed facet protocols specified.

  ;; finally, we'll use the common-lisp version of defclass to implement the modified class.

  ;; lets get started!

  (let ((user-slot-specifiers slot-specifiers)
        processed-slot-specifiers
        facet-specifiers
        deferred-code)
        
    ;; first, we want to look at each slot specifier, and separate out the facets, the readers, the writers, and the accessors.
    (dolist (slot-specifier user-slot-specifiers)
      (let (actual-reader
            actual-writer
            actual-accessor)
        (mlet (slot-name user-readers user-writers user-accessors facets rest)
              (parse-slot-specifier slot-specifier)
              ;; create an internal name for any readers, writers and accessors to specify to the underlying cl:defclass
          (when user-readers
            (setq actual-reader (gentemp (format nil "~A-READER" slot-name))))
          (when user-writers
            (setq actual-writer (gentemp (format nil "~A-WRITER" slot-name))))
          (when user-accessors
            (setq actual-accessor (gentemp (format nil "~A-ACCESSOR" slot-name))))

          ;; save off the facet values
          (update-alist slot-name
                       (alistify facets)
                       facet-specifiers)
          ;; create our version of the accessors, and push the result onto deferred code
          (push (construct-facetized-slot-accessors class-name slot-name user-readers actual-reader user-writers actual-writer 
                                                    user-accessors actual-accessor facets)
                deferred-code)
              
          ;; build up the slot specifier that goes to cl:defclass
          (push `(,slot-name
                  ,@(if actual-reader
                        `(:reader ,actual-reader))
                  ,@(if actual-writer
                        `(:writer ,actual-writer))
                  ,@(if actual-accessor
                        `(:accessor ,actual-accessor))
                  ,@rest)
                processed-slot-specifiers))))

  `(progn 
    (common-lisp:defclass ,class-name (,@superclass-names facet-support-class)
      ((class-slot-facet-value-alist-alist
        :allocation :class
        :documentation "Double alist, first of slotnames then facetnames to the value defined."
        :initform ',facet-specifiers) ;; note that every new class should have their own
        ,@(nreverse processed-slot-specifiers))
      ,@class-options)
    ,@deferred-code)))

;;
;; Facets API
;;

;; Defclass itself appears above under patched functions
;;

(defun defslot (slot-name super-slots &key facet-initializations &allow-other-keys)
  "Defslot allows one to define a hierarchy of slots, where slots are semantic relationships between class instances and
attributes (typically, other class instances). The current definition is pretty limited; the desired direction would, for
instance, allow us to define a general \"ancestor\" slot, and allow that to be a super-slot of, say, \"maternal-grandfather\",
specifying that the maternal grandfather is single-valued, but it's value is one of the multi-valued ancestor slot.

The current implementation only allows us to state that a slot supports set-oriented facets (for instance), and alas we
haven't (yet) implemented the :slot-type facet which would allow us to link such information to a \"kind\" of slot rather than
to a specific slot name. We will get to these more interesting applications of this concept later.

Note however, that defslot does make a global declaration about a particular slot name (within a package) as having a given
semantics. That is, unlike casual use of slotnames as relative to a class, defslot says that anytime a particular slotname
appears in any class, it has the same semantic meaning. The facet-initializations, if specified, is a list each element of 
which is either the name of a facet to be supported by the slot-name, or a pair, the second element of which is the value of 
the facet whenever it appears in a class. Setting a different value local to a class will signal an error; this allows one to
for instance, require that all decendants of a particular slot-name support multiple values."
  (declare (ignore slot-name super-slots facet-initializations))
  (warn "Defslot is not yet implemented"))

(defmacro deffacet (facet-name (super-facets &key (all nil) (default nil)) &optional unevaluated-comment &body body)
  "The primary macro for creating a new facet. The body is invoked on any write to a slot (including initialization). The following
formals are bound on invocation, and may be used within the body (as lexical): facet-form, slotname, current-slotvalue,
new-slotvalue. The body should evaluate new-slotvalue in light of the current-slotvalue and the facet-form (the value of the
facet when the class was defined - this is what was in the defclass form as the argument to this facet), and either allow the 
write by returning, or signal appropriately (q.v.)

:all implies all slots support the facet, with the :default value."
  (unless (stringp unevaluated-comment)
    (push unevaluated-comment body)
    (setq unevaluated-comment ""))
  `(update-alist ,(make-keyword facet-name) 
                 (make-facet-definition :super-facets ',super-facets 
                                        :option-value-pairs '((:all ,all) (:default ,default))
                                        :comment ',unevaluated-comment
                                        :facet-body '(lambda (facet-form slotname current-slotvalue new-slotvalue) 
                                                      ,@body))
                 *facet-definition-alist*))

(defun get-facet-definition (facet-name)
  (cdr (assoc (make-keyword facet-name) *facet-definition-alist*)))

(defgeneric slot-supports-facetp-using-class (class object slotname facetname)
  )

(defmethod slot-supports-facetp-using-class ((class t) (object t) slotname facetname)
  (facet-missing class object slotname facetname 'slot-supports-facetp))

(defmethod slot-supports-facetp-using-class ((class t) (object facet-support-class) slotname facetname)
  (let ((facet-definition (get-facet-definition facetname)))
    (unless facet-definition
      (facet-missing class object slotname facetname 'slot-supports-facetp))
    (or (member slotname (facet-definition-supported-slots facet-definition))
        (cadr (assoc :all (facet-definition-option-value-pairs facet-definition))))))

(defun slot-supports-facetp (object slotname facetname)
  "Defined in terms of slot-supports-facetp-using-class"
  (slot-supports-facetp-using-class (class-of object) object slotname facetname))

(defgeneric slot-definition-facets (slot-definition)
  (:documentation "return the facets defined for the slot-definition"))

(defgeneric slot-facet-value-using-class (object-class object slotname facetname)
  (:documentation "Returns the value of the facet facetname on the
slot slotname for this class-object. The default primary method signals an error of type clos-facets-error if no such facet 
is defined for this slot. If the facet is unbound for this slot, the default primary method signals an error of type 
facet-unbound."))

(defmethod slot-facet-value-using-class ((object-class t) (object t) slotname facetname)
  (facet-missing object-class object slotname facetname 'slot-facet-value))

(defmethod slot-facet-value-using-class ((object-class t) (object facet-support-class) slotname facetname)
  (unless (slot-supports-facetp-using-class object-class object slotname facetname)
    (facet-missing object-class object slotname facetname 'slot-facet-value))
  (let* ((facet-alist (cdr (assoc slotname (slot-facet-value-alist-alist object))))
         (facet-entry (assoc facetname facet-alist)))
    (if facet-entry
        (cdr facet-entry)
        ;; check to see if there is a default available
        (let* ((facet-definition (get-facet-definition facetname))
               (all-slots-p (cadr (assoc :all (facet-definition-option-value-pairs facet-definition))))
               (default-value-entry (if all-slots-p 
                                        (assoc :default (facet-definition-option-value-pairs facet-definition)))))
          (if default-value-entry
              (cdr default-value-entry)
              (facet-unbound object-class object slotname facetname))))))

(defun slot-facet-value (object slotname facetname)
  "Returns the value of the facet facetname on the slot slotname for the object. 
Implemented in terms of slot-facet-value-using-class"
  (slot-facet-value-using-class (class-of object) object slotname facetname))

(defun clear-slot-value-cache ()
  "used to clear our cache of slot value values"
  ;; not yet used
  (values))

(defgeneric facet-missing (instance-class instance slot-name facet-name operation &optional new-value)
  (:documentation "Patterned on slot-missing - invoked when an attempt is made to access a facet in an object 
where the name of the facet is not a name of a facet supported on that slot on that class. The default method signals
an error (clos-facets-error). The generic function is not intended to be called by programmers. Programmers may write methods
for it."))

(defmethod facet-missing ((instance-class t) instance slot-name facet-name operation &optional new-value)
  (error 'facet-missing-error 
         :instance instance
         :class instance-class 
         :name slot-name 
         :facet facet-name 
         :operation operation 
         :new-value new-value))

(defgeneric facet-unbound (class instance slot-name facet-name)
  (:documentation "Patterned on slot-unbound - invoked when an unbound facet is read. The default method signals an error of type 
unbound-facet. The generic function facet-unbound is not intended to be called by programmers, but programmers may write methods 
for it."))

(defmethod facet-unbound ((class t) instance slot-name facet-name)
  (error 'unbound-facet :instance instance :class class :name slot-name :facet facet-name))

;;;; testing facet validity
;; 
;; Facet validity testing is done on any write to a slot. This happens at two times
;;  When creating an object, as a method on shared-initialize 
;;   (so it is also colled by the primary methods for initialize-instance, reinitialize-instance, 
;;    update-instance-for-different-class, and update-instance-for-redefined-class)
;;   NB: this means that if a programmer does NOT use the system supplied primary function or otherwise calls shared-initialize in
;;       these circumstances, then they must also hook their own functions to invoke facet validity tests, mutis mutandis.
;;
;;  When changing the value of a slot on an object after it was created. Because reader and writer methods (provided by the 
;;  implementation) all use slot-value, it is slot-value

;; more patches to CLOS functions

;; the following patch existing functions in order to support :slot-values :multiple. 
;; Note that for the most part, facets should not require such "deep" support, however, in this case the functions need 
;; to take the additional "place" argument, preventing simply, e.g., adding methods for slot-value.
;;
;; Multiple values are represented using lists of values. The first in the list is the last one written 
;; (that is, they appear in reverse order), and the 0th place is therefore the last element of the list.
;; (This implementation is subject to some debate, but at least in my own code, I found that the last place was the most frequently
;; accessed, thus the preference of putting it first in the list to make it the easiest to get to).

(defmethod shared-initialize :after ((instance facet-support-class) slot-names &rest initargs &key &allow-other-keys)
  "After constructing a facet-support-class instance, initialize the slot-facet-value-alist-alist"
  (setf (slot-facet-value-alist-alist instance) (class-slot-facet-value-alist-alist instance)))

(defun multiple-slot-values-p (class instance slot-name)
  (equal (handler-case (slot-facet-value-using-class class instance slot-name :slot-values)
           (unbound-facet ()))
         :multiple))

(defgeneric slot-makunbound-using-class (class instance slot-name &optional place)
  (:documentation "implements slot-makunbound - see the AMOP"))

(defmethod slot-makunbound-using-class ((class t) instance slot-name &optional place)
  (let ((multiple-value-p (multiple-slot-values-p class instance slot-name)))
    (cond
      ((and place (not multiple-value-p))
       (error 'no-multiple-slotvalues :name slot-name :instance instance :place place))
      ((not multiple-value-p)
       (clos:slot-makunbound-using-class class instance slot-name))
      ((not place)
       ;; just "unbind" the last value
       (handler-case (pop (clos:slot-value-using-class class instance slot-name))
         (unbound-slot ()))) ; we don't care if it's already unbound
      (t
       ;; find the particular value and delete it
       (handler-case  (let ((values (clos:slot-value-using-class class instance slot-name)))
                        (setf (clos:slot-value-using-class class instance slot-name) (delete-n values place)))
         (unbound-slot ())))))) ; if the slot is unbound, so is this place


(defun slot-makunbound (instance slot-name &optional place)
  "Restores a slot in an instance to the unbound state. Calls slot-missing if no slot of the given name exists. 
Implemented using slot-makunbound-using-class. If multi-valued, then place is used, and making a place unbound causes it to be removed if there are subsequent places, i.e., if places 1-6 exist, and place 4 is unbound, then the value of place 5 is now accessible only at place 4, etc."
  (slot-makunbound-using-class (class-of instance) instance slot-name place))

(defgeneric slot-unbound (class instance slot-name &optional place)
  (:documentation "Called when an unbound slot is read in an instance whose metaclass is standard-class. 
The default method signals an error of condition type unbound-slot (with slots :name, naming the slot that was unbound, and :instance which is the instance that had the unbound slot). The function is called only by the function slot-value-using-class."))

(defmethod slot-unbound (class instance slot-name &optional place)
  (error 'unbound-slot :place place :name slot-name :instance instance))

(defgeneric slot-value-using-class (class object slot-name &optional place)
  (:documentation "Implementation for slot-value and setf of slot-value."))

(defmethod slot-value-using-class ((class t) object slot-name &optional place)
  (cond 
   ((and place (multiple-slot-values-p class object slot-name))
    (if (slot-boundp-using-class class object slot-name place)
      (nth place (common-lisp:slot-value object slot-name))
      (slot-unbound class object slot-name place)))
   ((multiple-slot-values-p class object slot-name)
    ;; just return the first place
    (if (slot-boundp-using-class class object slot-name 0)
      (car (common-lisp:slot-value object slot-name))
      (slot-unbound class object slot-name place)))
   (t
    (common-lisp:slot-value object slot-name))))

(defmethod (setf slot-value-using-class) (new-value (class t) object slot-name &optional place)
  (flet ((do-set (new)
           (if (common-lisp:slot-boundp object slot-name)
             (setf (common-lisp:slot-value object slot-name)
                   (nconc (list new) (common-lisp:slot-value object slot-name))) ; add to beginning of list (see comments above) 10/26/19
             (setf (common-lisp:slot-value object slot-name) (list new)))))

    (cond
     ((and place (slot-boundp-using-class class object slot-name place))
      ;; replace existing value
      (setf (nth place (common-lisp:slot-value object slot-name)) new-value))
     ;; check if there are *some* values there
     ((and place (common-lisp:slot-boundp object slot-name))
      ;; ok, we have to extend the list to be place-1 long, then we can append.
      (let ((sv (common-lisp:slot-value object slot-name)))
        (while (< (1+ (length sv)) place)
          (setq sv (nconc sv (list :unset-value))))
        (setf (common-lisp:slot-value object slot-name) sv)
        (do-set new-value)))
     (t
      (do-set new-value)))))

(defun slot-value (object slot-name &optional place)
  "Calls slot-missing if there is no slot of the given name. Implemented using slot-value-using-class. Setf'able"
  (slot-value-using-class (class-of object) object slot-name place))

(defsetf slot-value (object slot-name &optional place) (new-value)
  `(setf (slot-value-using-class (class-of ,object) ,object ,slot-name ,place) ,new-value))

(defgeneric slot-boundp-using-class (class instance slot-name &optional place)
  (:documentation "implements slot-boundp - see the AMOP"))

(defmethod slot-boundp-using-class ((class t) instance slot-name &optional place)
  (let ((multiple-value-p (multiple-slot-values-p class instance slot-name)))
    (cond
      ((and place (not multiple-value-p))
       (error 'no-multiple-slotvalues :name slot-name :instance instance :place place))
      ((or (not multiple-value-p) (not place))
       (clos:slot-boundp-using-class class instance slot-name)) ; if place not spec, then any value means the last value is bound
      (t
       ;; anything less than the length of the list of values is bound, but check the slot is bound first.
       (and (clos:slot-boundp-using-class class instance slot-name)
            (< place (list-length (clos:slot-value-using-class class instance slot-name))))))))

(defun slot-boundp (instance slot-name &optional place)
  "Tests if a specific slot in an instance is bound, returning true or false. Can be used in :after methods on initialize instance. Implented using slot-boundp-using-class."
  (slot-boundp-using-class (class-of instance) instance slot-name place))


(pushnew :cl-lib-clos-facets *features*)
