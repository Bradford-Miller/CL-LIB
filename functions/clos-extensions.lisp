(in-package cl-lib)
(version-reporter "CL-LIB-FNS-CLOS" 5 16 ";; Time-stamp: <2019-03-16 17:57:22 Bradford Miller(on Aragorn.local)>" 
                  "update documentation descriptions")

;; 5.16    3/16/19   Expand some of the description fields before releasing to github
;; 5.0     5/ 3/08   make generate-inherited-slot-definer and generate-inherited-slot-writer more flexible
;;                   to handle cases of objects as arguments

;; Some portions Copyright (C) 2019 Bradford W. Miller
;; This portion of CL-LIB Copyright (C) 1984-2008 Bradford W. Miller and the
;;                                                Trustees of the University of Rochester
;; 
;; This library is free software; you can redistribute it and/or modify it under the terms of the GNU 
;; Lesser General Public License as published by the Free Software Foundation; either version 3.0 of 
;; the License, or (at your option) any later version.

;; This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
;; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;; See the GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License along with this library; 
;; if not, see <http://www.gnu.org/licenses/>.


(DEFMACRO DEFCLASS-X (TYPE SUPERTYPES SLOTS . STUFF)
  "Extended defclass, also creates a TYPE-P function and MAKE-TYPE
function, like defstuct did."
  `(eval-when (compile load eval)
     (DEFCLASS ,TYPE ,SUPERTYPES ,SLOTS ,@STUFF)
     (DEFUN ,(INTERN (CONCATENATE 'STRING (STRING TYPE) "-P")) (TERM)
       (TYPEP TERM ',TYPE))
     (DEFUN ,(INTERN (CONCATENATE 'STRING "MAKE-" (STRING TYPE))) (&REST ARGS)
       (APPLY 'MAKE-INSTANCE ',TYPE ARGS))))

(macro-indent-rule defclass-x (like defclass))

;; Mostly, useful tools out of the Art of the MOP, or modifications.

(defvar *debug-clos-extensions* nil)

(defun make-load-form-with-all-slots (object &optional environment)
  (declare (ignore environment))
  (let ((new-instance (gentemp))
        (*print-readably* t)) ; for coming up with objects to fill in the slots
    `(let ((,new-instance (make-instance ',(type-of object)
                            ,@(generate-inherited-slot-definer object))))
       ,@(generate-inherited-slot-writer new-instance object)
       ,new-instance)))

(defun sample-load-form-quote-p-fn (x)
  "default function for creating load-forms"
  (cond
    ((or (typep x 'standard-object) ; handle the case where we need to construct the slot value itself
         (typep x 'structure-object))
     'load-form)
    ((consp x)
     'list)
    ((atom x)
     'quote)
    (t
     'self)))

(defparameter *load-form-quote-p-fn* #'sample-load-form-quote-p-fn
  "can be elaborated upon by the user, e.g., if it 
returns a function, the function is called on the argument
\(this sets it up for splicing into the load-form as the value of the slot)

I did this instead of making process-argument a generic function to make it easier to overload the 
\"sample\"-load-form-quote-p-fn
 - you can supply your own load-form-quote-p-fn and bind it to the parameter. If process-argument were generic, then
overloading the predefined processing would generate errors.")

;; Yeah, it's uglier. I'm sure there's a better way, but I haven't thought of it (yet).
(defun process-argument (arg-value)
  (let ((process-type (funcall *load-form-quote-p-fn* arg-value)))
    (case process-type
      (load-form
       (make-load-form arg-value))
      (quote
       (list 'quote arg-value))
      (list
       `(list ,@(mapcar #'process-argument arg-value)))
      (self
       arg-value)
      (otherwise
       (funcall process-type arg-value)))))

(defun generate-inherited-slot-definer (object)
  (let ((class (class-of object)))
    (mapcan #'(lambda (slot)
                (if (and (clos:slot-definition-initargs slot)
                         (slot-boundp object (clos:slot-definition-name slot)))
                    `(,(car (clos:slot-definition-initargs slot))
                      ;; only quote the next argument if needed.
                      ,(let* ((slot-reader (car (determine-slot-readers class (clos:slot-definition-name slot))))
                              (arg-value (if slot-reader
                                             (funcall slot-reader object)
                                           (clos::slot-value object (clos:slot-definition-name slot)))))
                         (process-argument arg-value)))))
            (clos:class-slots class))))

(defun generate-inherited-slot-writer (instance object)
  (let ((class (class-of object)))
    (mapcan #'(lambda (slot)
                (if (and (not (clos:slot-definition-initargs slot)) ; found it above in definer
                         (slot-boundp object (clos:slot-definition-name slot)))
                    ;; if we can't write it, ignore it. 
                    (or (let*-non-null ((writer (car (determine-slot-writers class (clos:slot-definition-name slot)))))
                          `((,(car writer) (,(cadr writer) ,instance)
                                           ,(process-argument (funcall (car (determine-slot-readers class (clos:slot-definition-name slot))) object)))))
                        (if *debug-clos-extensions*
                            (warn "Can't construct writer for slot ~W of ~W" slot object)))))
            (clos:class-slots class))))


;; thanks to Steve Haflich (smh@franz.com) for the following tidbit.
(defmethod determine-slot-readers ((class standard-class) (slot-name symbol))
#+excl
  (unless (clos:class-finalized-p class)
    (clos:finalize-inheritance class))
  (loop for c in (clos:class-precedence-list class)
      as dsd = (find slot-name (clos:class-direct-slots c)
		     :key #'clos:slot-definition-name)
      when dsd
      append (clos:slot-definition-readers dsd #+ccl c)))

(defmethod determine-slot-writers ((class standard-class) (slot-name symbol))
#+excl
  (unless (clos:class-finalized-p class)
    (clos:finalize-inheritance class))
  (loop for c in (clos:class-precedence-list class)
      as dsd = (find slot-name (clos:class-direct-slots c)
		     :key #'clos:slot-definition-name)
      when dsd
      append (clos:slot-definition-writers dsd #+ccl c)))

(defmethod determine-slot-initializers ((class standard-class) (slot-name symbol))
#+excl
  (unless (clos:class-finalized-p class)
    (clos:finalize-inheritance class))
  (loop for c in (clos:class-precedence-list class)
      as dsd = (find slot-name (clos:class-direct-slots c)
		     :key #'clos:slot-definition-name)
      when dsd
      append (clos:slot-definition-initargs dsd)))

(defun generate-legal-slot-initargs (class)
#+excl
  (unless (clos:class-finalized-p class)
    (clos:finalize-inheritance class))
  (mapcan #'(lambda (x) (copy-list (clos:slot-definition-initargs x))) (clos:class-slots class)))

(defun slot-for-initarg (class initarg)
  (let ((slots (clos:class-slots class)))
    (clos:slot-definition-name
     (find-if #'(lambda (slot)
                  (member initarg (clos:slot-definition-initargs slot)))
              slots))))

