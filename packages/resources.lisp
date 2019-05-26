(in-package CL-LIB)(cl-lib:version-reporter "CL-LIB-Resources" 5 16 ";; Time-stamp: <2019-03-01 17:25:12 Bradford Miller(on Aragorn.local)>"                          "fix toplevel");; 5.16  3/ 1/19  add test code;; 5.16  2/23/19  fixup toplevel;;; ****************************************************************;;; Resources ******************************************************;;; ****************************************************************;;; This is the resources package written February 1992 by ;;;   Bradford W. Miller;;;   miller@cs.rochester.edu;;;   University of Rochester, Department of Computer Science;;;   610 CS Building, Comp Sci Dept., U. Rochester, Rochester NY 14627-0226;;;   716-275-1118;;; (Please note that I am no longer there or at that address);;; I will be glad to respond to bug reports or feature requests.;;; Updated 6/4/93 by miller to improve efficiency for the typical case of not having a matcher;;; or initialization function (since you do the work on deinitialization to allow gc).;;; Added HOW TO USE section 1/3/94. (miller);;;;;; This version was NOT obtained from the directory;;; /afs/cs.cmu.edu/user/mkant/Public/Lisp-Utilities/initializations.lisp;;; via anonymous ftp from a.gp.cs.cmu.edu. (you got it in cl-lib).;;;;;;;;; Copyright (C) 2019, 1994, 1993, 1992 by Bradford W. Miller, miller@cs.rochester.edu;;; All rights reserved.;; This library is free software; you can redistribute it and/or modify it under the terms of the GNU ;; Lesser General Public License as published by the Free Software Foundation; either version 3.0 of ;; the License, or (at your option) any later version.;; This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; ;; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. ;; See the GNU Lesser General Public License for more details.;; You should have received a copy of the GNU Lesser General Public License along with this library; ;; if not, see <http://www.gnu.org/licenses/>.;; miller - Brad Miller (bradfordmiller@mac.com);;;;;; ********************************;;; HOW TO USE THIS PACKAGE ********;;; ********************************;;; To create a new resource:;;; (defresource (name (args) :initial-copies 0 :initializer nil :deinitializer nil;;;                           :matcher nil &allow-other-keys);;;		       &body constructor) <macro>;;; Name, an unevaluated symbol, will become the name of the new resource.;;; Args, a lambda list, are used to initialize (create) instances of the ;;;   resource, and come from allocate-resource (so it can be used to supply, ;;;   e.g. default arguments);;;;;;   Note that these args will be used in the default matcher, and;;;   only resources created with the same args will be allowed to;;;   match. If you are creating resources that are homogeneous (that;;;   will be initialized or deinitialized to a common state, or where;;;   you don't care about the state), you should supply nil for args.;;;;;; Constructor is a function to call to make a new object when the resource;;;   is empty, and uses the parameters Args as arguments (think of it as a;;;   defun). Note this is required.;;;   Options are:;;; :initial-copies (used to set up the pool to begin with). This is;;;     the number of times the constructor will be called initially;;;     to form a pool. This will only work if args is nil, or the;;;     constructor can be called with no args (since it has no idea;;;     what arguments to supply).;;;	:initializer (called on a newly allocated object, and the other ;;;     parameters). Note the constructor isn't called on objects that;;;     are already in the pool. The initializer is called before handing ;;;     an object in the pool to the user.;;;     :deinitializer (called on a newly freed object) Useful to;;;     allow gc of objects the resource refers to, or to;;;     preinitialize, if args aren't needed (if you e.g. do your;;;     deallocation at a user-insensitive time, but allocate at a;;;     user-sensitive time).;;; :matcher Args are like initializer, but is expected to be a;;;     predicate that succeeds if the unallocated pool object is;;;     appropriate for the call. The default one assumes only pool;;;     objects created with the same parameters are appropriate.;;;     This is useful if, e.g. you are going to put different size;;;     objects in the pool, but don't want to have to create a new;;;     object when a (bigger) one already exists.;;; allocate-resource (name &rest args) <function>;;;  Get a copy of the NAMEd resource, given the args (for the initializer, ;;;   matcher and constructor). ;;; allocate-<resource-name> (&rest args) <function>;;;  specialized version of allocate-resource for resource-name (created by defresource).;;; deallocate-resource (name object) <function>;;;  Return object to pool name. It's a bad idea to free an object to the wrong;;;   pool. ;;; deallocate-<resource-name> (object) <function>;;;   Specialized version of deallocate-resource for resource-name (created by defresource).;;; clear-resource (name) <function>;;;   Zaps Name's pool, and starts from empty. Normally only used within ;;;   DefResource when recompiled, or user should call if you change the;;;   constructor s.t. the old objects in the pool are obsolete. ;;;   Not a good idea to call this if active objects are in use.;;; clear-<resource-name> () <function>;;; map-resource (function name) <macro>;;;   Incompatibly defined wrt Lispm, this one is more like mapcar - the;;;   function is called over all resources of type Name, which;;;   is not evaluated. The args are the object, and t if the object is in use,;;;   nil otherwise. I.e.;;;   (map-resource #'(lambda (object in-use) (if in-use (format t "using ~S~%" object))) 'foobar);;;   will print out all of the foobar resources in use.;;; map-<resource-name> (function) <macro>;;; with-resource ((resource name &rest args) &body body) <macro>;;;  Resource is bound to a Named resource initialized with Args.;;;   Name is **not** eval'd but args are.;;;   The resource is freed when the form is exited.;;; with-<resource-name> ((resource &rest args) &body body) <macro>;;; ********************************;;; Motivation *********************;;; ********************************;;;;;; Resources are useful for avoid excessive consing and GCing by;;; explicitly maintaining pools of objects for reuse, instead of;;; creating them when needed and relying on the garbage collector to;;; flush them when they are no longer needed. When an object is no;;; longer needed, it is returned to the pool of objects of that type,;;; and recycled when a new object of that type is needed. Using;;; resources might wind up recycling objects faster than incremental;;; GC, but it isn't clear whether there are any real time/space;;; savings.;;;;;; Since I have been using the resource features of the explorer and;;; symbolics, the following was inspired to allow a simple port to;;; non-lispm lisps. It should be compatible with most uses of the;;; lispm's versions, though it does not support (yet) the wide;;; variety of options.;;;;;; note: efficiency can likely be improved; the basic idea is to take;;; a name of a resource, and put some properties on it - the pool of;;; all allocated resources and a freelist. So building a resource;;; instance uses (at least) three cons cells - one on the pool, one;;; on the freelist, and one on a description list for the default;;; matcher. The default matcher isn't particularly efficient either;;; since it uses position and nth; but most internal uses of;;; resources have no args (all resources identical) so matcher isn't;;; used anyway. Better would be to build a structure that keeps the;;; description and other info in it, then we have to be able to find;;; the structure on a free (symbolics passes an extra value to the;;; user for just this purpose I beleive - I've not looked at their;;; source code for this, of course :-);;;;;; resources won't be that useful for tiny thingos, since the;;; alloc/free cycle kicks out a cons cell per. For bigger things, the;;; efficiency depends on use. Long lived objects are probably better;;; off just using the usual gc. Particularly since on non-lispms we;;; can't prevent scavenge attempts of resources anyway.;;;;;; another (internal) use of resources is to write code that may;;; eventually depend on malloc - so the user has to get used to;;; explicitly freeing things anyway. (Shocker back end is an example;;; of this). Of course lisps that allow you to cons things in;;; separate areas that aren't gc'd can improve efficiency by making;;; sure resources are created there.  and if CL had some sort of FREE;;; function to explicitly free a list, (I mean it has to be cheaper,;;; in some cases, for an application to know what's garbage than to;;; have a gc discover it, no?) then resources could also be more;;; (generically) efficient.;;;;;; Note: on a symbolics we use the built-in stuff (translating args;;; as appropriate) since I assume that's gonna be more efficient than;;; this code.;;; ********************************;;; New  ***************************;;; ********************************;;; 12/29/93 Brad Miller;;; Defresource now creates specialized allocate, deallocate, etc. fns called;;; fn-<resource-name> rathern than <fn>-resource;;; i.e. (allocate-resource 'frozboz 1 2 3) is the generic version of;;; (allocate-frozboz 1 2 3);;; in particular for terms with no args, or other simplifications, the specialized;;; versions can be much faster.;; some simple work arounds for a different gc (just to nullify, feel;; free to substitute something more appropriate, and SEND IT OVER!#-excl (defpackage excl (:export #:*tenured-bytes-limit* tenuring))#-excl(defvar excl:*tenured-bytes-limit* 0)#-excl (defmacro tenuring (&body body) `(progn ,@body))(defstruct resource  (cons #'identity :type function)  (pool nil :type list)  (desc nil :type t)  (freel nil :type list)  (default #'identity :type function)  (free #'identity :type function)  (init #'identity :type function)  (matcher #'identity :type function)  )(defun extract-parameters (args)   (let (result)    (dolist (arg args (nreverse result))      (if (consp arg)	  (push (car arg) result)	  ;; (unless (char= #\& (char (string arg) 0))	  ;;   (push arg result))        (unless (find arg lambda-list-keywords)          (push arg result))))))(defmacro defresource ((name args &rest hack			     &key (initial-copies 0) 			     initializer deinitializer			     matcher &allow-other-keys)		       &body constructor)  "Name, an unevaluated symbol, will become the name of the new resource    (like the lispm, it's a property of Name).   Args, a lambda list, are used to initialize (create) instances of the    resource, and come from allocate-resource (so it can be used to supply,    e.g. default arguments)   constructor is a function to call to make a new object when the resource   is empty, and uses the parameters Args as arguments (think of it as a   defun). Note this is required.   Options are:	:initial-copies (used to set up the pool to begin with).	:initializer (called on a newly allocated object, and the other         parameters). Note the constructor isn't called on objects that        are already in the pool.        :deinitializer (called on a newly freed object) Useful to allow gc        of objects the resource refers to.	:matcher Args are like initializer, but is expected to be a predicate        that succeeds if the unallocated pool object is appropriate for the         call. The default one assumes only pool objects created with the same        parameters are appropriate.        This is useful if you are going to put different size objects in the        pool, but don't want to have to create a new object when a (bigger)        one already exists.        The lispm supports other options too."  #+symbolics  `(scl:defresource ,name ,args :constructor ,@constructor ,@hack)  #-symbolics  (let ((parameters (extract-parameters args))        (i (gensym))        (oldlimit (gensym))        (resource (gensym))        (object (intern 'object)) ; want symbol in current package.        ;; build specialized versions of allocate, deallocate, clear, map and with-resouce        (allocate-fn-name (intern (format nil "ALLOCATE-~A" name)))        (deallocate-fn-name (intern (format nil "DEALLOCATE-~A" name)))        (clear-fn-name (intern (format nil "CLEAR-~A" name)))        (map-fn-name (intern (format nil "MAP-~A" name)))        (with-fn-name (intern (format nil "WITH-~A" name))))    `(let ((,resource (make-resource                       :cons #'(lambda ,args ,@constructor)                       :pool nil                       :desc nil                       :freel nil                       :default #'(lambda ,args (list ,@parameters))                       :free #'(lambda (,object) ,object ,deinitializer)                       :init #'(lambda (,object ,@args) ,object ,initializer)                       :matcher #'(lambda (,object ,@args)                                    ,object                                    ,(or matcher                                         (null args)                                         `(let ((res (get ',name :resource)))                                            (declare (type resource res))                                            (every #'eql (list ,@parameters)                                                   (nth (position ,object (resource-pool res))                                                        (resource-desc res)))))))))       (declare (type resource ,resource))       (setf (get ',name :resource) ,resource)       ,@(when (plusp initial-copies)           `((eval-when (load eval)               (excl:tenuring                (let ((,oldlimit excl:*tenured-bytes-limit*)                      (,i ,initial-copies))                  (setf excl:*tenured-bytes-limit* nil)                  (while (plusp ,i)                    (decf ,i)   ; I'd use dotimes, but then i would be unref'd                    (push (funcall (resource-cons ,resource))                          (resource-pool ,resource))                    ,@(if (null args) nil                        `((push (funcall (resource-default ,resource))                                (resource-desc ,resource)))))                  (setf excl:*tenured-bytes-limit* ,oldlimit)))               (clear-resource ',name))))       (defun ,allocate-fn-name ,args         (do ((lasttry nil try)              (try (resource-freel ,resource) (cdr try)))             ((null try) nil)           (declare (ignorable lasttry))           ,@(cond             ((and (null matcher) (null args) (null initializer))              `((return-from ,allocate-fn-name (pop (resource-freel ,resource)))))             ((and (null matcher) (null args))              `((funcall (resource-init ,resource) (car try))                (return-from ,allocate-fn-name (pop (resource-freel ,resource)))))             ((and (null args) (null initializer))              `((when (funcall (resource-matcher ,resource) (car try))                 (if lasttry                     (rplacd lasttry (cdr try))                   (pop (resource-freel ,resource)))                 (return-from ,allocate-fn-name (car try)))))             ((null initializer)              `((when (apply (resource-matcher ,resource) (car try) ,args)                  (if lasttry                      (rplacd lasttry (cdr try))                    (pop (resource-freel ,resource)))                  (return-from ,allocate-fn-name (car try)))))             ((null args)              `((when (funcall (resource-matcher ,resource) (car try))                  (funcall (resource-init ,resource) (car try))                  (if lasttry                      (rplacd lasttry (cdr try))                    (pop (resource-freel ,resource)))                  (return-from ,allocate-fn-name (car try)))))             (t              `((when (apply (resource-matcher ,resource) (car try) ,args)                  (apply (resource-init ,resource) (car try) ,args)                  (if lasttry                      (rplacd lasttry (cdr try))                    (pop (resource-freel ,resource)))                  (return-from ,allocate-fn-name (car try)))))))                          ;; none found, init one         (let ((new ,(if args `(apply (resource-cons ,resource) ,args)                       `(progn ,@constructor))))                                  ,@(cond              ((and initializer args)               `((apply (resource-init ,resource) new ,args)))              (initializer               `((funcall (resource-init ,resource) new))))           (push new (resource-pool ,resource))           ,@(if args                 `((push (list ,args) (resource-desc ,resource))))           new))              (defun ,deallocate-fn-name (object)         ,@(if deinitializer               `((funcall (resource-free ,resource) object)))         (push object (resource-freel ,resource)))              (defun ,clear-fn-name ()         (setf (resource-freel ,resource) nil)         (dolist (res (resource-pool ,resource))           ,@(if deinitializer                 `((funcall (resource-free ,resource) res)))           (push res (resource-freel ,resource))))       (defmacro ,map-fn-name (function)         `(mapcar #'(lambda (ob)                      (funcall ,function ob (not (member ob (resource-freel (get ',',name :resource))))))                  (resource-pool (get ',',name :resource))))              (defmacro ,with-fn-name ((res ,@args) &body body)         `(let ((,res (,',allocate-fn-name ,,@args)))            (unwind-protect                (progn ,@body)              (,',deallocate-fn-name ,res)))))))           #+allegro-v4.1(add-initialization "lep-init for defresource"                    '(lep::eval-in-emacs "(put 'defresource 'fi:lisp-indent-hook '(like with-open-file))")                    '(:lep))(defun allocate-resource (name &rest args)  "Get a copy of the NAMEd resource, given the args (for the initializer,    matcher and constructor). Name is evaluated."  #+symbolics  (apply #'scl:allocate-resource name args)   #-symbolics  (let ((resource (get name :resource)))    (declare (type resource resource))    (do ((lasttry nil try)	 (try (resource-freel resource) (cdr try)))	((null try) nil)      (when (if args		(apply (resource-matcher resource) (car try) args)		(funcall (resource-matcher resource) (car try)))	(if args 	    (apply (resource-init resource) (car try) args)	    (funcall (resource-init resource) (car try)))	(if lasttry	    (rplacd lasttry (cdr try))	    (pop (resource-freel resource)))	(return-from allocate-resource (car try))))    ;; none found; init one    (let ((new (if args (apply (resource-cons resource) args)		   (funcall (resource-cons resource)))))      (if args          (apply (resource-init resource) new args)          (funcall (resource-init resource) new))      (push new (resource-pool resource))      (if args          (push (copy-list args) (resource-desc resource)))      new)))(defun deallocate-resource (name object)  "Return object to pool name. It's a bad idea to free an object to the wrong   pool. Name is evaluated."  #+symbolics  (scl:deallocate-resource name object)  #-symbolics  (let ((resource (get name :resource)))    (declare (type resource resource))    (funcall (resource-free resource) object)    (push object (resource-freel resource))))(defun clear-resource (name)  "Zaps Name's pool, and starts from empty. Normally only used within    DefResource when recompiled, or user should call if you change the   constructor s.t. the old objects in the pool are obsolete.    Not a good idea to call this if active objects are in use."  #+symbolics  (scl:clear-resource name)  #-symbolics  (let ((resource (get name :resource)))    (declare (type resource resource))    (setf (resource-freel resource) nil)    (dolist (res (resource-pool resource))      (funcall (resource-free resource) res)      (push res (resource-freel resource)))))(defmacro map-resource (function name)  "Incompatibly defined wrt Lispm, this one is more like mapcar - the   function is called over all resources of type Name, which   is not evaluated. The args are the object, and t if the object is in use,    nil otherwise."  #+symbolics  `(scl:map-resource ,name ,function)  #-symbolics  (let ((resource (gensym)))    `(let ((,resource (get ',name :resource)))       (declare (type resource ,resource))       (mapcar #'(lambda (ob)                   (funcall ,function ob (not (member ob (resource-freel ,resource)))))               (resource-pool ,resource)))))#+allegro-v4.1(add-initialization "lep-init for map"                    '(lep::eval-in-emacs "(put 'map-resource 'fi:lisp-indent-hook 1)")                    '(:lep))(defmacro with-resource ((resource name &rest args) &body body)  "Resource is bound to a Named resource initialized with Args.   Name is **not** eval'd but args are.   The resource is freed when the form is exited."  #+symbolics  `(scl:using-resource (,resource ,name ,@args) ,@body)  #-symbolics  `(let ((,resource (allocate-resource ',name ,@args)))     (unwind-protect	 (progn ,@body)       (deallocate-resource ',name ,resource))))#+allegro-v4.1(add-initialization "lep-init for with"                    '(lep::eval-in-emacs "(put 'with-resource 'fi:lisp-indent-hook '(like with-open-file))")                    '(:lep))(pushnew :cl-lib-resources *features*);;; *EOF*