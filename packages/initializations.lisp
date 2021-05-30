(in-package cl-lib-initializations)
(defparameter *cl-lib-initializations-version* 
  '(cl-lib:version-reporter "CL-LIB-Initializations" 5 16 ";; Time-stamp: <2019-02-18 13:14:31 Bradford Miller(on Aragorn.local)>" 
                            "5am testing"))

;; 5.16. 2/16/19 5am testing (see file initializations-tests.lisp)
;; 5.15  1/27/19 Add *debug-initializations*  
;; 5.11  5/16/11 Added support for 64-bit LispWorks
;; 5.1   5/18/07 default to silence when not interactive-lisp-p

;;; Copyright (C) 1991 by the Trustees of the University of Rochester.
;;; Portions Copyright (C) 2005, 2011, 2019 by Bradford W. Miller (bradfordmiller@mac.com)

;;; Right of use & redistribution is granted as per the terms of the 
;;; GNU LIBRARY GENERAL PUBLIC LICENCE version 1 which is incorporated here by reference.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; Gnu Library General Public License for more details.

;;; You should have received a copy of the Gnu Library General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Written 3/91 by Bradford W. Miller miller@cs.rochester.edu
;;; 4/8/94 ported to MCL 2.0.1 by miller
;;; 1/11/96 updated for Allegro 4.3beta by miller
;;; 11/29/01 added work-around for Lispworks braindamage with #|| ||# pairs spanning lines
;;;  6/ 3/02 fixed #+ stuff wrt LispWorks not liking two in a row.
;;;          added gc links for LispWorks, still investigating links for :before-cold, etc.
;;;  6/ 7/02 Added links to LispWorks Actions Lists (which is pretty much their
;;;          version of initializations).
;;   6/24/05 Updated for new CL-LIB organization (major release 5)

;;; Please send any bug reports or improvements to me at bradfordmiller@mac.com or
;;; bradford.w.miller@gmail.com

;;; I will be glad to respond to bug reports or feature requests.

;;;; Motivation:
;;; Since RHET and other projects have been using the initializations features of
;;; the explorer and symbolics, the following was inspired to allow a simple
;;; port to non-lispm lisps.

;;; Like the lisp machines, a number of lists are predefined, one to be run whenever
;;; the world is first booted (:WARM), one to force a single eval (:ONCE), one
;;; before any gc (:GC), after any gc (:AFTER-GC), before a non-checkpointed disk 
;;; save (:BEFORE-COLD), and after a non-checkpointed world is first booted (:COLD).
;;; Lispms had a initialization-keywords variable for
;;; associating new lists with keywords, mine is called *initialization-keywords*.

;;; one can also define when an initializtion is run, either :normal, which only
;;; places the form on the list (the default, unless :once is specified), :now
;;; which is to eval now and add it to the list, :first, which causes immediate
;;; evaluation if it hasn't been evaluated before (default for :once), and :redo
;;; which will not eval the form now, but will cause the initialization to
;;; be run the next time the list is processed, even if it has already been
;;; run.

;;; initializations are ordered as first added, first run.

;;; first set up the default lists

(defvar *warm-initialization-list* nil
  "Initializations run just after booting any world")

(defvar *cold-initialization-list* nil
  "Initializations run just after booting a non-checkpointed world")

(defvar *before-cold-initialization-list* nil
  "Initializations run just before saving a non-checkpointed world")

(defvar *once-initialization-list* nil
  "Initializations to be run only once")

(defvar *gc-initialization-list* nil
  "Initializations to be run before a gc")

(defvar *after-gc-initialization-list* nil
  "Initializations to be run after a gc")

(defvar *quit-initialization-list* nil 
  "Initializations to be run just before image quits")

(defvar *debug-initializations* nil
  "set non-nil to print some debug info")

#+allegro
(defvar *lep-initialization-list* nil
  "Initializations to be run after lep has been established")

(defparameter *initialization-keywords*
    (copy-list '((:warm *warm-initialization-list*)
                 (:cold *cold-initialization-list*)
                 (:before-cold *before-cold-initialization-list*)
                 (:once *once-initialization-list* :first)
                 #-sbcl ;; sbcl doesn't support before-gc hooks any more, so don't list it
                 (:gc *gc-initialization-list*)
                 #+allegro (:lep *lep-initialization-list*)
                 (:after-gc *after-gc-initialization-list*)
                 (:quit *quit-initialization-list*)))
  "Alist of initialization-list keywords and the list itself. Third element,
if present, is the default run time, :normal if absent. This can be overridden
by the add-initialization function.")

(defstruct initialization
  (name "" :type string)
  (form nil :type list)
  (flag nil)
  (run nil :type list))

;;; TODO: make more robust by checking the input.

(defun reset-initializations (initialization-list-name)
  "Sets the FLAG of each initialization on the passed list to NIL so it
will be (re)run the next time initializations is called on it."
  (declare (type symbol initialization-list-name))

  (dolist (init (symbol-value initialization-list-name))
    (setf (initialization-flag init) nil)))

(defun delete-initialization (name &optional keywords initialization-list-name)
  "Delete the initialization with name from either the list specified with the
keywords, or passed as initialization-list-name."
  (declare (type string name)
           (type list keywords)
           (symbol initialization-list-name))

  (unless initialization-list-name
    (setq initialization-list-name (cadr (assoc (car keywords)
                                                *initialization-keywords*))))

  (set initialization-list-name (delete name (symbol-value initialization-list-name)
                                        :test #'equal
                                        :key #'(lambda (x) (initialization-name x)))))

(defun initializations (initialization-list-name &optional redo (flag t))
  "Run the initializations on the passed list. If redo is non-nil, then the
current value of the flag on the initialization is ignored, and the 
initialization is always run. Otherwise an initialization is only run if
the associated flag is NIL. Flag is the value stored into the initialization's
flag when it is run."
  (declare (type symbol initialization-list-name))
  
  (dolist (init (symbol-value initialization-list-name))
    (when (or redo (not (initialization-flag init)))
      (eval (initialization-form init))
      (setf (initialization-flag init) flag))))

(defun add-initialization (name form &optional keywords initialization-list-name)
  "The initialization form is given name name and added to the initialization list
specified either by the keywords or passed directly. The keywords can also be used
to change the default time to evaluate the initialization."
  (declare (type string name)
           (type list form keywords)
           (type symbol initialization-list-name))

  (let ((type '(:normal))
        (list-name initialization-list-name)
        list-key)
    (cond
     (list-name
      (if keywords
          (setq type keywords)))
     (t
      (setq list-name (cdr (some #'(lambda (x)
                                     (setq list-key x) ; remember it
                                     (assoc x *initialization-keywords*))
                                 keywords)))
      (if (cdr list-name)
          (setq type (cdr list-name)))
      (setq list-name (car list-name))
      ;; now check if keywords override.
      (setq keywords (remove list-key keywords :test #'eq))
      (if keywords
          (setq type keywords))))

    (assert list-name (keywords initialization-list-name) "No initialization list name given")
    ;; OK, now we can process the entry.... first, is it already there?
    (let ((entry (if (boundp list-name)
                     (find name (symbol-value list-name) 
                           :test #'equal
                           :key #'(lambda (x) (initialization-name x))))))
      (cond
       ((null entry)
        (set list-name
             (nconc (if (boundp list-name)
                        (symbol-value list-name))
                    (list (setq entry (make-initialization
                                       :name name
                                       :form form
                                       :run type))))))
       (t
        ;; update the entry
        (if *debug-initializations* (format *error-output* "~&Debug-Initializations: Updating ~S with ~S" entry form))
        (setf (initialization-form entry) form)
        (setf (initialization-run entry) type)))
      (cond
       ((or (member :now type)
            (and (member :first type)
                 (not (initialization-flag entry))))
        (eval form)
        (setf (initialization-flag entry) t)))
      (cond
       ((member :redo type)
        (setf (initialization-flag entry) nil))))))

;;; the rest of this stuff is just to make sure the various predefined lists are
;;; automatically processed at the appropriate time.

(defun reset-and-invoke-initializations (list-keyword)
  (declare (type keyword list-keyword))
  
  (let ((entry (assoc list-keyword *initialization-keywords*)))
    (reset-initializations (cadr entry))
    (initializations (cadr entry))))

;;; :once requires no processing. It's done only at add-initialization time.
;;;
;;; :gc / :after-gc surround invocations to gc

;; sbcl doesn't have an advice facility. Rather than invent our own (which won't work in the case of any inline codes),
;; we will leverage the defined hooks, listed here:
;;
;; for :warm & :cold
;; sb-ext:*init-hooks* - a list of function designators called in an
;; unspecified order after a saved core image starts up, after the
;; system itself has been initialized
;;
;; NB: since SBCL doesn't distinguish between warm and cold booting,
;; or allow us to do something different before initialization
;; finishes (we can only get control after it has initialized), we
;; just run :cold before we run :warm
;;
;; for :quit ...
;; sb-ext:*exit-hooks* - a list of function designators called in an
;; unspecified order when sbcl exits (however, it is omitted if
;; (sbcl-ext:exit :abort t) is used).
;;
;; for :before-cold...
;; sb-ext:*save-hooks* - ditto, called before creating a saved core image
;;
;; for :after-gc...
;; sb-ext:*after-gc-hooks* - called after gc, except if gc called
;; during thread exit. In a multithreaded image, may run in any
;; thread.
;;
;; NB: SBCL does not have any before-gc hooks (though CMUCL on which
;; SBCL is based did, so not sure what's up with that).  At any rate,
;; until and unless we want to dive into the SBCL source to create a
;; robust advice facility (a poor man's version would just redefine
;; the fdefinitions of symbols and supress any warnings by generating
;; a wrapper around the original function, but that has issues with
;; inline directives, at least - I suspect the proper way to do this
;; would be to make sure even inline functions call a hook to look for
;; the existance of advice unless we've used a very high value for
;; optimize speed, which implies modifying the function compiler or at
;; least defun), we just won't support the :gc keyword on sbcl. Note
;; that sbcl does have extensions like weak pointers and finalization
;; meaning the most likely uses of such gc initializations would need
;; to be recoded to use those instead. (Browsing through some old sbcl
;; bug reports make me think they dropped before-gc because too many
;; users didn't grock that they shouldn't cons when gc had been
;; triggered! Talk about throwing out the baby with the bathwater...)

(defun execute-gc-inits ()
  (reset-and-invoke-initializations :gc))

#+excl
(excl:advise excl:gc :before handle-gc-initializations nil
             (execute-gc-inits))

#+ccl
(ccl:advise ccl:gc (execute-gc-inits) :when :before :name handle-gc-initializations)

#+(and LispWorks (not LispWorks-64bit))
(lispworks:defadvice (hcl:mark-and-sweep before-gc :before) (generation)
                     (progn generation (execute-gc-inits)))

#+LispWorks-64bit
(lispworks:defadvice (system:marking-gc before-gc :before) (generation &key &allow-other-keys)
  (progn generation (execute-gc-inits)))

(defun execute-after-gc-inits ()
  (reset-and-invoke-initializations :after-gc))

#+sbcl
(eval-when (:load-toplevel :execute)
  (pushnew 'execute-after-gc-inits sb-ext:*after-gc-hooks*))
        
#+excl
(excl:advise excl:gc :after handle-after-gc-initializations nil
             (execute-after-gc-inits))

#+ccl
(ccl:advise ccl:gc (execute-after-gc-inits) :when :after :name handle-after-gc-initializations)

#+(and LispWorks (not LispWorks-64bit))
(lispworks:defadvice (hcl:mark-and-sweep after-gc :after) (generation)
                     (progn generation
                            (execute-after-gc-inits)))

#+LispWorks-64bit
(lispworks:defadvice (system:marking-gc after-gc :after) (generation &key &allow-other-keys)
  (progn generation (execute-after-gc-inits)))

#||
;; LispWorks has also implemented this callback in 64bit versions - but no before callback, so to be symmetric
;; we use the advice on marking-gc
(system:set-automatic-gc-callback nil #'(lambda (generation-number)
                                          (declare (ignore generation-number))
                                          (reset-and-invoke-initializations :after-gc)))
||#

;;; :before-cold happens when we disk save, unless we specify :checkpoint
;;; while :cold happens after the world comes back (unless we specify :checkpoint)
;;; and :warm happens in any case.

#+(and excl (not (version>= 4 3)))
(eval-when (:load-toplevel :execute)
  (pushnew '(:eval initializations '*warm-initialization-list*) excl:*restart-actions* :test #'equalp))

(defun run-warm-inits () (initializations '*warm-initialization-list*))

#+(and excl (version>= 4 3))
(eval-when (:load-toplevel :execute)
  (progn
    (pushnew 'run-warm-inits excl:*restart-actions* :test #'equalp)))

#+ccl2
(eval-when (:load-toplevel :execute)
  (pushnew '(lambda () (initializations '*warm-initialization-list*)) ccl:*lisp-startup-functions* :test #'equalp))

#+lispworks
(eval-when (:load-toplevel :execute)
  (lw:define-action "When starting image" 
    "Run Warm Initializations"
    'run-warm-inits
    :after "System startup completed"))

#+(and excl (not (version>= 4 3)))
(eval-when (:load-toplevel :execute)
  (pushnew '(:eval initializations '*cold-initialization-list*) excl:*restart-actions* :test #'equalp))

(defun run-cold-inits () (initializations '*cold-initialization-list*))

#+(and excl (version>= 4 3))
(eval-when (:load-toplevel :execute)
  (progn
    (pushnew 'run-cold-inits excl:*restart-actions* :test #'equalp)))

#+ccl2
(eval-when (:load-toplevel :execute)
  (pushnew '(lambda () (initializations '*cold-initialization-list*)) ccl:*lisp-startup-functions* :test #'equalp))

#+Lispworks
(eval-when (:load-toplevel :execute)
  (lw:define-action "When starting image" 
    "Run Cold Initializations"
    'run-cold-inits
    :before "System startup completed"))

#+excl
(excl:advise excl:dumplisp :before handle-initializations nil
             (reset-initializations '*warm-initialization-list*)
             (unless (extract-keyword :checkpoint excl:arglist)
               (reset-and-invoke-initializations :before-cold)))

(defun run-cold-then-warm-inits ()
  (run-cold-inits)
  (run-warm-inits))

#+sbcl
(eval-when (:load-toplevel :execute)
  (pushnew sb-ext:*init-hooks* 'run-cold-then-warm-inits))

(defun do-before-cold ()
  (reset-initializations '*warm-initialization-list*)
  (reset-and-invoke-initializations :before-cold))

#+ccl2
(eval-when (:load-toplevel :execute)
  (pushnew '(do-before-cold) ccl:*save-exit-functions*))

#+lispworks
(eval-when (:load-toplevel :execute)
  (lw:define-action "Delivery Actions" 
    "Before Cold Initializations"
    'do-before-cold))

#+sbcl
(eval-when (:load-toplevel :execute)
  (pushnew 'do-before-cold sb-ext:*save-hooks*))

(defun do-quit ()
  (reset-and-invoke-initializations :quit))

#+excl
(format t "Don't know how to bind quit list for excl")

#+ccl2
(format t "Don't know how to bind quit list for ccl2")

#+lispworks
(eval-when (:load-toplevel :execute)
  (lw:define-action "When quitting image"
      "Quit Initializations"
    'do-quit))

#+sbcl
(eval-when (:load-toplevel :execute)
  (pushnew 'do-quit sb-ext:*exit-hooks*))

#+excl
(excl:advise excl:start-emacs-lisp-interface :after handle-initilizations nil
             (mp:process-run-function "delayed LEP init" 
                                      #'(lambda () (sleep 8) (while-not (lep:lep-is-running) (sleep 8))
                                                (reset-and-invoke-initializations :lep))))

(eval-when (:load-toplevel :execute)
  (add-initialization "User Notification"
                      '(if (cl-lib-essentials:interactive-lisp-p)
                        (format *error-output* "~2&--> Running Before-Cold Initializations~2%"))
                      '(:before-cold))
  (add-initialization "User Notification"
                      '(if (cl-lib-essentials:interactive-lisp-p)
                        (format *error-output* "~2&--> Running Cold Initializations~2%"))
                      '(:cold))
  (add-initialization "User Notification"
                      '(if (cl-lib-essentials:interactive-lisp-p)
                        (format *error-output* "~2&--> Running Warm Initializations~2%"))
                      '(:warm))
  #+excl
  (add-initialization "User Notification"
                      '(if (cl-lib-essentials:interactive-lisp-p)
                        (format *error-output* "~2&--> Running after LEP Initializations~2%"))
                      '(:lep))
  (add-initialization "User Notification"
                      '(if (cl-lib-essentials:interactive-lisp-p)
                        (format *error-output* "~2&--> Running Quit Initializations~2%"))
                      '(:quit))
  (add-initialization "Reset Cold Initializations"
                      '(reset-initializations '*cold-initialization-list*)
                      '(:before-cold)))

;;; 
;;; Here is a short example of the above (See also initializations-tests.lisp in this directory)
;;;

#+hell-freezes-over
(eval-when (:execute)
  (add-initialization "test1" '(print "should print once") '(:once))

  (add-initialization "test1" '(print "should print once") '(:once))

  ;; note the first immediately executed the print, and the second supressed it.

  (defvar *new-init-list* nil)

  (add-initialization "a hack" '(print "One hack") () '*new-init-list*)

  ;; didn't print yet

  (initializations '*new-init-list*)

  ;; now it did

  (initializations '*new-init-list*)

  ;; but not again until

  (reset-initializations '*new-init-list*)

  (initializations '*new-init-list*))

;;; Note in particular the :once list may be useful in init files, particularly
;;; where the result might be saved in a world and you don't want it redone
;;; e.g. in allegro checkpoint worlds.
;;; 
;;; The other sorts of lists are particularly useful for housekeeping

(pushnew :cl-lib-initializations *features*)

