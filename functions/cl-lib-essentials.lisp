;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: CL-LIB; Base: 10 -*-
(in-package cl-lib-essentials)

(defparameter *cl-lib-essentials-version* '(version-reporter "CL-LIB-Essentials" 5 16
                                            ";; Time-stamp: <2019-03-16 13:01:13 Bradford Miller(on Aragorn.local)>"
                                            ";; sbcl interactive-lisp-p"))

;; 5.16  2/22/19 sbcl port for interactive-lisp-p
;; 5.14 12/16/11 detailed-version-reporter
;; 10/22/07 5.6 longer version string

;;; 
;;; Copyright (C) 2005, 2007, 2011, 2019 by Bradford W. Miller, bradfordmiller@mac.com
;;;
;; This library is free software; you can redistribute it and/or modify it under the terms of the GNU 
;; Lesser General Public License as published by the Free Software Foundation; either version 3.0 of 
;; the License, or (at your option) any later version.

;; This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
;; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;; See the GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License along with this library; 
;; if not, see <http://www.gnu.org/licenses/>.

;; miller - Brad Miller (bradfordmiller@mac.com)

;;;

;; comments from the lep file fi-indent:

;;; Lisp form indentation specifications.
;;; Note that `t' specifies that the form is not special and `shadows'
;;;   any indentation specified with the property stored under the
;;;   indicator `fi:lisp-indent-hook'.

;;A note on indenting methods: (I think this is allegro specific)
;;
;; Triples are: depth, count and method.  In the following example, the
;; indentation method is a list of two lists, (1 (2 t)((1 0 quote)(0 t nil)))
;; and (0 t 1).  The first triple is for sexps at level 1 (ie, the arguments
;; to CASE).  The second triple is for the CASE sexp itself (ie, the
;; indentation applies to the elements of the CASE expression, not the
;; indentation of the individual elements within the CASE).  Note also that
;; triples for deeper sexps come first (ie, ordered depending on descending
;; depth).  For depth 1, the count is (2 t), which means apply the method
;; to all but the elements 0 and 1 of the CASE (0 is CASE and 1 is the
;; first argument to the CASE).  The method, ((1 0 quote) (0 t nil))
;; recursively defines what happens inside each of these elements in the
;; CASE (ie, refered to by a count of (2 t)): at depth 1 and count 0, the
;; elements are indented as quoted lists (aligned under the CAR); at depth
;; 0 for any element the standard indentation applies.
;;
;;(put 'case 'fi:lisp-indent-hook
;;     '((1 (2 t) ((1 0 quote)
;;		   (0 t nil)))
;;       (0 t 1)))


;; this came from the way to do things on allegro.
;; Lispworks uses (editor:setup-indent form-name no-of-args-to-treat-special &optional standard-indent-spaces special-indent-spaces)
(defmacro macro-indent-rule (symbol what)
  "Inform the editor (emacs) of how to indent macros, used by CL-LIB"
  #-lep (declare (ignore symbol what))
  #+lep                         ;must  be 4.1 or later, have lep
  `(add-initialization ,(format nil "lep init for ~A" symbol)
                       '(lep::eval-in-emacs
                         ,(concatenate 'string
                                       "(put '"
                                       (string-downcase (string symbol))
                                       " 'fi:lisp-indent-hook "
                                       (string-downcase (format nil "'~S)" what))))
                       '(:lep))
  )

(defun interactive-lisp-p ()
  "Return non-nil if the lisp is interactive (i.e., contains the development system)"
  #+excl excl:*print-startup-message* 
  #+lispworks (not (boundp 'lw:*delivery-level*))
  #+sbcl (not (eq sb-debug::*invoke-debugger-hook* 'sb-debug::debugger-disabled-hook)) ; true in sbcl-1.4.13
  #-(or lispworks excl sbcl) t)

;; version reporting: allows various cl-lib modules (or your own programs!) to print out an announcement so you can keep track of 
;; what's been loaded. Great for flushing into a log. This is a generalization of code I've implemented at least half a dozen 
;; times for each new project I start. Not so useful once you deploy (you'll want real configuration management), this is more 
;; a mechanism to make sure when you have multiple developers all running variations of code, you know which version of which 
;; file each one used at the time a bug was reported. It also helps you make sure you are running the latest version. The basic
;; idea is to put a timestamp into each file that gets updated automatically by your editor (e.g., FSF emacs has time-stamp do 
;; this, as at the top of this file). This can be captured into a lisp variable, and then printed into an appropriate log at the
;; right time. To modulate this, we create an initialization list (since the initialization package is already loaded), and a
;; macro for stuffing new initializations onto this list with the version info. Then there are functions to trigger the 
;; initializations, which cause everything to get printed to a particular stream, *error-output*
;;
;; Note that version-reporter can supply a different symbol for the report string, and for the initialization list, these can
;; be your own for the system you are building. You can rebind your report-string symbol before calling report-version, in order
;; to change how the report is printed, and you can also rebind *error-output* to change the destination.

(defvar *cl-lib-version-reporter-initializations* nil)

(defparameter *cl-lib-version-reporter-string* "~&;; ~A~45t~A~72t~D.~D~%;;~2t(~A)~%" "Default control string to announce the version")
(defparameter *cl-lib-detailed-version-reporter-string* "~&;; ~A~45t~A~72t~D.~D.~D~%;;~2t(~A)~%" "Default detail control string to announce the version")
(defparameter *cl-lib-version-announce-p* t "change this to NIL to prevent version announcement at startup")

(defvar *report-depth*)

(defmacro version-reporter (part-name major-version minor-version date comments
                                      &optional (report-string-symbol '*cl-lib-version-reporter-string*)
                                      (initialization-list-symbol '*cl-lib-version-reporter-initializations*))
  "set up a version announcement"
  `(eval-when (load eval)
     (cl-lib-initializations:add-initialization ,(format nil "~A report" part-name)
                                                '(do-report-version ',report-string-symbol
                                                  ,date ,(string part-name) ,major-version ,minor-version ,comments)
                                                () ',initialization-list-symbol)))

(defmacro detailed-version-reporter (part-name major-version minor-version micro-version date comments
                                      &key (report-string-symbol '*cl-lib-detailed-version-reporter-string*)
                                      (initialization-list-symbol '*cl-lib-version-reporter-initializations*))
  "set up a version announcement"
  `(eval-when (load eval)
     (cl-lib-initializations:add-initialization ,(format nil "~A report" part-name)
                                                '(do-report-detailed-version ',report-string-symbol
                                                  ,date ,(string part-name) ,major-version ,minor-version ,micro-version ,comments)
                                                () ',initialization-list-symbol)))

(defun report-version (&optional depth (initialization-list '*cl-lib-version-reporter-initializations*))
  "If depth is non-nil, then part names of the form foo-bar-bletch are suppressed at depth > 2. (using prefixes)."
  (let ((*report-depth* depth))
    (initializations initialization-list t)))

(defun report-at-depth-p (part-name)
  (not (and *report-depth*
            (> (count #\- part-name) *report-depth*))))

(defun do-report-version (report-string-symbol date part-name major-version minor-version comments)
  (when (report-at-depth-p part-name)
    (format *error-output* (symbol-value report-string-symbol) date part-name major-version minor-version comments)))

(defun do-report-detailed-version (report-string-symbol date part-name major-version minor-version micro-version comments)
  (when (report-at-depth-p part-name)
    (format *error-output* (symbol-value report-string-symbol) date part-name major-version minor-version micro-version comments)))

(eval-when (load eval)
  ;; deferred until we defined version-reporter here.
  (eval cl-user::*cl-lib-defpackage-version*)
  (eval cl-lib-initializations::*cl-lib-initializations-version*)
  (eval cl-lib-essentials::*cl-lib-essentials-version*))

;; if you want cl-lib to announce itself in a saved image...
(eval-when (load eval)
  (cl-lib-initializations:add-initialization "Announce cl-lib is on the job"
                                             '(when (and *cl-lib-version-announce-p*
                                                     (interactive-lisp-p))
                                                (report-version))
                                             '(:warm)))
