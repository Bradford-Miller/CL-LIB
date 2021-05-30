;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: CL-LIB; Base: 10 -*-
(in-package CL-LIB)

(cl-lib:version-reporter "CL-LIB-Scheme-Streams" 5 0 ";; Time-stamp: <2012-01-05 17:47:16 millerb>" 
                         "CVS: $Id: scheme-streams.lisp,v 1.2 2012/01/05 22:47:56 millerb Exp $
restructured version")


;;; 
;;; Copyright (C) 1996--1992 by Bradford W. Miller, miller@cs.rochester.edu
;;;                             and the Trustees of the University of Rochester
;;;
;;; Portions Copyright (C) 2002,2003 by Bradford W. Miller, bradford.w.miller@gmail.com

;;; Right of use & redistribution is granted as per the terms of the 
;;; GNU LIBRARY GENERAL PUBLIC LICENCE version 2 which is incorporated here by
;;; reference. 

;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; version 2.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Library General Public License for more details.

;;; You should have received a copy of the GNU Library General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;

;; scheme-stream (thanks to barmar@think.com for original code posted
;; in response to my question in comp.lang.lisp. "Improvements" are all mine,
;; and any errors are not to be blamed on him.)

;; Why use these instead of generators & series? Well, you can use
;; both. Note that generators discard their output, while
;; scheme-streams cache their output. What constitutes a tail-form
;; isn't specified. So it's valid to put, e.g. a Water's generator
;; there, or at least a fn that conses up a new scheme-stream whose
;; head is the result of the call on the generator, and whose tail is
;; another call on this fn.

(defstruct scheme-stream
  head
  tail
  (tail-closure-p t))

(defmacro cons-scheme-stream (head tail-form)
  `(make-scheme-stream :head ,head
		       :tail #'(lambda () ,tail-form)))

(defmacro list-scheme-stream (&rest args)
  "Analogue to the cl list function, only the last arg is delayed."
  (assert (> (list-length args) 1) (args)
          "List-scheme-stream requires at least 2 args.")
  (let* ((revargs (nreverse args))
         (result `(cons-scheme-stream ,(second revargs) ,(pop revargs))))
    (pop revargs)
    (while revargs
      (setq result `(make-scheme-stream :head ,(pop revargs)
                     :tail ,result :tail-closure-p nil)))
    result))

(defun rlist-to-scheme-stream (rlist delay)
  (let ((result (make-scheme-stream :head (pop rlist) :tail delay)))
    (while rlist
      (setq result (make-scheme-stream :head (pop rlist)
                                       :tail result :tail-closure-p nil)))
    result))

(defun ss-head (stream)
  "Return the head of scheme stream Stream. If Stream is not a stream,
returns NIL (to allow the usual car of nil)."
  (declare (optimize (speed 3) (safety 0)))
  (if (scheme-stream-p stream)
      (scheme-stream-head stream)))     ;return nil for non-streams

(defun ss-tail (stream)
  "Return the tail of the scheme stream Stream. Invokes lazy
evaluation if needed.  If stream is not a scheme stream, return NIL
\(allows the usual cdr of nil)."
  (declare (optimize (speed 3) (safety 0))
           (values tail from-closure-p))
  (cond
    ((not (scheme-stream-p stream))
     (values nil nil))
    ((scheme-stream-tail-closure-p stream)
     (values (setf (scheme-stream-tail-closure-p stream) nil
                   (scheme-stream-tail stream) (funcall
                                                (scheme-stream-tail stream)))
             t))
    (t (values (scheme-stream-tail stream) nil))))

;; scheme force/delay model

(defstruct scheme-delay
  (first-time-p t)
  value)

(defmacro scheme-delay (form)
  `(make-scheme-delay :value #'(lambda () ,form)))

(defmacro fake-scheme-delay (form)
  `(make-scheme-delay :value ,form :first-time-p nil))

(defun scheme-force (delay)
  (declare (type scheme-delay delay)
           (values delay-value first-time-p))
  (assert (scheme-delay-p delay) (delay)
          "Attempt to force ~S, which is not a delay!" delay)
  (let ((first-time-p (scheme-delay-first-time-p delay)))
    (values
     (cond (first-time-p
            (setf (scheme-delay-first-time-p delay) nil
                  (scheme-delay-value delay) (funcall
                                              (scheme-delay-value delay))))
           (t (scheme-delay-value delay)))
     first-time-p)))

(pushnew :cl-lib-scheme-streams *features*)
