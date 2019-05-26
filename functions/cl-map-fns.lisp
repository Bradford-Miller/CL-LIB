(in-package cl-lib)

(version-reporter "CL-LIB-FNS-Map Fns" 5 0 ";; Time-stamp: <2012-01-05 17:45:44 millerb>" 
                  "CVS: $Id: cl-map-fns.lisp,v 1.2 2012/01/05 22:47:54 millerb Exp $
restructured version")

;;; 
;;; Copyright (C) 1996--1992 by Bradford W. Miller, miller@cs.rochester.edu
;;;                             and the Trustees of the University of Rochester
;;;
;;; Portions Copyright (C) 2002 by Bradford W. Miller, bradford.w.miller@gmail.com

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


;;;
;;; The following is contributed by miller@cs.rochester.edu

;; additions for other versions of lisp are welcome!

;;; yep, verrry similar to the one in zetalisp, so on the symbolics we
;;; use that one instead.
#+SYMBOLICS (EVAL-WHEN (COMPILE LOAD EVAL) (SETF (SYMBOL-FUNCTION 'MAPATOMS) #'ZL:MAPATOMS))
#-SYMBOLICS
(DEFUN MAPATOMS (FUNC &OPTIONAL (PACKAGE *PACKAGE*) (INHERITED-SYMBOLS-TOO T))
  "Maps the passed function over all symbols in the package, and if inherited-symbols-too is non-nil, then
over those symbols as well. Note that the function may be called >once on a symbol."
  (DO-SYMBOLS (SYM PACKAGE)
    (IF (OR INHERITED-SYMBOLS-TOO
	    (EQ PACKAGE (SYMBOL-PACKAGE SYM)))
	(FUNCALL FUNC SYM))))

;;; add dynamic-extent declaration 3/8/91 - bwm
;;; rewrite for greater efficiency 5/28/93 - bwm
(DEFMACRO MAPC-DOTTED-LIST (FN &REST LISTS)
  "Like normal Mapc, but handles dotted lists, and will apply the fn
to the dotted argument, unless it is NIL"
  (let ((arglist `(,@(mapcar #'(lambda (l)
                                 (declare (ignore l))
                                 (gensym)) ; entry for each list passed.
                             lists)))
        (fnv (gensym)))
    `(BLOCK mdl (let ((,fnv ,fn))       ; avoid multiple evaluation
                  (MAPLIST #'(lambda ,arglist
                               (funcall ,FNv ,@(mapcar #'(lambda (argname) `(car ,argname)) arglist))
                             ;; is cdr an atom
                             (COND
                              ((or ,@(mapcar #'(lambda (arg) `(and (atom (cdr ,arg)) (cdr ,arg))) arglist))
                               (funcall ,FNv ,@(mapcar #'(lambda (argname) `(cdr ,argname)) arglist))
                               (RETURN-FROM mdl (VALUES)))))
                         ,@LISTS))
            (VALUES))))

(macro-indent-rule mapc-dotted-list (like mapc))

;;; add dynamic-extent declaration & use gensyms for local vars 3/8/91 - bwm
;;; rewrite for greater efficiency 5/28/93 - bwm
(DEFMACRO MAPCAR-DOTTED-LIST (FN &REST LISTS)
  "Like normal Mapcar, but handles dotted lists, and will apply the fn
to the dotted argument, unless it is NIL"
  (LET ((RETURN-VAL (GENSYM))
        (last-retval (gensym))
        (last-retval1 (gensym))
	(LASTCDR (GENSYM))
        (arglist `(,@(mapcar #'(lambda (l)
                                 (declare (ignore l))
                                 (gensym)) ; entry for each list passed.
                             lists)))
        (fnv (gensym)))
    `(LET (,RETURN-VAL ,LASTCDR ,last-retval1 (,fnv ,fn)) ; avoid multiple evaluation
       (BLOCK mdl (MAPLIST #'(LAMBDA ,arglist
                               (let ((,last-retval (list (funcall ,FNv ,@(mapcar #'(lambda (argname) `(car ,argname)) arglist)))))
                                 (if ,return-val
                                     (nconc ,last-retval1 ,last-retval)
                                   (setq ,return-val ,last-retval))
                                 (setq ,last-retval1 ,last-retval))
                               ;; is cdr an atom
			       (COND
                                ((or ,@(mapcar #'(lambda (arg) `(and (atom (cdr ,arg)) (cdr ,arg))) arglist))
                                 (SETQ ,LASTCDR (funcall ,FNv ,@(mapcar #'(lambda (argname) `(cdr ,argname)) arglist)))
                                 (RETURN-FROM mdl (values)))))
			   ,@LISTS))
       (NCONC ,RETURN-VAL ,LASTCDR))))

(macro-indent-rule mapcar-dotted-list (like mapcar))

;;; add dynamic-extent declaration & use gensyms for local vars 3/8/91 - bwm
;;; rewrite for greater efficiency 5/28/93 - bwm
(DEFMACRO MAPCAN-DOTTED-LIST (FN &REST LISTS)
  "Like normal Mapcan, but handles dotted lists, and will apply the fn
to the dotted argument, unless it is NIL"
  (LET ((RETURN-VAL (GENSYM))
        (fnv (gensym))
        (arglist `(,@(mapcar #'(lambda (l)
                                 (declare (ignore l))
                                 (gensym)) ; entry for each list passed.
                             lists))))
     `(LET (,RETURN-VAL (,fnv ,fn))
	(BLOCK mdl (MAPLIST #'(lambda ,arglist
				(SETQ ,RETURN-VAL (NCONC ,RETURN-VAL (funcall ,FNv ,@(mapcar #'(lambda (argname) `(car ,argname)) arglist))))
				;; is cdr an atom
				(COND
				  ((or ,@(mapcar #'(lambda (arg) `(and (atom (cdr ,arg)) (cdr ,arg))) arglist))
				   (SETQ ,RETURN-VAL (NCONC ,RETURN-VAL (funcall ,FNv ,@(mapcar #'(lambda (argname) `(cdr ,argname)) arglist))))
				   (RETURN-FROM mdl (VALUES)))))
			    ,@LISTS))
	,RETURN-VAL)))

(macro-indent-rule mapcan-dotted-list (like mapcan))
;;; rewrite for greater efficiency 5/28/93 - bwm
(DEFMACRO MAPLIST-DOTTED-LIST (FN &REST LISTS)
  "Like normal Maplist, but handles dotted lists, and will apply the
fn to the dotted argument, unless it is NIL"
  (LET ((RETURN-VAL (GENSYM))
        (fnv (gensym))
        (arglist `(,@(mapcar #'(lambda (l)
                                 (declare (ignore l))
                                 (gensym)) ; entry for each list passed.
                             lists))))
     `(LET (,RETURN-VAL (,fnv ,fn))
	(BLOCK mdl (MAPLIST #'(lambda ,arglist
				(SETQ ,RETURN-VAL (nconc ,RETURN-VAL (list (funcall ,FNv ,@arglist))))
				;; is cdr an atom
				(COND
				  ((or ,@(mapcar #'(lambda (arg) `(and (atom (cdr ,arg)) (cdr ,arg))) arglist))
				   (SETQ ,RETURN-VAL (nconc ,RETURN-VAL (list (funcall ,FNv ,@(mapcar #'(lambda (argname) `(cdr ,argname)) arglist)))))
				   (RETURN-FROM mdl (VALUES)))))
			    ,@LISTS))
	,RETURN-VAL)))

(macro-indent-rule maplist-dotted-list (like maplist))

;;; add dynamic-extent declaration 3/8/91 - bwm
;;; rewrite for greater efficiency 5/28/93 - bwm
(DEFMACRO SOME-DOTTED-LIST (FN &REST LISTS)
  "Like normal Some, but handles a single dotted list, and will apply
the fn to the dotted argument, unless it is NIL"
  (let ((fnv (gensym))
        (arglist `(,@(mapcar #'(lambda (l)
                                 (declare (ignore l))
                                 (gensym)) ; entry for each list passed.
                             lists))))
    `(let ((,fnv ,fn))
       (BLOCK sdl (MAPLIST #'(lambda ,arglist
                               (IF (funcall ,FNv ,@(mapcar #'(lambda (argname) `(car ,argname)) arglist))
                                   (RETURN-FROM sdl T)
                                 ;; is cdr an atom
                                 (COND
                                  ((or ,@(mapcar #'(lambda (arg) `(and (atom (cdr ,arg)) (cdr ,arg))) arglist))
                                   (IF (funcall ,FNv ,@(mapcar #'(lambda (argname) `(cdr ,argname)) arglist))
                                       (RETURN-FROM sdl T)
                                     (RETURN-FROM sdl NIL))))))
                           ,@LISTS)
              NIL))))                    ;fell thru maplist w/o return

(macro-indent-rule some-dotted-list (like some))

;;; add dynamic-extent declaration 3/8/91 - bwm
;;; rewrite for greater efficiency 5/28/93 - bwm
(DEFMACRO EVERY-DOTTED-LIST (FN &REST LISTS)
  "Like normal Every, but handles dotted lists, and will apply the fn
to the dotted arguments, unless they are (all) NIL."
  (let ((fnv (gensym))
        (arglist `(,@(mapcar #'(lambda (l)
                                 (declare (ignore l))
                                 (gensym)) ; entry for each list passed.
                             lists))))
    `(let ((,fnv ,fn))
       (BLOCK Edl (MAPLIST #'(lambda ,arglist
                               (IF (funcall ,FNv ,@(mapcar #'(lambda (argname) `(car ,argname)) arglist))
                                   ;; is cdr an atom
                                   (COND
                                    ((or ,@(mapcar #'(lambda (arg) `(and (atom (cdr ,arg)) (cdr ,arg))) arglist))
                                     (IF (funcall ,FNv ,@(mapcar #'(lambda (argname) `(cdr ,argname)) arglist))
                                         (RETURN-FROM Edl T)
                                       (RETURN-FROM Edl NIL))))
                                 (RETURN-FROM Edl NIL)))
                           ,@LISTS)
              T))))                      ;fell thru maplist w/o return

(macro-indent-rule every-dotted-list (like every))

(defmacro dosequence ((var sequence &optional result) &BODY body)
  "(dosequence (var sequence &optional result) &body body)      [macro]
This macro is like DOLIST \(q.v.), except that the iteration is over
any sequence, not necessarily a list."
#+lispm  (declare (zwei:indentation 1 1))
  (check-type var symbol)
  (let ((iter-index (gensym))
        (iter-limit (gensym)))
    `(do* ((,var)
           (,iter-limit (length ,sequence))
           (,iter-index 0 (+ ,iter-index 1)))
         ((= ,iter-index ,iter-limit)
          (setq ,var nil)
          ,result)
       (setq ,var (elt ,sequence ,iter-index))
       ,@body)))

(macro-indent-rule dosequence (like dolist))

