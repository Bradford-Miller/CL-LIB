(in-package cl-lib)(cl-lib:version-reporter "CL-LIB-FNS-Extensions" 5 16                          ";; Time-stamp: <2019-03-03 11:47:39 Bradford Miller(on Aragorn.local)>"                          "sbcl: get-compiled-function-name, getenv port");; 5.16 2/23/19 port get-compiled-function-name and getenv to sbcl;;; ;;; Copyright (C) 1996--1992 by Bradford W. Miller, miller@cs.rochester.edu;;;                             and the Trustees of the University of Rochester;;;;;; Portions Copyright (C) 2002,2019 by Bradford W. Miller, bradford.w.miller@gmail.com;; This library is free software; you can redistribute it and/or modify it under the terms of the GNU ;; Lesser General Public License as published by the Free Software Foundation; either version 3.0 of ;; the License, or (at your option) any later version.;; This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; ;; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. ;; See the GNU Lesser General Public License for more details.;; You should have received a copy of the GNU Lesser General Public License along with this library; ;; if not, see <http://www.gnu.org/licenses/>.;; miller - Brad Miller (bradfordmiller@mac.com);;;;;; The following is contributed by miller@cs.rochester.edu ;; additions for other versions of lisp are welcome!;;#+ALLEGRO-V4.0(EVAL-WHEN (COMPILE LOAD EVAL)  (setf (symbol-function 'COMMON-LISP:hash-table-size) (symbol-function 'excl::hash-table-buckets)	(symbol-function 'COMMON-LISP:hash-table-test) (symbol-function 'excl::hash-table-kind)));;(DEFUN COPY-HASH-TABLE (OLD-HASH-TABLE)  #+SYMBOLICS (CLI::COPY-TABLE OLD-HASH-TABLE)  #-SYMBOLICS  (let ((new-hash (make-hash-table		    :test (hash-table-test old-hash-table)		    :size (hash-table-size old-hash-table)		    :rehash-size (hash-table-rehash-size old-hash-table)		    :rehash-threshold (hash-table-rehash-threshold old-hash-table))))    (maphash #'(lambda (key entry)		 (setf (gethash key new-hash) entry))	     old-hash-table)    NEW-HASH));;; fix to not use "declare" options in non-let clause - 3/14/91 bwm;;; fix for optimized expansion when condition is already known;;; (constant) nil or non-nil.(DEFMACRO LET-MAYBE (CONDITION BINDINGS &BODY BODY)  "Binds let arguments only if condition is non-nil, and evaluates body in any case."  (cond   ((null condition)    `(PROGN ,@(IF (EQ (CAAR BODY) 'DECLARE) (CDR BODY) BODY)))   ((eq condition t)    `(let ,bindings ,@body))   (t                                   ;defer to runtime    `(IF ,CONDITION         (LET ,BINDINGS           ,@BODY)       (PROGN ,@(IF (EQ (CAAR BODY) 'DECLARE) (CDR BODY) BODY))))))(macro-indent-rule let-maybe ((1 1 quote) (0 2 1)));;; Explicit tagbody, with end-test at the end, to be nice to poor;;; compilers.(defmacro while (test &body body)  "Keeps invoking the body while the test is true;   test is tested before each loop."  (let ((end-test (gensym))	(loop (gensym)))    `(block nil       (tagbody (go ,end-test) 		,loop		,@body		,end-test		(unless (null ,test) (go ,loop))		(return)))))(macro-indent-rule while 1)(defmacro while-not (test &body body)  "Keeps invoking the body while the test is false;   test is tested before each loop."  (let ((end-test (gensym))	(loop (gensym)))    `(block nil       (tagbody (go ,end-test)		,loop		,@body		,end-test		(unless ,test (go ,loop))		(return)))))(macro-indent-rule while-not 1)(defmacro let*-non-null (bindings &body body)  "like let*, but if any binding is made to NIL, the let*-non-nullimmediately returns NIL."#+symbolics  (declare lt:(arg-template ((repeat let)) declare . body))  `(block lnn (let* ,(mapcar #'process-let-entry bindings)                    ,@body)))(macro-indent-rule let*-non-null (like let))(defun process-let-entry (entry)  "if it isn't a list, it's getting a nil binding, so generate areturn. Otherwise, wrap with test."  (declare (optimize (speed 3) (safety 0)))  (if (atom entry)      `(,entry (return-from lnn nil))      `(,(car entry) (or ,@(cdr entry) (return-from lnn nil)))))(defmacro msetq (vars value)#+lispm  (declare (compiler:do-not-record-macroexpansions)                  (zwei:indentation 1 1)) `(multiple-value-setq ,vars ,value))(macro-indent-rule msetq (like multiple-value-setq))(defmacro mlet (vars value &body body)#+lispm  (declare (compiler:do-not-record-macroexpansions)                  (zwei:indentation 1 3 2 1))   `(multiple-value-bind ,vars ,value ,@body))(macro-indent-rule mlet (like multiple-value-bind));;; the following is contributed by quiroz@cs.rochester.edu with;;; slight modifications by miller@cs.rochester.edu(defmacro cond-binding-predicate-to (symbol &rest clauses)  "(cond-binding-predicate-to symbol . clauses)                      [macro]a COND-like macro.  The clauses are exactly as in COND.  In the bodyof a clause, the SYMBOL is lexically bound to the value returned by thetest.  Example:   (cond-binding-predicate-to others    ((member 'x '(a b c x y z))     (reverse others)))evaluates to    (z y x)"#+lispm  (declare (zwei:indentation 0 3 1 1))  (check-type symbol symbol)  `(let (,symbol)     (cond ,@(mapcar #'(lambda (clause)                         `((setf ,symbol ,(first clause))                           ,@(rest clause)))                     clauses))))(macro-indent-rule cond-binding-predicate-to (like case))(defun Elapsed-Time-in-Seconds (Base Now)  "Returns the time in seconds that has elapsed between Base and Now.Just subtracts Base from Now to get elapsed time in internal time units,then divides by the number of internal units per second to get seconds."  (coerce  (/  (- Now Base)  internal-time-units-per-second)  'float));; OK, how many times have you written code of the form;;;; (let ((retval (mumble)));;    (setf (slot retval) bletch);;    (setf (slot retval) barf);;    retval);; or things of the sort? More than you care to remember most;; likely. Enter the utterly useful PROGFOO.  Think of it as a PROG1;; with the value being bound to FOO. inside it's extent Lexically, of;; course.(defmacro progfoo (special-term &body body)  `(let ((foo ,special-term))     ,@body     foo))(macro-indent-rule progfoo (like prog1))(defmacro with-rhyme (body)  "Well, there must be rhyme OR reason, and we now admit there is noreason, so...  Used to flag silly constructs that may need to berewritten for best effect."  body)(macro-indent-rule with-rhyme (like progn));; and for common lisp fans of multiple values... FOO is the first;; value, you can access all the values as MV-FOO.  returns the;; multiple values, like multiple-values-prog1(defmacro mv-progfoo (special-term &body body)  `(let* ((mv-foo (multiple-value-list ,special-term))          (foo (car mv-foo)))     ,@body     (values-list mv-foo)))(macro-indent-rule mv-progfoo (like multiple-value-prog1));; from the net;; From: Kerry Koitzsch <kerry@erg.sri.com>(defun GET-COMPILED-FUNCTION-NAME (fn)  "Returns the symbol name of a function. Covers the six major CL vendors."  #+lispm  (when (si:lexical-closure-p fn)    (return-from get-compiled-function-name nil))  (etypecase fn     (symbol fn)    (compiled-function #+cmu(kernel:%function-header-name fn)                       #+:mcl(ccl::function-name fn)                       #+lispm(si:compiled-function-name fn)                       #+akcl(system::compiled-function-name fn)                       #+lucid                       (when (sys:procedurep fn)                         (sys:procedure-ref fn SYS:PROCEDURE-SYMBOL))                       #+excl (xref::object-to-function-name fn)                       ;; modified 6/4/02 by abm061 for Lispworks 4.2                       ;; note that it is only effective for debug level 1 or more.                       #+lispworks                       (mlet (a b name) (function-lambda-expression fn)                             (declare (ignore a b))                             name)                       ;; new 2/23/19 BWM                       #+sbcl (sb-c::%fun-name fn)                       )));; back to miller@cs.rochester.edu;; This may seem like a silly macro, but used inside of other macros;; or code generation facilities it is very useful - you can see;; comments in the (one-time) macro expansion!(defmacro comment (&rest anything)  "Expands into nothing"  (declare (ignore anything)));; command line manipulation(defun command-line-arg (command-line argname default &optional no-read)  (cond-binding-predicate-to foo    ((member argname command-line :test #'equalp             :key #'(lambda (x) (if (symbolp x) (string x) x)))     (if no-read         (values (second foo) t)       (let ((read-value (if (stringp (second foo))                             (read-from-string (second foo) nil :eof)                           (second foo))))         (if (eq read-value :eof)             (values default nil)           (values read-value t)))))    (t     (values default nil))));; getenv - already exists in lispworks as of v5#-lispworks (defun getenv (env-variable)  #+excl (sys:getenv env-variable)  #+lispworks (lw:environment-variable env-variable)  ;; 2/23/19 BWM  #+sbcl (posix-getenv env-variable)  #-(or excl lispworks sbcl)  (error "CL-LIB: getenv: Port me!"))#-EXCL(defmacro if* (condition true &rest false)  `(if ,condition ,true (progn ,@false)));;; ********************************;;; misc ***************************;;; ********************************;;; we define a variable to be a symbol of the form ?NAME, i.e., a;;; symbol whose first character is #\?. (Used in various logic languages like RHET, Shocker, etc.)(defun make-variable (x)  (make-symbol (format nil "?~a" x)))(defun variablep (item)  "Returns T if ITEM is a variable, namely a symbol of the form ?NAME,   whose first character is a question-mark."  (and (symbolp item)       (char= (char (symbol-name item) 0)              #\?)));;; ********************************;;; Noting Progress ****************;;; ********************************(defmacro noting-progress ((&optional (width 70)) &body body)  "Every time NOTE-PROGRESS is called within the body of a NOTING-PROGRESS   macro, it prints out a dot. Every width number of dots it also outputs   a carriage return."  (let ((dot-counter (gensym "DOT")))    `(let ((,dot-counter 0))       (declare (special ,dot-counter))       (flet ((note-progress ()		(incf ,dot-counter)		(when (> ,dot-counter ,width)		  (setf ,dot-counter 0)		  (terpri))		(princ #\.)))	 ,@body))));;; *EOF*