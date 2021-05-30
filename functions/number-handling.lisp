;;; -*- Mode: LISP; Syntax: ansi-common-lisp; Package: CL-LIB; Base: 10 -*-(in-package cl-lib)(version-reporter "CL-LIB-FNS-Number Handling" 5 1 ";; Time-stamp: <2007-05-18 11:35:41 miller>"                   "CVS: $Id: number-handling.lisp,v 1.1.1.1 2007/11/19 17:41:31 gorbag Exp $add round-off");;; ;;; Copyright (C) 1994, 1993, 1992 by Bradford W. Miller, miller@cs.rochester.edu;;;                                and the Trustees of the University of Rochester;;; Right of use & redistribution is granted as per the terms of the ;;; GNU LIBRARY GENERAL PUBLIC LICENCE version 2 which is incorporated here by;;; reference. ;;; This program is free software; you can redistribute it and/or modify;;; it under the terms of the GNU Library General Public License as published by;;; the Free Software Foundation; version 2.;;; This program is distributed in the hope that it will be useful,;;; but WITHOUT ANY WARRANTY; without even the implied warranty of;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the;;; GNU Library General Public License for more details.;;; You should have received a copy of the GNU Library General Public License;;; along with this program; if not, write to the Free Software;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.;;;;;; The following is contributed by miller@cs.rochester.edu;;; the following is contributed by baldwin@cs.geneseo.edu(defun Bit-Length (N)  " Computes the number of bits needed to represent integer N.Assumes that 0 requires 1 bit to represent, positive numbers requirefloor(log(N))+1 bits, and negative numbers require one bit more thantheir positive counterparts (for the sign bit). This treatment ofnegative integers is a little bit arbitrary, but seems as good asanything else."  (cond    ((= N 0)  1)    ((< N 0)  (+ (Bit-Length (- N)) 1))    ((> N 0)  (+ (floor (log N 2)) 1))))(defun Sum-of-Powers-of-Two-Representation (N)  "Figures out how to represent N as a sum of powers of two. Returns a list of exponents,the idea being the N is the sum over E in this list of two raised to the E-th power. Requires N to be a positive integer, so that all exponents in the result list are integers."  (declare (integer N))  (assert (> N 0))  (do ( (I 0 (+ I 1))        (Exps '() (if (logbitp I N)		      (cons I Exps)		      Exps)) )      ((>= I (integer-length N))  Exps)    (declare (integer I) (list Exps))))(defun Difference-of-Powers-of-Two-Representation (N)  "Figures out how to represent N as the difference of a sequence of powers of 2  (e.g., 2^e1 - 2^e2 - ...). Returns a list of exponents, with e1 as the last andthe others in some arbitrary order. Requires N to be an integer greater than 0,which simplifies the code but isn't absolutely necessary. Starts by figuring out The smallest power of two greater than or equal to N - this exponent becomes e1. Remaining exponents are just those of the greater power of two minus N."  (declare (integer N))  (assert (> N 0))  (let* ((E1 (ceiling (log N 2)))	 (Next-Power (expt 2 E1)))    (declare (integer E1 Next-Power))    (if (= Next-Power N)	(list E1)	(append (Sum-of-Powers-of-Two-Representation (- Next-Power N)) (list E1)))))(defun Ordinal-String (N)  " Generates a string representing N as an ordinal number (i.e., 1st, 2nd, etc.). Works by printing N and the appropriate suffix to a string - N is printed in decimal, the suffix is looked up based on the last digit of N (i.e., N mod 10)."  (declare (integer N))  (let ((Last-Digit (mod (abs N) 10))	(Last-2-Digits (mod (abs N) 100)))    (declare (integer Last-Digit))    (format nil "~d~a" N (cond			   ((or (= Last-2-Digits 11)				(= Last-2-Digits 12)				(= Last-2-Digits 13))         "th")			   ((= Last-Digit 1)                  "st")			   ((= Last-Digit 2)                  "nd")			   ((= Last-Digit 3)                  "rd")			   (t                                 "th")))))(defun Between (Lo Hi)  "Generates a list of integers between Lo and Hi, inclusive. Straightforward recursive definition, i.e., result is Lo consed ontointegers from Lo+1 to Hi, unless Lo is greater than Hi in which caseresult is nil."  (declare (integer Lo Hi))  (cond    ((> Lo Hi)  '() )    (t   (cons Lo (Between (+ Lo 1) Hi)))));; miller@cs.rochester.edu(DEFUN ROUND-TO (NUMBER &OPTIONAL (DIVISOR 1))  "Like Round, but returns the resulting number"  (* (ROUND NUMBER DIVISOR) DIVISOR))(defun factorial (n)  "Compute the factorial of an integer"  (cond ((minusp n)	 (cerror "Compute -(~D!) instead" "I can't do -~D!" n n)	 (factorial (- n)))	(t	 (do ((x n (1- x))	      (result 1))	     ((zerop x) result)	   (declare (fixnum x))	   (setf result (* x result))))));; ferguson@cs.rochester.edu(defun round-off (n &optional (sig 0))  "rounds n off to sig significant figures (positive is right of decimal, negative is left), returning a float."  (/ (fround n (expt 10 (- sig)))     (expt 10 sig)))