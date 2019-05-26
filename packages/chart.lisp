;;	$Id: chart.lisp,v 1.2 2008/05/03 17:42:02 gorbag Exp $	;; Author Bradford W. Miller (miller@mcc.com)(cl-lib:version-reporter "CL-LIB-Chart" 5 7 ";; Time-stamp: <2008-05-03 13:13:44 gorbag>"                          "CVS: $Id: chart.lisp,v 1.2 2008/05/03 17:42:02 gorbag Exp $;; documentation");;; ;;; Copyright (C) 1994, 1993, 1992 by Bradford W. Miller, miller@mcc.com;;; Unlimited use is granted to the end user, other rights to all users;;; are as granted by the GNU LIBRARY GENERAL PUBLIC LICENCE;;; version 2 which is incorporated here by reference.;;; This program is free software; you can redistribute it and/or modify;;; it under the terms of the GNU Library General Public License as published by;;; the Free Software Foundation; version 2.;;; This program is distributed in the hope that it will be useful,;;; but WITHOUT ANY WARRANTY; without even the implied warranty of;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the;;; GNU Library General Public License for more details.;;; You should have received a copy of the GNU Library General Public License;;; along with this program; if not, write to the Free Software;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.;;;(defpackage chart  (:use common-lisp cl-lib)  (:export #:generic-chart #:generic-chart-p           #:generic-chart-entry #:generic-chart-entry-p           #:chart-entries-starting-at-index #:chart-entries-ending-at-index           #:chart-entry-indices #:chart-entry-label #:chart-entry-parents #:chart-last-index           #:chart-entries-next #:chart-entries-prior #:add-chart-entry           #:annotate-chart-entry #:chart-entry-annotation           #:chart-entry-content)  (:documentation "Chart abstraction, as used in chart parsers. Charts can be viewed as lego blocks: ---------------------------------------- |                  S                   | +--------------------------------------+ |        NP        |        VP         | +------------------+-------------------+ |   DET   |   N    |    V    |   ADVB  | +---------+--------+---------+---------+ |   the   |  boy   |   ran   | quickly |   input ---------------------------------------- 0         1        2         3         4   index In the above example, we have used it as a chart parser might, showing that the labels for each chart consituent constitutes a labelling of those sentence parts that are covered by the block.  Charts have the following functionality: Every chart constituent (lego block) has a index for its start and end position. we can ask for blocks that end at the point we start, or begin at the point we end.  Blocks can relable any other blocks they cover in the chart.   No block is inherently better than another, although typically we use charts to find covers of the input. the input being a list of tokens (when used as a parser). In this generalization of charts, we do not distinguish the input except that it will not be marked as a  relabelling of something. That is, it's possible to have your input cover more than one index if you want to. Because we are creating an abstraction of charts here (although a simple implementation is also provided for testing), we do not specify even the data structure(s) for charts, applications are free to optimize these for how they will use charts. Note that the choice of data structure implementation, and supporting methods for the generic functions defined below will determine the actual performace of your chart."))(in-package chart);; Chart abstraction, as used in chart parsers.;; This is used by pat-match-chart (the pattern match function), currently residing in the lymphocyte directory.;; Charts can be viewed as lego blocks:;; ----------------------------------------;; |                  S                   |;; +--------------------------------------+;; |        NP        |        VP         |;; +------------------+-------------------+;; |   DET   |   N    |    V    |   ADVB  |;; +---------+--------+---------+---------+;; |   the   |  boy   |   ran   | quickly |   input;; ----------------------------------------;;;; 0         1        2         3         4   index;; In the above example, we have used it as a chart parser might, showing that the labels for each chart consituent;; constitutes a labelling of those sentence parts that are covered by the block. ;;;; Charts have the following functionality:;; Every chart constituent (lego block) has a index for its start and end position.;; we can ask for blocks that end at the point we start, or begin at the point we end.;; ;; Blocks can relable any other blocks they cover in the chart. ;; ;; No block is inherently better than another, although typically we use charts to find covers of the input.;; the input being a list of tokens (when used as a parser).;; In this generalization of charts, we do not distinguish the input except that it will not be marked as a ;; relabelling of something. That is, it's possible to have your input cover more than one index if you want to.;; Because we are creating an abstraction of charts here (although a simple implementation is also provided for testing),;; we do not specify even the data structure(s) for charts, applications are free to optimize these for how they will use;; charts. Note that the choice of data structure implementation, and supporting methods for the generic functions;; defined below will determine the actual performace of your chart.;; A chart is a data object. Because we are not creating a particular chart here, our object will necessarily be ;; fairly sparse:(defclass-x generic-chart ()  ()  (:documentation "A chart is a way to collect data similar to an array, but entries can cover more than one index. Typically, access is from (0,1) rather than (0), i.e. the indexes are vertices, and the data are segments betweenvertices in the graph."))(defclass-x generic-chart-entry ()  ()  (:documentation "A chart entry is a constituent of a chart."))(defgeneric chart-entries-starting-at-index (chart index)  (:documentation "Return the set of chart entries that start at the index in the chart"))(defgeneric chart-entries-ending-at-index (chart index)  (:documentation "Return the set of chart entries that end at the index in the chart"))(defgeneric chart-entry-indices (chart-entry)  (:documentation "Return a list of two values, (S E) indicating the start and end index covered by the chart-entry in the chart"))(defgeneric chart-entry-label (chart-entry)  (:documentation "Return the label or name of the chart entry, e.g. \"NP\"."))(defgeneric chart-entry-content (chart-entry)  (:documentation "Return the content of the chart entry (what it is we match against). This might just be the label."))(defgeneric chart-entry-parents (chart-entry)  (:documentation "When a chart entry is derived from others, this function returns the entries that it was derived from, useful, e.g., in chart parsers to know which det and n a np entry covers, etc. Inputs will return NIL"))(defgeneric chart-last-index (chart)  (:documentation "Return the highest index in the chart, i.e. the maximum index covered by some chart-entry"))(defgeneric chart-entries-next (chart chart-entry)  (:documentation "Return the chart entries that start at the end index of the passed chart entry"))(defgeneric chart-entries-prior (chart chart-entry)  (:documentation "Return the chart entries that end at the start index of the passed chart entry"))(defgeneric add-chart-entry (chart label start end &optional content parents &key)  (:documentation "Creates a chart entry spanning the start and end index, with given label, and adds it to the chart. Note that other keywords are allowed, so you can put more information on your chart entry (and pass them to add-chart-entry). Returns the new chart-entry"))(defgeneric annotate-chart-entry (chart chart-entry annotation-label annotation)  (:documentation "Effectively, annotation-label is a property-name, and annotation is the property."))(defgeneric chart-entry-annotation (chart chart-entry annotation-label)  (:documentation "Effectively, annotation-label is a property-name, we return the property."))(eval-when (load eval compile)  (defsetf chart-entry-annotation annotate-chart-entry));; here's some generic methods on the above. These won't be particularly efficient, but it allows one to define ;; fewer custom methods for their implementation, and get going. Later you can define all the methods for efficiency.(defmethod chart-entries-next ((chart generic-chart) (chart-entry generic-chart-entry))  "Dumb but simple"  (chart-entries-starting-at-index chart (second (chart-entry-indices chart-entry))))(defmethod chart-entries-prior ((chart generic-chart) (chart-entry generic-chart-entry))  "Dumb but simple"  (chart-entries-ending-at-index chart (first (chart-entry-indices chart-entry))))(defmethod print-object ((o generic-chart-entry) stream)  (print-unreadable-object (o stream :type t :identity t)    (format stream "~A <~D,~D>" (chart-entry-label o) (first (chart-entry-indices o)) (second (chart-entry-indices o)))));; here's an implementation of a very simple chart that obeys the above protocols. It has some limitations: (defclass-x simple-chart (generic-chart)  ((chart-entries :type list :initform nil :accessor chart-entries)))(defclass-x simple-chart-entry (generic-chart-entry)  ((chart-entry-label :type symbol :initarg :chart-entry-label :accessor chart-entry-label) ; see defgeneric   (chart-entry-content :type t :initarg :chart-entry-content :accessor chart-entry-content) ; see defgeneric   (chart-entry-indices :type list :initarg :chart-entry-indices :accessor chart-entry-indices) ; see defgeneric   (chart-entry-parents :initform nil :initarg :chart-entry-parents :accessor chart-entry-parents) ; see defgeneric      (chart-entry-plist :initform nil :type alist :accessor chart-entry-plist :initarg :chart-entry-plist                      :documentation "All chart entries must support a property list for portably adding annotations in code that deals with generic charts. Individual overrides with more efficient annotations are possible. See annotate-chart-entry.")))(defmethod add-chart-entry ((chart simple-chart) label start end &optional (content label) parents &key)  (cl-lib:progfoo (make-simple-chart-entry                    :chart-entry-label label                   :chart-entry-indices (list start end)                   :chart-entry-parents parents                   :chart-entry-content content)                  (push foo (chart-entries chart))))(defmethod chart-entries-starting-at-index ((chart simple-chart) index)  "Dumb but simple"  (remove-if-not #'(lambda (entry) (eql index (first (chart-entry-indices entry)))) (chart-entries chart)))(defmethod chart-entries-ending-at-index ((chart simple-chart) index)  "Dumb but simple"  (remove-if-not #'(lambda (entry) (eql index (second (chart-entry-indices entry)))) (chart-entries chart)))(defmethod chart-last-index ((chart simple-chart))  (let ((result 0))    (dolist (entry (chart-entries chart))      (let ((end-point (second (chart-entry-indices entry))))        (when (> end-point result)          (setq result end-point))))    result))(defmethod annotate-chart-entry (chart (chart-entry simple-chart-entry) annotation-label annotation)  (declare (ignore chart))  (update-alist annotation-label annotation (chart-entry-plist chart-entry))  annotation)(defmethod chart-entry-annotation (chart (chart-entry simple-chart-entry) annotation-label)  (declare (ignore chart))  (cdr (assoc annotation-label (chart-entry-plist chart-entry))))(pushnew :cl-lib-chart *features*)