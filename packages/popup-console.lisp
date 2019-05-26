(cl-lib:version-reporter "CL-LIB-Popup Console" 5 7 ";; Time-stamp: <2007-11-22 22:50:30 gorbag>" 
                         "CVS: $Id: popup-console.lisp,v 1.1.1.1 2007/11/26 15:13:24 gorbag Exp $
;; documentation")

;; 5.1 5/18/07 Title Option (LispWorks Native Windows (CAPI))

;;; Copyright (C) 2005 by Bradford W. Miller and Raytheon Corporation.
;;; Unlimited non-commercial use is granted to the end user, other rights to
;;; the non-commercial user are as granted by the GNU LIBRARY GENERAL PUBLIC LICENCE
;;; version 2 which is incorporated here by reference.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; Gnu Library General Public License for more details.

;;; You should have received a copy of the Gnu Library General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Written 6/2005 by Bradford W. Miller bradfordmiller@mac.com

;; The motivation for this package, initially, was the lack of a distiguished *error-output* in LispWorks, 
;; that is, unlike allegro, access to file descriptor 2 was not available, and so programs that are intended 
;; to be pipes have no place to put an error, other than a file.
;;
;; This package allows one to create a stream into a popup console window and bind *error-output* or pretty much 
;; anything else to it.
;;
;; The initial implementation uses CAPI (since I'm on LispWorks), however, the port to CLIM should be straightforward
;; (and I'm sure I'll get around to it ;-), since they are more similar than different.
;;
;; How to use:
;; First, call (create-console). By default, a read-only console is created, suitable for displaying errors 
;;   as your program prints them, (create-console :rw) will put up an editor buffer (read/write). 
;;   In either case, a console object is returned, which is needed for the following calls. Optionally, you can
;;   specify a second argument, which will be the title of the window.
;; 
;; (console-stream console) returns a stream associated with a console, e.g., the thing to format to.
;;
;; (format-console console format-string &rest args) is format for a console.
;;
;; (read-line-console console &optional eof-error-p eof-value) is read-line for a console, but the console 
;;     argument is required.
;;
;; That's it! Simple and straightforward.

(defpackage cl-lib-console 
  (:use #:common-lisp #+lispworks #:capi #:mp)
  (:export #:create-console #:destroy-console #:console-stream #:format-console #:read-line-console)
  (:documentation "The motivation for this package, initially, was the lack of a distiguished *error-output* in LispWorks, 
that is, unlike allegro, access to file descriptor 2 was not available, and so programs that are intended 
to be pipes have no place to put an error, other than a file.

This package allows one to create a stream into a popup console window and bind *error-output* or pretty much 
anything else to it.

How to use:
First, call (create-console). By default, a read-only console is created, suitable for displaying errors 
  as your program prints them, (create-console :rw) will put up an editor buffer (read/write). 
  In either case, a console object is returned, which is needed for the following calls. Optionally, you can
  specify a second argument, which will be the title of the window.

\(console-stream console) returns a stream associated with a console, e.g., the thing to format to.

\(format-console console format-string &rest args) is format for a console.

\(read-line-console console &optional eof-error-p eof-value) is read-line for a console, but the console 
    argument is required."))


(in-package cl-lib-console)

;; right now, this all assumes lispworks.
#-lispworks
(error "Popup Console has not been ported outside the Lispworks/CAPI environment yet!")

;; definitions

(define-interface console ()
  ((console-type :initform :ro :reader console-type))
  )

(define-interface popup-console-r/o (console)
  ((console-type :initform :ro))
  (:panes
   (console collector-pane
            :enabled nil
            :accessor console-pane
            :visible-min-width '(:character 80)
            :visible-min-height '(:character 12)))
  (:default-initargs :title "Popup Console R/O"))

(define-interface popup-console-r/w (console)
  ((console-type :initform :rw))
  (:panes
   (console interactive-pane
            :enabled t
            :accessor console-pane
            :visible-min-width '(:character 80)
            :visible-min-height '(:character 24)))
  (:default-initargs :title "Popup Console R/W"))


;; main functions

(defun create-console (&optional (type :ro) (title (format nil "Popup Console ~A" type)))
  "Makes sure the environment is ready for popping up a window; two kinds of consoles are available, :ro and :rw, default :ro"
  ;; check environment
  (initialize-console-environment)
  ;; create window
  (let ((console (initialize-console-window type :title title)))
    (display console)
    console))

(defun destroy-console (console)
  "Console is no longer needed"
  (destroy console))

(defun console-stream (console)
  "Return the stream of a console"
  (case (console-type console)
    (:ro
     (collector-pane-stream (console-pane console))) ;; with luck, suitable for binding to *error-output*
    (:rw
     (interactive-pane-stream (console-pane console)))))

(defun format-console (console format-string &rest args)
  "Like format (oh if only that were a generic function!), but for consoles."
  (apply #'format (console-stream console) format-string args))

(defun read-line-console (console &optional (eof-error-p t) eof-value)
  "read-line for a console; really can just call read-line on the console stream!"
  (assert (eql (console-type console) :rw) () "Can't read from a display-only console")
  (read-line (console-stream console) eof-error-p eof-value))

;; helper functions

(defun initialize-console-environment ()
  (unless mp::*multiprocessing*
    (mp:initialize-multiprocessing))

  #-cocoa
  (x-utils:ensure-motif-libraries))

(defun initialize-console-window (type &key (min-width 200) (title nil title-p))
  (apply #'make-instance
         (case type
           (:ro
            'popup-console-r/o)
           (:rw
            'popup-console-r/w))
         :visible-min-width min-width (if title-p (list :title title))))

(pushnew :cl-lib-console *features*)
