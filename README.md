#CL-LIB: A library of common functions and packages for Common Lisp

##### README.md Time-stamp: <2019-04-20 12:21:37 Bradford Miller(on Aragorn.local)>

This version is an update of one distributed under the CMU AI Library,
the URCS ftp, and my own page at
<https://sites.google.com/site/bradfordwmiller/software>. It was originally distributed as part of the Association of Lisp Users web site when hosted at URCS.

NB: LGPL license (see COPYING.LIB-3.0). 

### Recent Changes:

This new version for github has **asdf** (.asd) files, originally added for
compatability with **SBCL** (used for roslisp).  Note that the older
defsystem files may be found in the archive directory.

In addition, a minimal set of tests have been added as a quick check for porting purposes.

Note that certain lisp-implementation specific directories (as mentioned in the legacy README file) have been moved under the archive subdirectory and are no longer maintained as I no longer have access to current versions of these implementations (i.e. allegro and MCL).

## Usage

To use, copy to your ~/common-lisp/ directory as **asdf** will look
there by default for systems to load. (You may also put it anywhere
you want if you configure asdf to look there).

Before use do (from a listener):

* (asdf:compile-system :cl-lib)
* (asdf:load-system :cl-lib)

Note that this will load a core version of the libraries. You can load
individual bits (see the .asd files) such as locatives or resources
(both primarily created for compatibility with the Lisp Machines but
useful in their own right) or everything with system **:cl-lib-all** .

NB: you only need to compile once, after that loading (e.g. in your site-init, initialization file, etc.) is all that is needed until/unless you update to a newer version.

## Caveats

While this release includes tests defined under the Five AM framework,
that isn't itself included (and is not necessary to run CL-LIB). You
can get it here: <https://github.com/sionescu/fiveam>.  The current
FiveAM tests are pretty basic - just what is needed to assure that
functions run in a particular lisp environment, rather than
comprehensive tests for correctness.

Please report bugs. Currently I have access to Lispworks (7.0) on Mac
OS, and the current version of sbcl (tested both on Mac OS and
Ubuntu/ARM), but reports from other environments (particularly with
patches) are welcome!


