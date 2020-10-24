# CL-LIB: A library of common functions and packages for Common Lisp

##### README.md Time-stamp: <2020-01-03 17:42:27 Bradford Miller(on Aragorn.local)>

This version is a significant update of one distributed under the CMU
AI Library, the URCS ftp, and my own page at
<https://sites.google.com/site/bradfordwmiller/software>. That one was
originally distributed as part of the Association of Lisp Users web
site when hosted at URCS, in the days of yore before github, Google,
or other common mechanisms for distribution of source code.

NB: LGPL license (see COPYING.LIB-3.0). The intent is that you can use
it freely for your own systems even if it is commercial without the
insidious copyleft applying to your code, but if you modify the
library you should share your changes or fixes back to the community.

### Recent Changes:

This new version for github has **asdf** (.asd) files, originally
added for compatability with **SBCL** (e.g., used for roslisp).  Note
that the older defsystem files may be found in the archive directory,
but other changes may have rendered them obsolete.

In addition, a minimal set of tests have been added as a quick check
for porting purposes. These have not (yet) been extended to really
check the full functionality of the individual packages, but are just
a sanity check that nothing is severely broken.

Note that certain lisp-implementation specific directories (as
mentioned in the legacy README file) have been moved under the archive
subdirectory and are no longer maintained as I no longer have access
to current versions of these implementations (i.e. allegro and MCL,
the latter no longer compatible with current machines).

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

NB: you only need to compile once, after that loading (e.g. in your
site-init, initialization file, etc.) is all that is needed
until/unless you update to a newer version (either of this library or
your underlying lisp implementation).

## Caveats

While this release includes tests defined under the Five AM framework,
that isn't itself included (and is not necessary to run CL-LIB). You
can get it here: <https://github.com/sionescu/fiveam>.  The current
FiveAM tests are pretty basic - just what is needed to assure that
functions run in a particular lisp environment, rather than
comprehensive tests for correctness.

Please report bugs. Currently I maintain compatibility with Lispworks
(7.0) on Mac OS, and the current version of sbcl (which I at least
intend to test both on Mac OS and Ubuntu/ARM), as these are the lisp
implementations I use regularly, but reports from other environments
(particularly with patches) are welcome!


