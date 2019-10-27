(in-package cl-lib)

;; load this AFTER :cl-lib-essentials

;; when updating the library (new package) put this line before the CL-LIB-FNS one (so the time stamp is updated).
(cl-lib-essentials:version-reporter "CL-LIB" 5 17
                                    ";; Time-stamp: <2019-10-26 20:29:39 Bradford Miller(on Aragorn.local)>" 
                                    ";; 5am")


;; when updating the functions put this line before the CL-LIB one (so the time stamp is updated).
(cl-lib-essentials:version-reporter "CL-LIB-FNS" 5 17
                                    ";; Time-stamp: <2019-10-26 20:29:31 Bradford Miller(on Aragorn.local)>" 
                                    ";; 5am")

(pushnew :cl-lib *features*)
(pushnew :cl-lib-5 *features*)

