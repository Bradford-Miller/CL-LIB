(in-package cl-lib)

;; load this AFTER :cl-lib-essentials

;; when updating the library (new package) put this line before the CL-LIB-FNS one (so the time stamp is updated).
(cl-lib-essentials:version-reporter "CL-LIB" 5 17
                                    ";; Time-stamp: <2019-10-19 13:43:19 Bradford Miller(on Aragorn.local)>" 
                                    ";; 5am")


;; when updating the functions put this line before the CL-LIB one (so the time stamp is updated).
(cl-lib-essentials:version-reporter "CL-LIB-FNS" 5 16
                                    ";; Time-stamp: <2019-02-18 16:01:47 Bradford Miller(on Aragorn.local)>"
                                    ";; 5am")

(pushnew :cl-lib *features*)
(pushnew :cl-lib-5 *features*)

