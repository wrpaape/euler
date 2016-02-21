(defpackage #:sets
    (:use #:common-lisp)
    (:export #:prime-sieve))


(defpackage #:set-3
    (:use #:common-lisp
          #:sets)
    (:export #:problem-26
             #:problem-27))


(defpackage #:lisp-api
    (:use #:common-lisp
          #:asdf)
    (:export #:main))

