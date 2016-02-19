(defpackage #:sets
    (:use #:common-lisp)
    (:export #:greet))


(defpackage #:set-3
    (:use #:common-lisp
          #:sets)
    (:export #:problem-26))


(defpackage #:lisp-api
    (:use #:common-lisp
          #:asdf)
    (:export #:main))

