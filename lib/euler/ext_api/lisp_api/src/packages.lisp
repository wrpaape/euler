(defpackage :sets
    (:use :common-lisp)
    (:export :sets
             :greet))


(defpackage :set-3
    (:use :common-lisp
          :sets)
    (:export :set-3
             :problem-26))


(defpackage :lisp-api
    (:use :common-lisp
          :asdf)
    (:export :main))

