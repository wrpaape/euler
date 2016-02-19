(in-package #:lisp-api)

(defvar *problem-set-package* nil "dox")
(defvar *problem-function*    nil "docs")

(defun main (argv))

(defun exit-on-error (msg)
  (princ msg *error-output*))
