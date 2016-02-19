(in-package #:lisp-api)


(define-condition set-num-not-provided-error
                  (error)
  (:report (lambda (stream)
             (princ "Problem set number not provided." stream))))


(define-condition prob-num-not-provided-error
                  (error)
  (:report (lambda (stream)
             (princ "Problem number not provided." stream))))


(define-condition set-does-not-exist-error
                  (error)
                  ((set-num :initarg :set-num
                            :reader set-does-not-exist-set-num))
  (:report (lambda (condition stream)
             (format stream "Problem set number \"~A\" does not exist."
                     (set-does-not-exist-set-num condition)))))


(define-condition problem-not-found-error
                  (error)
                  ((prob-num :initarg :prob-num
                             :reader  problem-not-found-prob-num)
                   (set-pkg  :initarg :set-pkg
                             :reader  problem-not-found-set-pkg))
  (:report (lambda (condition stream)
             (format stream "Problem number \"~A\" not found in set ~A."
                     (problem-not-found-prob-num condition)
                     (problem-not-found-set-pkg  condition)))))

(defvar *set-str* nil "dox")
(defvar *set-pkg* nil "dox")
(defvar *prob-str* nil "docs")
(defvar *prob-fun* nil "docs")

(defun main (argv)
  (handler-case
    (progn
      (pop argv)
      (if argv
        (setf *set-str* (pop argv))
        (error 'set-num-not-provided-error)))
      
      (condition () (exit 1))))

(defun exit (&optional code)
      "Programmatically exit the lisp image."
      ;; This group from "clocc-port/ext.lisp"
      #+sbcl (sb-ext:exit :code code)
      #+allegro (excl:exit code)
      #+clisp (#+lisp=cl ext:quit #-lisp=cl lisp:quit code)
      #+cmu (ext:quit code)
      #+cormanlisp (win32:exitprocess code)
      #+gcl (lisp:bye code)                     ; XXX Or is it LISP::QUIT?
      #+lispworks (lw:quit :status code)
      #+lucid (lcl:quit code)
      ;; This group from Maxima
      #+kcl (lisp::bye)                         ; XXX Does this take an arg?
      #+scl (ext:quit code)                     ; XXX Pretty sure this *does*.
      #+(or openmcl mcl) (ccl::quit)
      #+abcl (cl-user::quit)
      #+ecl (si:quit)
      ;; This group from <hebi...@math.uni.wroc.pl>
      #+poplog (poplog::bye)                    ; XXX Does this take an arg?
      #-(or sbcl allegro clisp cmu cormanlisp gcl
            lispworks lucid kcl scl openmcl mcl abcl ecl)
      (error 'not-implemented :proc (list 'exit code)))

