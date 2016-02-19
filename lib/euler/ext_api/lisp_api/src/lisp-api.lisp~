(in-package #:lisp-api)

(define-condition set-num-not-provided-error (error) ()
  (:report "Problem set number not provided."))


(define-condition prob-num-not-provided-error (error) ()
  (:report "Problem number not provided."))


(define-condition set-does-not-exist-error
                  (error)
                  ((set-num :initarg :set-num
                            :reader set-does-not-exist-set-num))
  (:report (lambda (condition stream)
             (format stream "Problem set number \"~A\" does not exist."
                     (set-does-not-exist-set-num condition)))))


(define-condition prob-not-found-error
                  (error)
                  ((prob-num :initarg :prob-num
                             :reader  prob-not-found-prob-num)
                   (set-num  :initarg :set-num
                             :reader  prob-not-found-set-num))
  (:report (lambda (condition stream)
             (format stream "Problem number \"~A\" not found in problem set \"~A\"."
                     (prob-not-found-prob-num condition)
                     (prob-not-found-set-num  condition)))))

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


(defvar *set-num* nil "dox")
(defvar *set-pkg* nil "dox")
(defvar *prob-num* nil "docs")
(defvar *prob-fun* nil "docs")

(defun main (argv)
  (handler-case
      (progn
        (pop argv)

        (setf *set-num* (pop argv))

        (unless *set-num* (error 'set-num-not-provided-error))

        (setf *set-pkg* (find-package
                          (concatenate 'string  "SET-" *set-num*)))

        (unless *set-pkg* (error 'set-does-not-exist-error
                                 :set-num *set-num*))

        (setf *prob-num* (pop argv))

        (unless *prob-num* (error 'prob-num-not-provided-error))

        (setf *prob-fun* (find-symbol
                           (concatenate 'string  "PROBLEM-" *prob-num*)
                           *set-pkg*))

        (unless *prob-fun* (error 'prob-not-found-error
                                  :prob-num *prob-num*
                                  :set-num  *set-num*))

        (princ (funcall *prob-fun*)))

    (condition () (exit 1))))
