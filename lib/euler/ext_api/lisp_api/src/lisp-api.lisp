;;;; *********************************************************************************
;;;; *                               - lisp-api.lisp -                               *
;;;; *                                                                               *
;;;; * Houses the main package of the lisp-api system, responsible for communication *
;;;; * between Elixir Mix project 'euler' and problems solved in Common Lisp.        *
;;;; *********************************************************************************

(in-package #:lisp-api)

;;; **********************************************************************************
;;; *                                    - main -                                    *
;;; *                                                                                *
;;; * External client interface, responsible for dispatch of input problem set and   *
;;; * problem number to the appropriate problem function.                            *
;;; **********************************************************************************

(defun main (argv)
  (handler-case
      (let ((set-num nil)
            (set-pkg nil)
            (prob-num nil)
            (prob-fun nil))

        (pop argv) ; remove cmd from argv

        (setf set-num (pop argv))

        (unless set-num (error 'set-num-not-provided-error))

        (setf set-pkg (find-package
                          (concatenate 'string  "SET-" set-num)))

        (unless set-pkg (error 'set-does-not-exist-error
                                 :set-num set-num))

        (setf prob-num (pop argv))

        (unless prob-num (error 'prob-num-not-provided-error))

        (setf prob-fun (find-symbol
                           (concatenate 'string  "PROBLEM-" prob-num)
                           set-pkg))

        (unless prob-fun (error 'prob-not-found-error
                                  :prob-num prob-num
                                  :set-num  set-num))

        (princ (funcall prob-fun)))

    (condition (arg-error)
                 (princ arg-error *error-output*)
                 (exit 1))))


;;; **********************************************************************************
;;; *                                    - exit -                                    *
;;; *                                                                                *
;;; *
;;; **********************************************************************************

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


;;; **********************************************************************************
;;; *                           EXIT CONDTION DEFINITIONS                            *
;;; **********************************************************************************

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

