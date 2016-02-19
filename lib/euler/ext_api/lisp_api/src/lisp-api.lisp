;;;; ********************************************************************************
;;;; *                               - lisp-api.lisp -                              *
;;;; *                                                                              *
;;;; * Houses the main package of the lisp-api system, responsible for              *
;;;; * communication between Elixir Mix project 'euler' and problems solved in      *
;;;; * Common Lisp.                                                                 *
;;;; ********************************************************************************

(in-package #:lisp-api)

;;; *********************************************************************************
;;; *                                    - main -                                   *
;;; *                                                                               *
;;; * External client interface, responsible for dispatch of input problem set and  *
;;; * problem number to the appropriate problem function.                           *
;;; *********************************************************************************

(defun main (argv)
  "Dispatch appropriate problem function according to command line arguments ~
  'set-num' and 'prob-num'"

  ;; wrap main forms in a condition handler
  (handler-case
    (let* ((prob-fun      (parse-problem-function argv))      ;; parse cmd line argv
           (res-cons-cell (time-problem-function  prob-fun))) ;; time prob-func

      ;; delimit the results and time elapsed with a newline and print to stdout
      (format t
              "~D~%~S"
              (car res-cons-cell)
              (cdr res-cons-cell)))

    ;; if error signal → report condition to stderr and with status code '1'
    (condition (arg-error)
                 (princ arg-error *error-output*)
                 (exit 1))))


;;; *********************************************************************************
;;; *                           - time-problem-function -                           *
;;; *                                                                               *
;;; * Times a call to the input problem function 'prob-fun', returning a cons cell  *
;;; * of the form:                                                                  *
;;; *                            (result . time-elapsed)                            *
;;; *                                                                               *
;;; * where 'time-elapsed' is the time taken for 'prob-fun' to evaluate 'result',   *
;;; * rounded to the nearest μs.                                                    *
;;; *********************************************************************************

(defun time-problem-function (prob-fun)
  "Times a call to the input problem function 'prob-fun', returning a cons cell of ~
  the form:\n  (time-elapsed . result)\n where 'time-elapsed' is the time taken for ~
  'prob-fun' to evaluate 'result', rounded to the nearest μs."

  (let* ((time-start   (get-internal-real-time))
         (result       (funcall prob-fun))
         (time-stop    (get-internal-real-time))
         (time-elapsed (round (* (/ (- time-stop
                                       time-start)
                                    INTERNAL-TIME-UNITS-PER-SECOND)
                                 1e6))))

    (list* result time-elapsed)))

;;; *********************************************************************************
;;; *                           - parse-problem-function -                          *
;;; *                                                                               *
;;; * Parses problem function symbol 'prob-fun' from command line input 'argv'.     *
;;; *********************************************************************************

(defun parse-problem-function (argv)
  "Parses problem function symbol from command line input 'argv'."

  (let ((set-num  nil)  ; string referencing package 'set-pkg'       ("3")
        (set-pkg  nil)  ; package housing target problem function    ('SET-3)
        (prob-num nil)  ; string referencing problem function        ("26")
        (prob-fun nil)) ; function symbol of target problem function ('PROBLEM-26)

    ;; remove cmd string from head of 'argv'
    (pop argv)

    ;; read in next arg
    (setf set-num
          (pop argv))

    ;; if no more args, signal error condition
    (unless set-num (error 'set-num-not-provided-error))

    ;; find package referenced by 'set-num'
    (setf set-pkg
          (find-package (concatenate 'string  "SET-" set-num)))

    ;; if can't be found, signal error condition
    (unless set-pkg (error 'set-does-not-exist-error
                           :set-num set-num))

    ;; read in next arg
    (setf prob-num
          (pop argv))

    ;; if no more args, signal error condition
    (unless prob-num (error 'prob-num-not-provided-error))

    ;; find function referenced by 'prob-num' in package 'set-pkg'
    (setf prob-fun
          (find-symbol (concatenate 'string  "PROBLEM-" prob-num)
                       set-pkg))

    ;; if can't be found, signal error condition
    (unless prob-fun (error 'prob-not-found-error
                              :prob-num prob-num
                              :set-num  set-num))
  
    ;; return target problem function
    prob-fun))


;;; *********************************************************************************
;;; *                                    - exit -                                   *
;;; *                                                                               *
;;; * Responsible for "programmatically exit"ing the list image.  Taken from the    *
;;; * common lisp wiki:                                                             *
;;; *                                                                               *
;;; *                      http://www.cliki.net/portable%20exit                     *
;;; *********************************************************************************

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


;;; *********************************************************************************
;;; *                           EXIT CONDITION DEFINITIONS                          *
;;; *********************************************************************************

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
