;;;; ********************************************************************************
;;;; *                                  sets.lisp                                   *
;;;; *                                                                              *
;;;; * Exports common functionality "used" by 'set-x' packages                      *
;;;; ********************************************************************************

(in-package #:sets)

;;; *********************************************************************************
;;; *                                - prime-sieve -                                *
;;; *                                                                               *
;;; * Generates an ascending list of prime numbers ranging (inclusive) from '2' up  *
;;; * to a potential maximum of 'limit'.                                            *
;;;; ********************************************************************************

(defun prime-sieve (limit)

  ;; initialize pool of potential prime numbers
  (let  ((n-pool (loop for n from 2 to limit
                       collect n)))

    (loop for prime in n-pool ;; while values remain in 'n-pool'...
          collect prime       ;; the next value must be prime

          ;; for the remaining values of 'n-pool', 'n', delete 'n' if it is evenly
          ;; divisible by 'prime'
          do (setf n-pool (delete-if #'(lambda (n)
                                         (eql (rem n prime) 0))
                                     (cdr n-pool)))))) ;; excluding 'prime'


(defun append-unless (set-list new-el exclude-test)
  (let ((set-tail set-list))

    (loop named test-membership

          do (let ((set-el   (car set-tail))
                   (rem-tail (cdr set-tail)))

               (when (funcall exclude-test set-el new-el)
                     (return-from test-membership nil))

               (unless rem-tail
                       (rplacd set-tail (cons new-el nil))
                       (return-from test-membership set-list))

               (setf set-tail rem-tail)))))
