;;;; ********************************************************************************
;;;; *                                  sets.lisp                                   *
;;;; *                                                                              *
;;;; * Exports common functionality "used" by 'set-x' packages                      *
;;;; ********************************************************************************

(in-package #:sets)

;;; *********************************************************************************
;;; *                              - append-unique-p -                              *
;;; *                                                                               *
;;; * Recieves a list of unique elements, 'set-list', an untested element,          *
;;; * 'new-el', and a function to test uniqueness, 'exclude-test'. If 'new-el' is   *
;;; * deemed unique, it is appended to 'set-list'.                                  *
;;; *                                                                               *
;;; * Existing member of 'set-list', 'set-el', and untested element 'new-el' are    *
;;; * applied to 'exclude-test' as first and second parameters respectively, which  *
;;; * should return a truthy value if 'new-el' is not unique and 'nil' in all other *
;;; * cases.                                                                        *
;;; *********************************************************************************

(defun append-unique-p (set-list new-el exclude-test)
  (let ((set-tail set-list))

    (loop named test-membership

          do (let ((set-el (car set-tail)))

               (when (funcall exclude-test set-el new-el)
                     (return-from test-membership nil))

               (let ((rem-tail (cdr set-tail)))

                 (unless rem-tail
                         (rplacd set-tail (cons new-el nil))
                         (return-from test-membership set-list))

                   (setf set-tail rem-tail))))))


;;; *********************************************************************************
;;; *                                - prime-sieve -                                *
;;; *                                                                               *
;;; * Generates an ascending list of prime numbers ranging (inclusive) from '2' up  *
;;; * to a potential maximum of 'limit'.                                            *
;;; *********************************************************************************

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
