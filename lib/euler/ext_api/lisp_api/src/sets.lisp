(in-package #:sets)

(defun prime-sieve (limit)
  (let  ((n-pool (loop for n from 2 to limit
                       collect n)))

    (loop for prime in n-pool
          collect prime
          do (setf n-pool (delete-if #'(lambda (n)
                                         (eql (rem n prime) 0))
                                     (cdr n-pool))))))

