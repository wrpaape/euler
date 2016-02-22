;;;; ********************************************************************************
;;;; *                                - set-3.lisp -                                *
;;;; *                                                                              *
;;;; * Houses solutions to problems 21-30.                                          *
;;;; ********************************************************************************

(in-package #:set-3)

;;; *********************************************************************************
;;; *                                 - problem-26 -                                *
;;; *                                                                               *
;;; *	A unit fraction contains 1 in the numerator. The decimal representation of    *
;;; * the unit fractions with denominators 2 to 10 are given:										    *
;;; *                                                                               *
;;; * 	1/2	= 	0.5                                                                 *
;;; * 	1/3	= 	0.(3)                                                               *
;;; * 	1/4	= 	0.25                                                                *
;;; * 	1/5	= 	0.2                                                                 *
;;; * 	1/6	= 	0.1(6)                                                              *
;;; * 	1/7	= 	0.(142857)                                                          *
;;; * 	1/8	= 	0.125                                                               *
;;; * 	1/9	= 	0.(1)                                                               *
;;; * 	1/10	= 	0.1                                                               *
;;; *                                                                               *
;;; *	Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be	*
;;; * seen that 1/7 has a 6-digit recurring cycle.																	*
;;; *                                                                               *
;;; *	Find the value of d < 1000 for which 1/d contains the longest recurring cycle *
;;; * in its decimal fraction part.																									*
;;; *********************************************************************************

(defun problem-26 ()
  "Solves Project Euler problem 26: 'Reciprocal Cycles'"

	(let ((max-period 6)  ;; tracks the running longest decimal digit period (init to 6 given in word problem)
        (solution-d 7)) ;; digit 'd' where '1 / d' produces the recurring cycle with period 'max-period'

    ;; loop over the remaining valid range of 'd'
		(loop for d from 11 below 1000
          do (let ((rmdr      1)        ;; tracks the running long division remainder, init to first dividend '1'
                   (rmdrs     (list 1)) ;; running list tracking history of remainders, init with 'rmdr'
                   (quot-digs 0))       ;; running count of quotient digits, init to 0

                 ;; process '1 / d' with standard long division
                 (loop named divide-and-carry

                       ;; multiply the last remainder 'rmdr' by 10 until 'd' can go into it, incrementing
                       ;; 'quot-digs' by the required the decimal offset (+1 for every * 10)
                       do (loop do (incf quot-digs)
                                   (setf rmdr (* rmdr 10))
                                while (< rmdr d))

                          ;; divide next dividend by 'd', update 'rmdr' to the integer remainder of this quotient
                          (setf rmdr (rem rmdr d))

                          ;; if 'd' divides evenly into '1'...no recurrence cycle, digit period = 0 and needn't
                          ;; be compared to current max → continue to next 'd'
                          (when (eq rmdr 0)                     
                                (return-from divide-and-carry))

                          ;; search list of previous remainders for an occurence of 'rmdr'
                          (loop for prev-rmdr in rmdrs

                                ;; if 'rdmr' is not unique, at least one full cycle of digits has been calculated
                                do (when (eql rmdr prev-rmdr)             
                                         (setf prev-rmdr 1)               ;; starting from the initial dividend...
                                         (loop until (eql prev-rmdr rmdr) ;; until the matching 'rmdr' is found...

                                               ;; restart long division, decrementing 'quot-digs' for each
                                               ;; leading digit (offset)
                                               do (loop do (decf quot-digs)
                                                           (setf prev-rmdr (* prev-rmdr 10))
                                                        while (< prev-rmdr d))

                                                  (setf prev-rmdr (rem prev-rmdr d)))

                                         ;; the final value of 'quot-digs' should now equal the digit period
                                         ;; of the recurring cycle for '1 / d'

                                         (when (> quot-digs max-period) ;; if period surpasses the current max...

                                               ;; update 'solution-d' and its digit period
                                               (setf max-period quot-digs)
                                               (setf solution-d  d))
                                         
                                         (return-from divide-and-carry))) ;; continue to next 'd'

                          ;; otherwise 'rmdr' is unique to 'rmdrs', push to head of list and continue dividing 
                          (push rmdr rmdrs))))

    ;; digits 11 through 999 have been processed, return digit 'd' with the longest digit period
    solution-d))


;;; *********************************************************************************
;;; *                                 - problem-27 -                                *
;;; *                                                                               *
;;; * Euler discovered the remarkable quadratic formula:                            *
;;; *                                                                               *
;;; * n² + n + 41                                                                   *
;;; *                                                                               *
;;; * It turns out that the formula will produce 40 primes for the consecutive      *
;;; * values n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is  *
;;; * divisible by 41, and certainly when n = 41, 41² + 41 + 41 is clearly          *
;;; * divisible by 41.                                                              *
;;; *                                                                               *
;;; * The incredible formula  n² − 79n + 1601 was discovered, which produces 80     *
;;; * primes for the consecutive-values n = 0 to 79. The product of the             *
;;; * coefficients, −79 and 1601, is −126479.                                       *
;;; *                                                                               *
;;; * Considering quadratics of the form:                                           *
;;; *                                                                               *
;;; * n² + an + b, where |a| < 1000 and |b| < 1000                                  *
;;; *                                                                               *
;;; * where |n| is the modulus/absolute value of n                                  *
;;; * e.g. |11| = 11 and |−4| = 4                                                   *
;;; *                                                                               *
;;; * Find the product of the coefficients, a and b, for the quadratic expression   *
;;; * that produces the maximum number of primes for consecutive values of n,       *
;;; * starting with n = 0.                                                          *
;;; *********************************************************************************

(defun problem-27 ()
  "Solves Project Euler problem 27: 'Quadratic Primes'"

  (let* ((b-range (prime-sieve 999))    ;; generate valid range for b 
         (primes  (copy-list b-range))) ;; initialize set of prime numbers

    ; (append-unique-p primes 1007 #'(lambda (x y) (eql (rem y x) 0)))))
    
    (let ((max-cons-primes  0)
          (solution-product 0)
          (max-prime-tested 999))

      (loop for b in b-range
            do (loop for a from -999 to 999
                     do (loop named count-cons-primes
                              for n from 1
                              do (let ((quad (+ (* n (+ n a)) b)))

                                   ; (format t "a:    ~D~%" a)
                                   ; (format t "b:    ~D~%" b)
                                   ; (format t "n:    ~D~%" n)
                                   ; (format t "quad: ~D~%" quad)
                                   ; (format t "max:  ~D~%" max-cons-primes)
                                   ; (finish-output nil)
                                   ; (sleep 1)

                                   ;; unless 'quad' is prime...
                                   (unless (and (> quad 1) ;; negative numbers, 0, and 1 aren't prime

                                                         ;; if 'quad' is outside the range of known primes...
                                                (or (and (> quad max-prime-tested)


                                                         ;; calculate primality and update 'primes'
                                                         (update-primes-p primes
                                                                     max-prime-tested
                                                                     quad)

                                                         ;; if 'quad' is prime, update 'max-prime-tested'
                                                         (setf max-prime-tested quad))

                                                    ;; otherwise check 'primes' for 'quad'
                                                    (loop for prime in primes
                                                          until (> prime quad)
                                                          do (when (eql prime quad)
                                                                   (return t)))))


                                           ;; if 'n' exceeds previous 'max-cons-primes', update solution vars
                                           (when (> n max-cons-primes)
                                                 (setf max-cons-primes n)
                                                 (setf solution-product (* a b)))

                                           ;; continue to next 'a'
                                           (return-from count-cons-primes))))))
    
      (format t "max:    ~D~%" max-cons-primes)
      ; (format t "primes: ~S~%" primes)
      solution-product)))


;;; *********************************************************************************
;;; *                              - update-primes-p -                              *
;;; *                                                                               *
;;; * Extends existing comprehensive list of prime numbers starting at 2, 'primes', *
;;; * from its last checked maximum, input 'next', to 'next-max'. If 'next-max' is  *
;;; * prime, the updated version of 'primes'.  Otherwise nil is returned, however   *
;;; * 'primes' will still be modified if any primes exist between 'next' and        *
;;; * 'next-max'.                                                                   *
;;; *********************************************************************************

(defun update-primes-p (primes next next-max)
  (loop named extend-primes
           ;; set list-traversal vars 'prime-tail' and 'rem-tail'
        do (let ((prime-tail primes)
                 (rem-tail   (cdr primes)))

             (incf next) ;; starting from one more than last max...

             ;; check primality of 'next'
             (loop named test-next

                      ;; 'next' is not prime if evenly divisible by a known prime
                   do (when (eql (rem next (car prime-tail)) 0)

                            ;; if 'next-max' has been reached and isnt prime...
                            (when (eql next next-max)

                                  ;; return nil to caller
                                  (return-from extend-primes nil))

                            ;; else continue to the next 'next'
                            (return-from test-next))
                      
                      ;; if last known prime has been reached, extend 'primes'
                      (unless rem-tail

                              ;; set 'next' to last item of list
                              (rplacd prime-tail (cons next nil))

                              ;; if 'next-max' has been reached and is prime...
                              (when (eq next next-max)
                                    
                                    ;; return modified 'primes' to caller
                                    (return-from extend-primes primes))

                              ;; else continue to the next 'next'
                              (return-from test-next))

                      ;; advance list-traversal vars
                      (setf prime-tail rem-tail)
                      (setf rem-tail (cdr rem-tail))))))
