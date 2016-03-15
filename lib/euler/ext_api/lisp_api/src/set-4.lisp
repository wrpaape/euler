;;;; ***************************************************************************
;;;; *                             - set-4.lisp -                              *
;;;; *                                                                         *
;;;; * Houses solutions to problems 31-40.                                     *
;;;; ***************************************************************************

(in-package #:set-4)

;;; ****************************************************************************
;;; *                              - problem-31 -                              *
;;; *                                                                          *
;;; * In England the currency is made up of pound, £, and pence, p, and there  *
;;; * are eight coins in general circulation:                                  *
;;; *                                                                          *
;;; * 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).                      *
;;; *                                                                          *
;;; * It is possible to make £2 in the following way:                          *
;;; *                                                                          *
;;; * 1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p                                *
;;; *                                                                          *
;;; * How many different ways can £2 be made using any number of coins?        *
;;; ****************************************************************************


(defun problem-31 ()
  "Solves Project Euler problem 31: 'Coin Sums'"

    ;; starting with 200p remaining and 0 known change-making combinations,
    ;; for coins larger than 1p, explore branches where a coin is counted
    ;; zero times through the maximum times it can be divided evenly into
    ;; the remaining total
    (add-change 200
                '(200 100 50 20 10 5 2)
                0))


(defun add-change (rem-total change num-combs)
  ;; when no 'change' remains, the remaining total can only be balanced in one
  ;; way (with pennies)
  ;;
  ;; when 'rem-total' reaches zero, 200p has been counted evenly
  ;;
  ;; in either of these cases, a new unique combination of coins totaling 200p
  ;; has been discovered, increment the solution accumulator, 'num-combs' and
  ;; return immediately
  (when (or (null change)
            (zerop rem-total))
        (return-from add-change (1+ num-combs)))


  ;; otherwise child nodes exist for the current 'change' branch
  ;; pop the next largest coin from 'change' stack
  (let ((next-coin (pop change)))
    
    ;; starting with the case where 'next-coin' is skipped,
    ;; update 'num-combs' for each child node
    (loop do (setf num-combs (add-change rem-total
                                         change
                                         num-combs))

             ;; decrement 'rem-total' for next iteration's case
             ;; when 'next-coin' is counted an additional time
             (decf rem-total next-coin)

          ;; if 'rem-total' is negative all children nodes of 'next-coin'
          ;; have been accounted for, return from 'add-change'
          until (minusp rem-total)))

    ;; pass accumulator one level up call stack
    num-combs)

;;; ****************************************************************************
;;; *                              - problem-39 -                              *
;;; *                                                                          *
;;; * If p is the perimeter of a right angle triangle with integral length     *
;;; * sides, {a, b, c}, there are exactly three solutions for p = 120.         *
;;; *                                                                          *
;;; * {20, 48, 52}, {24, 45, 51}, {30, 40, 50}                                 *
;;; *                                                                          *
;;; * For which value of p ≤ 1000, is the number of solutions maximised?       *
;;; ****************************************************************************


(defun problem-39 ()
  "Solves Project Euler problem 39: 'Integer Right Triangles'"

  ;; triangle solution conditions:
  ;;
  ;;   (1) right triangle with sides a, b, c where:
  ;;  
  ;;         => c > a, b
  ;;  
  ;;         => a, b, and c are positive integers
  ;;  
  ;;            (*) a, b >= 1
  ;;  
  ;;            (*) c >= 2
  ;;
  ;;   (2) a + b + c = p  (triangle)
  ;;  
  ;;        (*) p >= 4 (from naive min values for a, b, c)
  ;;
  ;;   (3) a² + b² = c²   (right triangle)
  ;;
  ;;   from the above eqs, can subsitute 'a' and simplify:
  ;;
  ;;   (4) c = p²/2(p - b) - b
  ;;
  ;;         → 'sq-term' ←
  ;;
  ;;         (*) 'sq-term' must be integer
  ;;
  ;;         => p² must be even (divide evenly by 2)
  ;;
  ;;         (*) p must be even (squares of odd numbers are odd)
  ;;
  ;;         => 'sq-term' must be greater than 2b (c is greater than b)
  ;;
  ;;         (*) when 'sq-term' is less than or equal to 2b,
  ;;             b has met or surpassed c
  ;;
  ;; sweep method:
  ;;
  ;;   for all even values of 'p' where
  ;;
  ;;     4 <= p <= 1000
  ;;
  ;;   if b in range 1, 2, ... B where
  ;;   
  ;;     p²/2(p - B) < 2B
  ;;
  ;;   produces an integer 'sq-term', a solution fitting problem
  ;;   conditions has been found
  ;;
  ;;   'max-sol-p' corresponds to p that produces the greatest
  ;;   number of these solutions, 'max-count'

  (let ((max-count 0)
        (max-sol-p 0))

    (loop for p from 4 to 1000 by 2
          do (let ((half-p-sq  (ash (* p p) -1))
                   (sols-count 0))

               (loop named b-sweep
                     for b from 1
                     do (let ((sq-term (/ half-p-sq (- p b))))

                          (when (integerp sq-term)

                                (if (> sq-term (ash b 1))

                                    (incf sols-count)

                                    (progn (when (> sols-count max-count)
                                                 (setf max-count sols-count)
                                                 (setf max-sol-p p))

                                           (return-from b-sweep))))))))

  max-sol-p))
