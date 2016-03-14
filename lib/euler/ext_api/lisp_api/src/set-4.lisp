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
  ;; way: with pennies
  ;; when 'rem-total' reaches zero, 200p has been counted evenly
  ;;
  ;; In either of these cases, a new unique combination of coins totaling 200p
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
;;; * sides, {a, b, c}, there are exactly three solutions for p = 120.           *
;;; *                                                                          *
;;; * {20, 48, 52}, {24, 45, 51}, {30, 40, 50}                                       *
;;; *                                                                          *
;;; * For which value of p ≤ 1000, is the number of solutions maximised?       *
;;; ****************************************************************************


(defun problem-39 ()
  "Solves Project Euler problem 39: 'Integer Right Triangles'"

  ;; a² + b² = c²
  ;;
  ;; a + b + c = p
  ;;
  ;; p² - 2(a + b)p + 2ab = 0
  ;;                _______
  ;; p = (a + b) ± √a² + b²
  ;;             →(   c    )←
  ;;
  ;; '-' not valid bc p = a + b + c
  ;;                _______
  ;; p = (a + b) + √a² + b²
  ;;
  ;; c = p²/2(p - b) - b
  ;;
  ;; => bc a, b, and c must be integers, p² must be even
  ;; => bc squares of odd numbers must be odd, p must be even
  ;;
  ;; growth of c is dominated by p



  (let ((max-sols 0)
        (sol-p    0))

    (loop for p from 2 to 1000 by 2
          do (let ((half-p-sq  (ash (* p p) -1))
                   (sols-count 0))

               (loop named b-sweep
                     for b from 1
                     do (let ((sq-term (/ half-p-sq (- p b))))

                          (when (integerp sq-term)

                                (if (> sq-term
                                       (ash b 1))

                                    (incf sols-count)

                                    (progn (when (> sols-count max-sols)
                                                 (setf max-sols sols-count)
                                                 (setf sol-p    p))

                                           (return-from b-sweep))))))))

  sol-p))
