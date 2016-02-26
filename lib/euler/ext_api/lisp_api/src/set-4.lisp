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