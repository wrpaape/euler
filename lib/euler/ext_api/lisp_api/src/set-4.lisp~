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
                '(200 100 50 20 10 5 2 1)
                0))


(defun add-change (rem-total change num-combs)
  (when (zerop rem-total)
        (return-from add-change (1+ num-combs)))

  (loop while (> (car change) rem-total)
        do (setf change (cdr change)))
  
  
  (reduce #'(lambda (num-combs coin)
              (add-change (- rem-total coin)
                          change,
                          num-combs))
           change
           :inital-value num-combs))
