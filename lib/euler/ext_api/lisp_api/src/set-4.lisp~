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


(defun add-change (rem-total change num-perms)
  (format t "~%rem-total:  ~D~%" rem-total)
  (format t "num-perms:  ~D~%" num-perms)
  (format t "change:     ~S~%" change)
  (force-output nil)
  ; (sleep 0.01)

  (when (or (zerop rem-total)
            (null change))
        (return-from add-change (1+ num-perms)))


  (let ((next-coin (pop change)))
    
    (loop do (setf num-perms (add-change rem-total
                                         change
                                         num-perms))
             (decf rem-total next-coin)

          until (minusp rem-total)))

    num-perms)

  
  ; (format t "rem-total:  ~D~%" rem-total)
  ; (format t "numb-combs: ~D~%" num-perms)
  ; (format t "change:     ~S~%" change)
  ; (format t "RETURNED!~%")
  ; (format t "next: ~D~%" next)
  ; (force-output nil)
  
  ; (reduce #'(lambda (num-perms coin)
  ;             (add-change (- rem-total coin)
  ;                         change
  ;                         num-perms))
  ;         change
  ;         :initial-value num-perms))
