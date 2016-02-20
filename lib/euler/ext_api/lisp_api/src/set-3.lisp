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

	(let ((base-dividend 100)
        (max-rec-cyc   6)
        (solution-d    7))

    ;; loop over the remaining valid range of 'd'
		(loop named traverse-search-interval
          for d from 11 below 1000
          do (when (> d base-dividend)
                   (setf base-dividend (* base-dividend 10)))

             (let ((rmdr      (rem base-dividend d))
                   (rmdrs     '())
                   (last-rmdr nil))

               (loop named divide-and-carry
                     until (eql rmdr 0)
                     for len-rmdrs from 0
                     do (let ((tail-rmdrs rmdrs))

                          (loop named traverse-remainders
                                for rec-cyc downfrom len-rmdrs
                                while tail-rmdrs
                                do (when (eql rmdr (car tail-rmdrs))

                                         (loop named count-rec-zeros
                                               for rec-zeros from 0
                                               do (setf last-rmdr (* last-rmdr 10))
                                                  (unless (< last-rmdr base-dividend)
                                                          (when (eql last-rmdr base-dividend)
                                                                (incf rec-cyc rec-zeros))

                                                          (return-from count-rec-zeros)))

                                         (when (> rec-cyc max-rec-cyc)
                                               (setf max-rec-cyc rec-cyc)
                                               (setf solution-d  d))

                                         (return-from divide-and-carry)))

                                   (setf tail-rmdrs (cdr tail-rmdrs)))

                        (setf tail-rmdrs (cons rmdr nil))

                        (setf last-rmdr rmdr)

                        (loop do (setf last-rmdr (* last-rmdr 10))
                                  while (< last-rmdr d))

                        (setf rmdr (rem last-rmdr d)))))

    solution-d))

                        ; (loop for prev-rmdr in prev-rmdrs
                        ;       for rec-cyc from 1
                        ;       do (when (eql rmdr prev-rmdr)

                        ;                (let ((mult-prev-rmdr (* (car prev-rmdrs) 10))
                        ;                      (trailing-zeros 0))

                        ;                  (loop while (< mult-prev-rmdr base-dividend)
                        ;                        do (setf mult-prev-rmdr (* mult-prev-rmdr 10))
                        ;                           (incf trailing-zeros))

                        ;                  (when (eql mult-prev-rmdr base-dividend)
                        ;                        (incf rec-cyc trailing-zeros)))

                        ;                (when (> rec-cyc max-rec-cyc)
                        ;                      (setf max-rec-cyc     rec-cyc)
                        ;                      (setf solution-d d))

                                       ; (return-from divide-and-carry))))))

