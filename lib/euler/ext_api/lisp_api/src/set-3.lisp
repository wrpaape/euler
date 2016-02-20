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

	(let ((max-rec-cyc   6)
        (solution-d    7))

    ;; loop over the remaining valid range of 'd'
		(loop named traverse-search-interval
          for d from 1 below 1000
          do (let ((rmdr      1)
                   (rmdrs     (list 1))
                   (rec-cyc 0))

                 (loop named divide-and-carry
                       do (loop do (incf rec-cyc)
                                   (setf rmdr (* rmdr 10))
                                while (< rmdr d))

                          (setf rmdr (rem rmdr d))

                          (when (eq rmdr 0)
                                (return-from divide-and-carry))


                          (loop named traverse-remainders
                                for prev-rmdr in rmdrs
                                do (when (eql rmdr prev-rmdr)
                                         (setf prev-rmdr 1)
                                         (loop until (eql prev-rmdr rmdr)
                                               do (loop do (decf rec-cyc)
                                                           (setf prev-rmdr (* prev-rmdr 10))
                                                        while (< prev-rmdr d))

                                                  (setf prev-rmdr (rem prev-rmdr d)))

                                         (when (> rec-cyc max-rec-cyc)
                                               (setf max-rec-cyc rec-cyc)
                                               (setf solution-d  d))
                                         
                                         ; (format t "~%d:     ~D~%" d)
                                         ; (format t "rmdr:  ~S~%" rmdr)
                                         ; (format t "rmdrs: ~S~%" rmdrs)
                                         ; (format t "1.0 / d:    ~F~%" (/ 1.0 d))
                                         ; (format t "rec-cyc:    ~D~%" rec-cyc)
                                         ; (finish-output nil)
                                         ; (sleep 1)

                                         (return-from divide-and-carry)))

                          (push rmdr rmdrs))))

    (format t "max: ~D: ~%" max-rec-cyc)
    solution-d))

                                           ; (when (> rec-cyc max-rec-cyc)
                                           ;       (setf max-rec-cyc rec-cyc)
                                           ;       (setf solution-d  d))

                                           ; (format t "~%d:     ~D~%" d)
                                           ; (format t "rmdr:  ~S~%" rmdr)
                                           ; (format t "rmdrs: ~S~%" rmdrs)
                                           ; (format t "1.0 / d:    ~F~%" (/ 1.0 d))
                                           ; (format t "len-digits: ~D~%" len-digits)
                                           ; (format t "rec-cyc:    ~D~%" rec-cyc)
                                           ; (finish-output nil)
                                           ; (sleep 1)

                                           ; (return-from divide-and-carry))


                                     ; (setf tail-rmdrs rem-tail)
                                     ; (setf rem-tail   (cdr rem-tail)))
                                             ; (loop named count-rec-zeros
                                             ;       for rec-zeros from 0
                                             ;       do (setf last-rmdr (* last-rmdr 10))
                                             ;          (unless (< last-rmdr base-dividend)
                                             ;                  (when (eql last-rmdr base-dividend)
                                             ;                        (incf rec-cyc rec-zeros))

                                             ;                  (return-from count-rec-zeros)))


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

