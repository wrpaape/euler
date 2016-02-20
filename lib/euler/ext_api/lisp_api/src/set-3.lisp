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

	(let ((base-dividend   100)
        (max-rec-cyc     6)
        (max-rec-cyc-dig 7))

    ;; loop over the remaining valid range of 'd'
		(loop for d from 13 below 15
          do (when (> d base-dividend)
					   (setf base-dividend (* base-dividend 10)))

             (let ((rmdr       (rem base-dividend d))
                   (prev-rmdrs '()))

               ; (format t "~%~%d:           ~D~%" d)
               ; (format t "max-rec-cyc:     ~D~%" max-rec-cyc)
               ; (format t "max-rec-cyc-dig: ~D~%" max-rec-cyc-dig)
               ; (force-output t)

               (loop named divide-and-carry
                     until (eql rmdr 0)
                     do (push rmdr prev-rmdrs)

                        (loop do (setf rmdr (* rmdr 10))
                                  while (< rmdr d))

                        (setf rmdr (rem rmdr d))

                        (format t "    d:     ~D~%" d)
                        (format t "    rmdr:  ~D~%" rmdr)
                        (format t "    rmdrs: ~S~%" prev-rmdrs)
                        (force-output t)

                        (loop for prev-rmdr in prev-rmdrs
                              for rec-cyc from 1
                              do (when (eql rmdr prev-rmdr)

                                       (let ((mult-prev-rmdr (* (car prev-rmdrs) 10))
                                             (trailing-zeros 0))

                                         (loop while (< mult-prev-rmdr base-dividend)
                                               do (setf mult-prev-rmdr (* mult-prev-rmdr 10))
                                                  (incf trailing-zeros))

                                         (when (eql mult-prev-rmdr base-dividend)
                                               (incf rec-cyc trailing-zeros)))

                        (format t "      rec-cyc:  ~D~%" rec-cyc)

                                       (when (> rec-cyc max-rec-cyc)
                                             (setf max-rec-cyc     rec-cyc)
                                             (setf max-rec-cyc-dig d))

                                       (return-from divide-and-carry))))))

    max-rec-cyc-dig))


                       ; (format t "~%d:        ~D~%" d)
                       ; (format t "~%rmdr:     ~D~%" rmdr)
                       ; (format t "~%rmdrs:    ~D~%" rmdrs)
                       ; (format t "rec-cyc:  ~D~%" rec-cyc)
                       ; (force-output t)
                                    

