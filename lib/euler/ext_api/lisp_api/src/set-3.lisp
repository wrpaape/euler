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
        ; (base-digits     '(0))
        ; (base-rec-cyc    1)
        (max-rec-cyc     6)
        (max-rec-cyc-dig 7))

    ;; loop over the remaining valid range of 'd'
		; (loop for d from 11 below 1000 do
		(loop for d from 380 below 381 do
			(when (> d base-dividend)
					  (setf base-dividend (* base-dividend 10)))
            ; (push 0 base-digits)
            ; (incf base-rec-cyc))

      (let ((base-remainder (rem base-dividend d))
            (rmdr           base-dividend)
            (rec-cyc        0)
            ; (digs           base-digits)
            ; (rec-cyc        base-rec-cyc))

        (format t "~%~%d:           ~D~%" d)
        (format t "max-rec-cyc:     ~D~%" max-rec-cyc)
        (format t "max-rec-cyc-dig: ~D~%" max-rec-cyc-dig)
        (format t "base-dividend:   ~D~%" base-dividend)
        (format t "rmdr:            ~D~%" rmdr)
        ; (format t "digs:            ~S~%" digs)
        (force-output t)

      (loop named divide-and-carry do
        (setf rmdr)
        (multiple-value-setq (dig rmdr)
                             (floor rmdr d))

          (format t "    rmdr: ~D~%" rmdr)
          (format t "    dig:  ~D~%" dig)
          (force-output t)

          (when (eql rmdr 0)
                (setf rec-cyc 0)
                (return-from divide-and-carry))

          ; (when (or (and (eql dig  0)
          ;                (eql rmdr 0))
          ;           (member dig digs))
          ;       (return-from divide))
          (when (member dig digs)
                (return-from divide-and-carry))

          (loop do
                (setf rmdr (* rmdr 10))
                while (< rmdr d))

          (push dig digs)

          (incf rec-cyc))

          (format t "  d:        ~D~%" d)
          (format t "  rec-cyc:  ~D~%" rec-cyc)
          (format t "  digs:     ~S~%" digs)
          (force-output t)
                           
      (when (> rec-cyc max-rec-cyc)
            (setf max-rec-cyc     rec-cyc)
            (setf max-rec-cyc-dig d))))


    max-rec-cyc-dig))
