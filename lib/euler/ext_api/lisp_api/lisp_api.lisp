(defun main (set-num-str prob-num-str)
  (let ((set-num  (intern set-num-str))
        (prob-num (intern prob-num-str)))

  (print `(set number ,set-num))
  (print `(problem number ,prob-num))))

(defun get-argv ()
  (or
   #+SBCL      *posix-argv*  
   #+LISPWORKS system:*line-arguments-list*
   #+CMU       extensions:*command-line-words
   nil))


(and
  (defparameter *argv* (get-argv))
  (print *argv*)
  (apply #'main (cdr *argv*)))
