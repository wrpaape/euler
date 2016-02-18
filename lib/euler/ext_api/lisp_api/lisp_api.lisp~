(defun main (set-num-str prob-num-str)
  (let ((set-num  (intern set-num-str))
        (prob-num (intern prob-num-str)))

  (print `(set number ,set-num))
  (print `(problem number ,prob-num))))

(defun fetch-argv ()
  (or
   #+SBCL      *posix-argv*  
   #+LISPWORKS system:*line-arguments-list*
   #+CMU       extensions:*command-line-words
   nil))

(and
  (defparameter *argv* (get-argv)) ; retrieve argv, including command as head
  (apply #'main (cdr *argv*)))     ; apply argv tail to 'main' function
