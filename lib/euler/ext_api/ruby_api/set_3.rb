=begin
**************************************************************************************
*                                    - set_3.rb -                                    *
*                                                                                    *
* Class 'Set3' houses solutions for problems 21-30.                                  *
**************************************************************************************
=end
class Set3
=begin
**************************************************************************************
*                                - self.problem_25 -                                 *
*                                                                                    *
* The Fibonacci sequence is defined by the recurrence relation:                      *
*                                                                                    *
* Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.                                         *
* Hence the first 12 terms will be:                                                  *
*                                                                                    *
* F1  = 1                                                                            *
* F2  = 1                                                                            *
* F3  = 2                                                                            *
* F4  = 3                                                                            *
* F5  = 5                                                                            *
* F6  = 8                                                                            *
* F7  = 13                                                                           *
* F8  = 21                                                                           *
* F9  = 34                                                                           *
* F10 = 55                                                                           *
* F11 = 89                                                                           *
* F12 = 144                                                                          *
*                                                                                    *
* The 12th term, F12, is the first term to contain three digits.                     *
*                                                                                    *
* What is the index of the first term in the Fibonacci sequence to contain 1000      *
* digits?                                                                            *
**************************************************************************************
=end
  def self.problem_25
    # init fibonacci object to track current and previous fibonacci numbers by index
    fib = {
      # last:  55,
      prev:  89,
      curr:  144,
      index: 12
    }

    # smallest 1000 digit number
    min_thousand_digits = 10 ** 999 # lmao ty Bignum

    # until current value of fibonacci object surpasses 1000 digits...
    until fib[:curr] >= min_thousand_digits
      last_prev    = fib[:prev] # copy previous fib to temporary variable
      fib[:prev]   = fib[:curr] # update next previous fib
      fib[:curr]  += last_prev  # next fib = sum of last current and previous fibs
      fib[:index] += 1          # increment index
    end

    fib[:index] # return index of 1000 digit fibonacci number
  end
end
