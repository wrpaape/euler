defmodule Euler.Set3 do
  alias IO.ANSI
  alias Euler.ExtAPI.{JavaAPI,
                      JavaScriptAPI,
                      RubyAPI,
                      CAPI,
                      LispAPI}

  @moduledoc """
  Set3 houses solutions to problems 21 - 30.
  """

  @doc """
  21) Amicable Numbers

  Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
  If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.

  For example, the proper divisors of 220 are 2, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

  Evaluate the sum of all the amicable numbers under 10000.

  02/11/16

  iex> Euler.Set3.problem_21 |> elem(1)
  31626
  """
  def problem_21, do: JavaAPI.call(~w(3 21))


  @doc """
  22) Names Scores

  Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.

  For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.

  What is the total of all the name scores in the file?

  02/14/16

  iex> Euler.Set3.problem_22 |> elem(1)
  871198282
  """
  def problem_22, do: CAPI.call(~w(3 22))
  def problem_22_elixir do                      # arrives at same solution as C in ~20 ms vs ~5ms
    :euler
    |> Application.get_env(:c_api_dir)
    |> Path.join("data/set_3-prob_22-data.txt") # fetch data file stored in 'c_api' directory
    |> File.read!                               # read in data
    |> String.split(~w(" ,), trim: true)        # split into individual names
    |> Enum.sort                                # sort alphabetically
    |> Enum.reduce({0, 1}, fn(name, {sum_name_scores, index}) ->
      char_score =
        name
        |> String.to_char_list                            # decompose bitstring to list of ASCII chars
        |> Enum.reduce(0, &(&1 + &2 - ?@))                # sum offset of each from '@' ('A' => 1, 'Z' => 26) => add to acc
      
      {sum_name_scores + (char_score * index), index + 1} # mult by index to calc 'name score' => add to acc, inc index
    end)
    |> elem(0) # take first elem of acc tuple, the total of all name scores
  end


  @doc """
  23) Non-abundant Sums

  A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

  A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.

  As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant numbers is 24. By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers. However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.

  Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.

  02/14/16

  iex> Euler.Set3.problem_23 |> elem(1)
  4179871
  """
  def problem_23, do: JavaScriptAPI.call(~w(3 23))


  @doc """
  24) Lexicographic permutations

  A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically, we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:

  012   021   102   120   201   210

  What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?

  02/15/16

  iex> Euler.Set3.problem_24 |> elem(1)
  2783915460
  """
  def problem_24, do: CAPI.call(~w(3 24))

  @doc """
  25) 1000-digit Fibonacci Number

  The Fibonacci sequence is defined by the recurrence relation:

  Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
  Hence the first 12 terms will be:

  F1  = 1
  F2  = 1
  F3  = 2
  F4  = 3
  F5  = 5
  F6  = 8
  F7  = 13
  F8  = 21
  F9  = 34
  F10 = 55
  F11 = 89
  F12 = 144

  The 12th term, F12, is the first term to contain three digits.

  What is the index of the first term in the Fibonacci sequence to contain 1000 digits?

  02/16/16

  iex> Euler.Set3.problem_25 |> elem(1)
  4782
  """
  def problem_25, do: RubyAPI.call(~w(3 25))


  @doc """
  26) Reciprocal Cycles

  A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:

  1/2  = 0.5
  1/3  = 0.(3)
  1/4  = 0.25
  1/5  = 0.2
  1/6  = 0.1(6)
  1/7  = 0.(142857)
  1/8  = 0.125
  1/9  = 0.(1)
  1/10 = 0.1

  Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.

  Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.

  02/20/16

  iex> Euler.Set3.problem_26 |> elem(1)
  983
  """
  def problem_26, do: LispAPI.call(~w(3 26))


  @doc """
  27) Quadratic Primes

  Euler discovered the remarkable quadratic formula:

  n² + n + 41

  It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and certainly when n = 41, 41² + 41 + 41 is clearly divisible by 41.

  The incredible formula  n² − 79n + 1601 was discovered, which produces 80 primes for the consecutive values n = 0 to 79. The product of the coefficients, −79 and 1601, is −126479.

  Considering quadratics of the form:

  n² + an + b, where |a| < 1000 and |b| < 1000

  where |n| is the modulus/absolute value of n
  e.g. |11| = 11 and |−4| = 4

  Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n = 0.

  02/22/16

  iex> Euler.Set3.problem_27 |> elem(1)
  -59231
  """
  def problem_27, do: LispAPI.call(~w(3 27))


  @doc """
  28) Number Spiral Diagonals

  Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:

  #{ANSI.red}21#{ANSI.reset} 22 23 24 #{ANSI.red}25#{ANSI.reset}
  20  #{ANSI.red}7#{ANSI.reset}  8  #{ANSI.red}9#{ANSI.reset} 10
  19  6  #{ANSI.red}1#{ANSI.reset}  2 11
  18  #{ANSI.red}5#{ANSI.reset}  4  #{ANSI.red}3#{ANSI.reset} 12
  #{ANSI.red}17#{ANSI.reset} 16 15 14 #{ANSI.red}13#{ANSI.reset}

  It can be verified that the sum of the numbers on the diagonals is 101.

  What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?

  02/22/16

  iex> Euler.Set3.problem_28 |> elem(1)
  669171001
  """
  def problem_28, do: JavaAPI.call(~w(3 28))


  @doc """
  29) Distinct Powers

  Consider all integer combinations of aᵇ for 2 ≤ a ≤ 5 and 2 ≤ b ≤ 5:

  2²=4,  2³=8,   2⁴=16,  2⁵=32
  3²=9,  3³=27,  3⁴=81,  3⁵=243
  4²=16, 4³=64,  4⁴=256, 4⁵=1024
  5²=25, 5³=125, 5⁴=625, 5⁵=3125
  If they are then placed in numerical order, with any repeats removed, we get the following sequence of 15 distinct terms:

  4, 8, 9, 16, 25, 27, 32, 64, 81, 125, 243, 256, 625, 1024, 3125

  How many distinct terms are in the sequence generated by aᵇ for 2 ≤ a ≤ 100 and 2 ≤ b ≤ 100?

  02/24/16

  iex> Euler.Set3.problem_29 |> elem(1)
  9183
  """
  def problem_29, do: CAPI.call(~w(3 29))


  @doc """
  30) Digit Fifth Powers

  Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:

  1634 = 1⁴ + 6⁴ + 3⁴ + 4⁴
  8208 = 8⁴ + 2⁴ + 0⁴ + 8⁴
  9474 = 9⁴ + 4⁴ + 7⁴ + 4⁴

  As 1 = 1⁴ is not a sum it is not included.

  The sum of these numbers is 1634 + 8208 + 9474 = 19316.

  Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.

  02/25/16

  iex> Euler.Set3.problem_30 |> elem(1)
  443839
  """
  def problem_30, do: CAPI.call(~w(3 30))
end
