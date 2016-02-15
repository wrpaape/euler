defmodule Euler.Set3 do
  alias Euler.ExtAPI.{JavaAPI,
                      JavaScriptAPI,
                      CAPI}

  @moduledoc """
  Set3 houses solutions to problems 21 - 31.
  """

  @doc """
  21) Amicable Numbers

  Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
  If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.

  For example, the proper divisors of 220 are 2, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

  Evaluate the sum of all the amicable numbers under 10000.

  02/11/16

  iex> Euler.Set3.problem_21
  31626
  """
  def problem_21, do: JavaAPI.call(~w(3 21))


  @doc """
  22) Names Scores

  Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.

  For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.

  What is the total of all the name scores in the file?

  02/14/16

  iex> Euler.Set3.problem_22
  871198282
  """
  def problem_22, do: CAPI.call(~w(3 22))
  def problem_22_elixir do
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

  iex> Euler.Set3.problem_23
  """
  def problem_23, do: JavaScriptAPI.call(~w(3 23))
end
