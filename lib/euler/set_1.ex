defmodule Euler.Set1 do
  @moduledoc """
  Set1 holds solutions to problems 1 - 25.
  """


  @doc """
  1) Multiples of 3 and 5

  If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
  Find the sum of all the multiples of 3 or 5 below 1000.

  01/10/16

  iex> Euler.Set1.problem_1
  233168
  """
  def problem_1 do
    1..999
    |> Enum.filter(&(rem(&1, 3) == 0 or rem(&1, 5) == 0))
    |> Enum.sum
  end


  @doc """
  2) Even Fibonacci Numbers

  If each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be:
  
  1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

  1/10/16

  iex> Euler.Set1.problem_2
  4613732
  """
  def problem_2 do
    {0, 1}
    |> Stream.unfold(fn
      ({x, y}) when x + y >= 4_000_000 ->
        nil

      ({x, y}) ->
        next = x + y

        {next, {y, next}}
    end)
    |> Enum.filter(&(rem(&1, 2) == 0))
    |> Enum.sum
  end


  @doc """
  3) Largest Prime Factor

  The prime factors of 13195 are 5, 7, 13 and 29.

  What is the largest prime factor of the number 600851475143 

  01/11/16

  iex> Euler.Set1.problem_3
  6857
  """
  @x 600851475143
  def problem_3(last_smallest_factor \\ 2) do
    smallest_factor =
      last_smallest_factor..@x - 1
      |> Enum.find(&(rem(@x, &1) == 0))

    largest_factor =
      @x
      |> div(smallest_factor)

    2..div(largest_factor, 2) + 1
    |> Enum.any?(&(rem(largest_factor, &1) == 0))
    |> if do
      smallest_factor
      |> + 1
      |> problem_3
    else
      largest_factor
    end
  end

  @doc """
  4) Largest Palindrome Product 

  A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

  Find the largest palindrome made from the product of two 3-digit numbers.

  01/11/16

  iex> Euler.Set1.problem_4
  906609
  """
  def problem_4 do
    999..100
    |> Enum.reduce(0, fn(x, max_palindrome_product) ->
      x..100
      |> Enum.find_value(fn(y) ->
        product = 
          x * y
          |> Integer.digits

        product
        |> Enum.reverse
        |> case do
          ^product -> Integer.undigits(product)
          ________ -> nil
        end
      end)
      |> case do
        nil ->
          max_palindrome_product

        palindrome_product ->
          max_palindrome_product
          |> max(palindrome_product)
      end
    end)
  end


  @doc """
  5) Smallest Multiple

  2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

  What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

  01/11/16

  iex> Euler.Set1.problem_5
  232792560
  """
  @fact_twenty Enum.reduce(2..20, &*/2)
  def problem_5(last_smallest \\ @fact_twenty) do
    2..20
    |> Enum.reduce(last_smallest, fn(x, last_smallest) ->
      last_smallest
      |> div(x)
      |> case do
        next_smallest when next_smallest == last_smallest / x ->
          20..2
          |> Enum.all?(&(rem(next_smallest, &1) == 0))
          |> if do
            next_smallest
          else
            last_smallest
          end
        _____________________________________________________ ->
          last_smallest
      end
    end)
    |> case do
      ^last_smallest -> last_smallest
      next_smallest  -> problem_5(next_smallest)
    end
  end

  @doc """
  6) Sum Square Difference
  The sum of the squares of the first ten natural numbers is,

  12 + 22 + ... + 102 = 385
  The square of the sum of the first ten natural numbers is,

  (1 + 2 + ... + 10)2 = 552 = 3025
  Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.

  Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

  01/12/16

  iex> Euler.Set1.problem_6
  25164150
  """
  def problem_6 do
    {sum_of_squares, sum} =
      1..100
      |> Enum.reduce({0, 0}, fn(x, {sum_of_squares, sum}) ->
        {sum_of_squares + :math.pow(x, 2), sum + x}
      end)

    sum
    |> :math.pow(2)
    |> - sum_of_squares
    |> trunc
  end

  @doc """
  10001st Prime
  By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

  What is the 10 001st prime number?

  01/12/16

  iex> Euler.Set1.problem7
  104743
  """
  def problem_7 do
    3
    |> Stream.iterate(&(&1 + 2))
    |> Enum.reduce_while({[], 1}, fn
      (_, {acc_primes, 10_001}) ->
        {:halt, List.last(acc_primes)}

      (x, {acc_primes, count}) -> 
        next_acc = 
          acc_primes
          |> Enum.reduce_while({[], acc_primes}, fn(_, {behind, [next | ahead]}) ->
            if rem(x, next) == 0 do
              {:halt, {acc_primes, count}}
            else
              {:cont, {[next | behind], ahead}}
            end
          end)
          |> case do
            {rev_acc_primes, []} -> {Enum.reverse([x | rev_acc_primes]), count + 1}
            last_acc             -> last_acc
          end

        {:cont, next_acc}
    end)
  end
end
