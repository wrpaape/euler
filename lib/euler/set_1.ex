defmodule Euler.Set1 do
  @moduledoc """
  Set1 holds solutions to problems 1 - 25.
  """


  @doc """
  1) Multiples of 3 and 5

  If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
  Find the sum of all the multiples of 3 or 5 below 1000.

  1/10/16

  Answer: 233168
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

  Answer: 4612732
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

  1/11/16

  Answer: 6857
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

  1/11/16

  Answer: 906609
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

  """
  def problem_5
end
