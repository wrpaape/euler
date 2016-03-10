defmodule Euler.Set4 do
  alias Euler.ExtAPI.{LispAPI,
                      JavaAPI,
                      CAPI}

  @moduledoc """
  Set4 houses solutions to problems 31 - 40.
  """

  @doc """
  31) Coin Sums

	In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:

	1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).

	It is possible to make £2 in the following way:

	1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p

	How many different ways can £2 be made using any number of coins?

  02/26/16

  iex> Euler.Set4.problem_31 |> elem(1)
  73682
  """
  def problem_31, do: LispAPI.call(~w(4 31))


  @doc """
  32) Pandigital Products

  We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.

  The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.

  Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.

  HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.

  02/29/16

  iex> Euler.Set4.problem_32 |> elem(1)
  45228
  """
  def problem_32, do: JavaAPI.call(~w(4 32))


  @doc """
  33) Digit Canceling Fractions

	The fraction ⁴⁹/₉₈ is a curious fraction, as an inexperienced mathematician in attempting to simplify it may incorrectly believe that ⁴⁹/₉₈ = ⁴/₈, which is correct, is obtained by cancelling the 9s.

	We shall consider fractions like, ³⁰/₅₀ = ³/₅, to be trivial examples.

	There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing two digits in the numerator and denominator.

	If the product of these four fractions is given in its lowest common terms, find the value of the denominator.

  03/02/16

  iex> Euler.Set4.problem_33 |> elem(1)
  100
  """
  def problem_33, do: CAPI.call(~w(4 33))


  @doc """
  34) Digit Factorials

  145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

  Find the sum of all numbers which are equal to the sum of the factorial of their digits.

  Note: as 1! = 1 and 2! = 2 are not sums they are not included.

  03/04/16

  iex> Euler.Set4.problem_34 |> elem(1)
  40730
  """
  def problem_34, do: CAPI.call(~w(4 34))


  @doc """
  35) Circular Primes

  The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.

  There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

  How many circular primes are there below one million?

  03/09/16

  iex> Euler.Set4.problem_35 |> elem(1)
  55
  """
  def problem_35, do: CAPI.call(~w(4 35))
end
