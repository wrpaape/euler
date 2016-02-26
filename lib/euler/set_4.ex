defmodule Euler.Set4 do
  alias Euler.ExtAPI.{LispAPI,
                      JavaAPI}

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
  """
  def problem_32, do: JavaAPI.call(~w(4 32))
end
