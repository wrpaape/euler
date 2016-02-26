defmodule Euler.Set4 do
  alias Euler.ExtAPI.{LispAPI}

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

  # 02/25/16

  # iex> Euler.Set4.problem_31
  """
  def problem_31, do: LispAPI.call(~w(4 31))
end
