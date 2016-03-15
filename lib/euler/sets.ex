defmodule Euler.Sets do
  require Integer

  @moduledoc """
  Sets houses common functionality shared amongst 'Euler.SetX' problem set modules
  """

  @doc """
  nth_pow/2 - positive integer power ("inspired by" stackoverflow user Paweł Obrok)

  iex> 5 |> Euler.Sets.nth_pow(4)
  625
  """
  def nth_pow(_, 0),                        do: 1
  def nth_pow(x, n) when Integer.is_odd(n), do: x * nth_pow(x, n - 1)
  def nth_pow(x, n)                         do
    sqrt_rem = nth_pow(x, div(n, 2))
    sqrt_rem * sqrt_rem
  end
end
