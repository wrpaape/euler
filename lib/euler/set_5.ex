defmodule Euler.Set5 do
  alias Euler.ExtAPI.{CAPI}

  @moduledoc """
  Set5 houses solutions to problems 41 - 50.
  """

  @doc """
  41) Pandigital Prime

  We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is also prime.

  What is the largest n-digit pandigital prime that exists?
  """
  def problem_41, do: CAPI.call(~w(5 41))
end
