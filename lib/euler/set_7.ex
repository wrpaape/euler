defmodule Euler.Set7 do
  alias IO.ANSI
  alias Euler.ExtAPI.CAPI

  @moduledoc """
  Set7 houses solutions to problems 61 - 70.
  """

  @doc """
  15) Maximum Path Sum II

	By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.

     #{ANSI.red}3#{ANSI.reset}
    #{ANSI.red}7#{ANSI.reset} 4
   2 #{ANSI.red}4#{ANSI.reset} 6
	8 5 #{ANSI.red}9#{ANSI.reset} 3

	That is, 3 + 7 + 4 + 9 = 23.

	Fin the maximum total from top to bottom in triangle.txt (right click and 'Save Link/Target As...'), a 15K text file containing a triangle with one-hundred rows.

	NOTE: This is a much more difficult version of Problem 18. It is not possible to try every route to solve this problem, as there are 2⁹⁹ altogether! If you could check one trillion (10¹²) routes every second it would take over twenty billion years to check them all. There is an efficient algorithm to solve it. ;o)

  02/08/16

  iex> Euler.Set7.problem_67 |> elem(1)
  7273
  """
  def problem_67, do: CAPI.call(~w(7 67))
end
