defmodule Euler.Set2 do
  alias Euler.ClangAPI

  @moduledoc """
  Set2 holds solutions to problems 11 - 20.
  """

  alias IO.ANSI

  @grid """
  08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
  49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
  81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
  52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
  22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
  24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
  32 98 81 28 64 23 67 10 #{ANSI.red}26#{ANSI.reset} 38 40 67 59 54 70 66 18 38 64 70
  67 26 20 68 02 62 12 20 95 #{ANSI.red}63#{ANSI.reset} 94 39 63 08 40 91 66 49 94 21
  24 55 58 05 66 73 99 26 97 17 #{ANSI.red}78#{ANSI.reset} 78 96 83 14 88 34 89 63 72
  21 36 23 09 75 00 76 44 20 45 35 #{ANSI.red}14#{ANSI.reset} 00 61 33 97 34 31 33 95
  78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
  16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
  86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
  19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
  04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
  88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
  04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
  20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
  20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
  01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48
  """
  @doc"""
  In the 20×20 grid below, four numbers along a diagonal line have been marked in red.

  #{@grid}
  The product of these numbers is 26 × 63 × 78 × 14 = 1788696.

  What is the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) in the 20×20 grid?
  """
  @adj_count 4
  @grid_size 20
  @grid_list String.replace(@grid, [ANSI.red, ANSI.reset], "")
    |> String.split("\n", trim: true)
    |> Enum.map(fn(row_str) ->
      row_str
      |> String.split(" ")
      |> Enum.map(&String.to_integer/1)
    end)

  def problem_11 do
    0
    |> horiz
    |> vert
    |> diag
  end

  def horiz(max_prod), do: Enum.reduce(@grid_list, max_prod, &do_horiz(&1, &2, 1, 1, []))

  def do_horiz([0 | rem_row], max_prod, _, _, _) do
    rem_row
    |> do_horiz(max_prod, 1, 1, [])
  end

  def do_horiz([cell | rem_row], max_prod, @adj_count, tail_prod, [tail_hd | tail_tl]) do
    next_prod      = cell * tail_prod
    next_max_prod  = max(max_prod, next_prod)
    next_tail_prod = div(next_prod, tail_hd)
    next_tail      = List.insert_at(tail_tl, @adj_count - 2, cell)

    rem_row
    |> do_horiz(next_max_prod, @adj_count, next_tail_prod, next_tail)
  end

  def do_horiz([cell | rem_row], max_prod, counter, tail_prod, tail) do
    next_tail =
      counter
      |> case do
        @adj_count - 1 -> Enum.reverse([cell | tail])
        ______________ -> [cell | tail]
      end

    rem_row
    |> do_horiz(max_prod, counter + 1, tail_prod * cell, next_tail)
  end

  def do_horiz([], max_prod, _, _, _), do: max_prod

  def vert(max_prod), do: do_vert({@grid_list, max_prod})

  def do_vert({[[] | _], max_prod}), do: max_prod
  def do_vert({rem_grid, max_prod})  do
    rem_grid
    |> Enum.reduce({{[], max_prod}, 1, 1, []}, fn
      ([0 | rem_row], {{rem_grid, max_prod}, _, _, _}) ->
        {{[rem_row | rem_grid], max_prod}, 1, 1, []}


      ([cell | rem_row], {{rem_grid, max_prod}, @adj_count, tail_prod, [tail_hd | tail_tl]}) ->
        next_prod      = cell * tail_prod
        next_max_prod  = max(max_prod, next_prod)
        next_tail_prod = div(next_prod, tail_hd)
        next_tail      = List.insert_at(tail_tl, @adj_count - 2, cell)

        {{[rem_row | rem_grid], next_max_prod}, @adj_count, next_tail_prod, next_tail}


      ([cell | rem_row], {{rem_grid, max_prod}, counter, tail_prod, tail}) ->
        next_tail =
          counter
          |> case do
            @adj_count - 1 -> Enum.reverse([cell | tail])
            ______________ -> [cell | tail]
          end

        {{[rem_row | rem_grid], max_prod}, counter + 1, tail_prod * cell, next_tail}
    end)
    |> elem(0)
    |> do_vert
  end

  def diag(max_prod) do
    @grid_list
    |> Enum.reduce({[[], [], [], []], 0, @grid_size}, fn(row, {[b_l, t_r, b_r, t_l], bl_tr_split, br_tl_split}) ->
        {bl_row, tr_row} = Enum.split(row, bl_tr_split)
        {br_row, tl_row} = Enum.split(row, br_tl_split)

        {[[Enum.reverse(bl_row) | b_l], [tr_row | t_r], [Enum.reverse(br_row) | b_r], [tl_row | t_l]], bl_tr_split + 1, br_tl_split - 1}
    end)
    |> elem(0)
    |> Enum.reduce(max_prod, &do_diag(&1, [], &2, 1, 1, []))
  end

  def do_diag([[0 | rem_row] | rem_diag], next_diag, max_prod, _, _, _) do
    rem_diag
    |> do_diag([rem_row | next_diag], max_prod, 1, 1, [])
  end

  def do_diag([[cell | rem_row] | rem_diag], next_diag, max_prod, @adj_count, tail_prod, [tail_hd | tail_tl]) do
    next_prod      = cell * tail_prod
    next_max_prod  = max(max_prod, next_prod)
    next_tail_prod = div(next_prod, tail_hd)
    next_tail      = List.insert_at(tail_tl, @adj_count - 2, cell)

    rem_diag
    |> do_diag([rem_row | next_diag], next_max_prod, @adj_count, next_tail_prod, next_tail)
  end

  def do_diag([[cell | rem_row] | rem_diag], next_diag, max_prod, counter, tail_prod, tail) do
    next_tail =
      counter
      |> case do
        @adj_count - 1 -> Enum.reverse([cell | tail])
        ______________ -> [cell | tail]
      end

    rem_diag
    |> do_diag([rem_row | next_diag], max_prod, counter + 1, tail_prod * cell, next_tail)
  end

  def do_diag([[] | rem_diag], next_diag, max_prod, _, _, _), do: do_diag(rem_diag, next_diag, max_prod, 1, 1, [])
  def do_diag([], [], max_prod, _, _, _),                     do: max_prod
  def do_diag([], next_diag, max_prod, _, _, _),              do: do_diag(next_diag, [], max_prod, 1, 1, [])


  @doc """
  The sequence of triangle numbers is generated by adding the natural numbers. So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. The first ten terms would be:

  1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

  Let us list the factors of the first seven triangle numbers:

   1: 1
   3: 1,3
   6: 1,2,3,6
  10: 1,2,5,10
  15: 1,3,5,15
  21: 1,3,7,21
  28: 1,2,4,7,14,28
  We can see that 28 is the first triangle number to have over five divisors.

  What is the value of the first triangle number to have over five hundred divisors?
  """
  def problem_12, do: ClangAPI.call(~w(2 12))
end
    # 1..100
    # |> Enum.map_reduce(0, fn(n, last_tri) ->
    #   tri = last_tri + n
    #   {Enum.join([tri, ": ", Enum.filter(1..tri, &(rem(tri, &1) == 0)) |> Enum.join(", ") ]), tri}
    # end)
    # |> elem(0)
    # |> Enum.join("\n")
		# |> IO.puts