defmodule Euler.ExtAPI do
  def parse(stdout) do
    stdout
    |> Integer.parse
    |> case do
      :error        -> stdout
      {int_sol, ""} -> int_sol
      ____________  ->
        stdout
        |> Float.parse
        |> elem(0)
    end
  end
end
