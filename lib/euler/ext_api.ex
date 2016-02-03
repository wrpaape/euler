defmodule Euler.ExtAPI do
  def parse_stdout(stdout) do
    stdout
    |> :binary.split("\n")
    |> case do
      [sol_str, time_str] ->
        {parse_time(time_str), parse_sol(sol_str)}

      ___________________ ->
        stdout
        |> Euler.format_error("Improper solution format")
        |> IO.puts

        System.halt(1)
    end
  end

  defp parse_time(time_str) do
    time_str
    |> Integer.parse
    |> case do
      {micro_secs, ""} -> micro_secs
      ________________ ->
        time_str
        |> Euler.format_error("Failed to parse time elapsed from")
        |> IO.puts

        System.halt(1)
    end
  end

  defp parse_sol(sol_str) do
    sol_str
    |> Integer.parse
    |> case do
      :error        -> sol_str
      {int_sol, ""} -> int_sol
      ____________  ->
        sol_str
        |> Float.parse
        |> elem(0)
    end
  end
end
