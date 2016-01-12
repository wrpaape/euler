defmodule Euler do
  alias IO.ANSI
  alias Euler.Ticker

  @moduledoc """
  This module is responsible for running and reporting answers to
  problem sets hosted on projecteuler.
  """

  @problems_per_module Application.get_env(:euler, :problems_per_module)
  @solution_colors     ANSI.cyan_background  <> ANSI.magenta
  @default_colors      ANSI.white_background <> ANSI.black

  def main(argv) do
    argv
    |> Enum.each(fn(problem_number_string) ->
      problem_number_string
      |> Integer.parse
      |> process(problem_number_string)
    end)
  end

  def process(:error, arg_string) do
   arg_string
   |> format_error
   |> IO.puts
  end

  def process({problem_number, _}, problem_number_string) do
    set_module =
      problem_number
      |> parse_set_module

    problem_function =
      "problem_" <> problem_number_string 
      |> String.to_atom

    set_module
    |> retreive_problem_doc(problem_function)
    |> IO.puts

    Ticker.start

    {time, solution} =
      set_module
      |> :timer.tc(problem_function, [])

    Ticker.stop

    solution
    |> format_output(time)
    |> IO.write
  end

  defp format_error(arg_string) do
    [ANSI.bright,
     ANSI.red,
     ANSI.blink_slow,
     "Failed to parse problem number from:\n  ",
     ANSI.blink_off,
     {inspect(arg_string), ANSI.red}
     |> frame]
  end

  defp format_output(solution, time) do
    ["\nsolution:\n\n",
     {inspect(solution), ANSI.cyan}
     |> frame,
     "\nsolved in:\n\n",
     time
     |> format_time
     |> frame]
  end

  def frame({string, colors}) do
    lpad = ["  ", colors]
    rnl  = [ANSI.reset, "\n"]

    {:ok, colspan} = :io.columns
    contents_colspan = colspan - 8

    row_length =
      string
      |> String.length
      |> min(contents_colspan)

    horiz = String.duplicate("═", row_length + 2)

    contents = 
      string
      |> Stream.unfold(fn
        (:halt)      -> nil
        (rem_string) ->
          rem_string
          |> String.split_at(row_length)
          |> case do
            {last_row, ""} ->
              {String.ljust(last_row, row_length), :halt} 

            next_tup ->
              next_tup
          end
      end)
      |> Enum.map(&[lpad, "║ ", ANSI.bright, &1, ANSI.normal, colors, " ║", rnl])

    [lpad, "╔", horiz, "╗", rnl , contents, lpad, "╚", horiz, "╝", rnl]
  end

  defp parse_set_module(problem_number) do
    __MODULE__
    |> Module.safe_concat("Set" <> parse_problem_set(problem_number))
  end

  defp parse_problem_set(problem_number) do
    problem_number
    |> div(@problems_per_module)
    |> + 1
    |> Integer.to_string
  end

  defp retreive_problem_doc(set_module, problem_function) do
    set_module
    |> Code.get_docs(:docs)
    |> Enum.find_value(fn
      ({{^problem_function, _}, _, _, _, doc}) -> doc
      (______________________________________) -> nil
    end)
  end

  defp format_time(time) when time < 1_000,     do: {Integer.to_string(time)        <> " µs", ANSI.green}
  defp format_time(time) when time < 1_000_000, do: {format_float(time / 1_000)     <> " ms", ANSI.yellow}
  defp format_time(time),                       do: {format_float(time / 1_000_000) <> " s",  ANSI.red}

  defp format_float(float), do: :io_lib.format("~.4g", [float]) |> List.to_string
end
