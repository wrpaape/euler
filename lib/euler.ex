defmodule Euler do
  alias IO.ANSI
  alias Euler.Ticker

  @moduledoc """
  This module is responsible for running and reporting answers to
  problem sets hosted on projecteuler.
  """

  @problems_per_module Application.get_env(:euler, :problems_per_module)

  def main(argv) do
    argv
    |> Enum.each(fn(problem_number_string) ->
      problem_number_string
      |> Integer.parse
      |> process(problem_number_string)
    end)
  end

  def process(:error, arg_string) do
    [ANSI.bright,
     ANSI.red,
     ANSI.blink_slow,
     "Failed to parse problem number from:\n  ",
     ANSI.blink_off,
     ANSI.white_background,
     inspect(arg_string),
     ANSI.reset] |> IO.puts
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

    {microseconds, solution} =
      set_module
      |> :timer.tc(problem_function, [])

    Ticker.stop

    ["\nsolution:\n\n  ",
     ANSI.bright,
     ANSI.cyan_background,
     ANSI.magenta,
     inspect(solution),
     ANSI.reset,
     "\n\nsolved in:\n\n  ",
     format_time(microseconds),
     ANSI.reset] |> IO.puts
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

  def format_time(time) when time < 1_000,     do: [ANSI.green,  format_float(time),             " µs"]
  def format_time(time) when time < 1_000_000, do: [ANSI.yellow, format_float(time / 1_000),     " ms"]
  def format_time(time),                       do: [ANSI.red,    format_float(time / 1_000_000), " s" ]

  def format_float(float), do: :io_lib.format("~.4g", [float])
end
