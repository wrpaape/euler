defmodule Euler do
  alias IO.ANSI
  alias Euler.Ticker

  @moduledoc """
  This module is responsible for running and reporting answers to
  problem sets hosted on projecteuler.
  """

  @problems_per_module Application.get_env(:euler, :problems_per_module)
  @default_lang_tup    Application.get_env(:euler, :elixir_lang_tup)
  @solution_colors     ANSI.cyan_background  <> ANSI.magenta
  @default_colors      ANSI.white_background <> ANSI.black
  @parse_opts [strict:  [help: :boolean, all: :boolean, latest: :boolean],
               aliases: [h:    :help,    a:   :all,     l:      :latest]]

  def main(argv) do
    argv
    |> OptionParser.parse(@parse_opts)
    |> handle_parse
    |> Enum.each(&process/1)
  end

  def handle_parse({[], problem_number_strings, []}) do
    problem_number_strings
    |> Enum.map(&{Integer.parse(&1), &1})
  end
  def handle_parse({opts, _, []}),                    do: opts
  def handle_parse({_, _, invalids}),                 do: invalids

  def process({invalid_opt, nil}) do
    invalid_opt
    |> format_error("Invalid option")
    |> IO.puts
  end

  def process({:help, :true}) do
    "euler [<problem numbers> | --latest/-l | --all/-a | --help/-h]"
    |> IO.puts
  end

  def process({:all, true}) do
  end

  def process({:latest, true}) do
  end

  def process({:error, arg_string}) do
    arg_string
    |> format_error("Failed to parse problem number from")
    |> IO.puts
  end

  def process({{problem_number, _}, problem_number_string}) do
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

    {time, solution, language_tup} =
      set_module
      |> :timer.tc(problem_function, [])
      |> case do
        {_delayed, api_tup = {_time, _sol, _lang_tup}} ->
          api_tup

        local_tup ->
          local_tup
          |> Tuple.append(@default_lang_tup)
      end

    Ticker.stop

    solution
    |> format_output(time, language_tup)
    |> IO.write
  end

  def format_error(arg_string, msg) do
    [ANSI.bright,
     ANSI.red,
     ANSI.blink_slow,
     msg,
     ":\n  ",
     ANSI.blink_off,
     {inspect(arg_string), ANSI.red}
     |> frame]
  end

  defp format_output(solution, time, language_tup) do
    ["\nsolution:\n\n",
     {inspect(solution), ANSI.cyan}
     |> frame,
     "\nsolved with:\n\n",
     language_tup
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
    |> div(@problems_per_module + 1)
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
