defmodule Euler.ExtAPI.RubyAPI do
  alias Euler.{ExtAPI,
               Ticker}

  @lang_tup Application.get_env(:euler, :ruby_lang_tup)
  @cmd      Application.get_env(:euler, :ruby_cmd)
  @dir      Application.get_env(:euler, :ruby_api_dir)
  @prog     Application.get_env(:euler, :ruby_api_prog)
  
  def call(set_prob) do
    @cmd
    |> System.cmd([@prog | set_prob], cd: @dir, stderr_to_stdout: true)
    |> case do
      {stdout, 0}         ->
        stdout
        |> ExtAPI.parse_stdout
        |> Tuple.append(@lang_tup)

      {error_msg, status} -> 
        Ticker.stop

        error_msg
        |> IO.write

        System.halt(status)
    end
  end
end
