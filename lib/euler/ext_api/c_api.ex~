defmodule Euler.ExtAPI.CAPI do
  alias Euler.{ExtAPI,
               Ticker}

  @lang_tup Application.get_env(:euler, :c_lang_tup)
  @dir      Application.get_env(:euler, :c_api_dir)
  @exe      Application.get_env(:euler, :c_api_exe)
  @cmd      Path.join(@dir, @exe)

  def call(set_prob) do
    @cmd
    |> System.cmd(set_prob, cd: @dir, stderr_to_stdout: true)
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
