defmodule Euler.ExtAPI.CAPI do
  alias Euler.{ExtAPI,
               Ticker}

  @dir Application.get_env(:euler, :c_api_dir)
  @exe Application.get_env(:euler, :c_api_exe)
  @cmd Path.join(@dir, @exe)
  
  def call(set_prob) do
    @cmd
    |> System.cmd(set_prob, stderr_to_stdout: true)
    |> case do
      {stdout, 0}         ->
        stdout
        |> ExtAPI.parse_stdout

      {error_msg, status} -> 
        Ticker.stop

        error_msg
        |> IO.write

        System.halt(status)
    end
  end
end
