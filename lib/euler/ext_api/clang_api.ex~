defmodule Euler.ExtAPI.ClangAPI do
  alias Euler.{ExtAPI,
               Ticker}

  @clang_api_cmd Application.get_env(:euler, :clang_api_cmd)
  
  def call(set_prob) do
    time_start = :erlang.timestamp

    @clang_api_cmd
    |> System.cmd(set_prob, stderr_to_stdout: true)
    |> case do
      {stdout,    0}      ->
        delay = :erlang.timestamp
          |> :timer.now_diff(time_start)

          {delay, ExtAPI.parse(stdout)}

      {error_msg, status} -> 
        Ticker.stop

        error_msg
        |> IO.write

        System.halt(status)
    end
  end
end
