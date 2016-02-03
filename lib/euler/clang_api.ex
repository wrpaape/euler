defmodule Euler.ClangAPI do
  alias Euler.Ticker

  @clang_api_cmd Application.get_env(:euler, :clang_api_cmd)
  
  def call(set_prob) do
    @clang_api_cmd
    |> System.cmd(set_prob, stderr_to_stdout: true)
    |> case do
      {result,    0}      ->
        result

      {error_msg, status} -> 
        Ticker.stop

        error_msg
        |> IO.write

        System.halt(status)
    end
  end
end
