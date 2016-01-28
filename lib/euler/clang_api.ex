defmodule Euler.ClangAPI do
  alias Euler.Ticker

  @clang_api_cmd Application.get_env(:euler, :clang_api_cmd)
  
  def call(argv) do
    System.cmd(@clang_api_cmd, argv)
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
