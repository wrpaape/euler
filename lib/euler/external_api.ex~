defmodule Euler.ExternalAPI do
  alias Euler.Ticker

  @cmd_map ~w(clang java)
    |> Enum.reduce(Map.new, fn(resource, cmd_map) ->
      res_key = resource
        |> String.to_atom

      cmd_key = resource <> "_api_cmd"
        |> String.to_atom

      cmd = :euler
        |> Application.get_env(cmd_key)

      cmd_map
      |> Map.put(res_key, cmd)
    end)
  
  def call(res_key, argv) do
    @cmd_map
    |> Map.get(res_key)
    |> System.cmd(argv)
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
