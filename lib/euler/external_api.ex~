defmodule Euler.ExternalAPI do
  @cmd_map Application.get_env(:euler, :cmd_map)
    # |> Application.get_env(:resources)
    # |> Enum.reduce(Map.new, fn(resource, cmd_map) ->
    #   res_key = resource
    #     |> String.to_atom

    #   cmd_key = resource <> "_api_cmd"
    #     |> String.to_atom

    #   cmd = :euler
    #     |> Application.get_env(cmd_key)

    #   cmd_map
    #   |> Map.put(res_key, cmd)
    # end)
  
  def call(res_key, set_prob) do
    {cmd, argv} =
      @cmd_map
      |> Map.get(res_key)
      |> case do
        {cmd, prog} -> {cmd, [prog | set_prob]}
        cmd         -> {cmd, set_prob}
      end

    cmd
    |> System.cmd(argv, stderr_to_stdout: true)
    |> elem(0)
  end
end
