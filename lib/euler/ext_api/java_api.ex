defmodule Euler.ExtAPI.JavaAPI do
  alias Euler.ExtAPI

  @class_path Application.get_env(:euler, :java_api_cp)
  
  def call(set_prob) do
    time_start = :erlang.timestamp

    stdout =
      "java"
      |> System.cmd(["JavaAPI" | set_prob], cd: @class_path)
      |> elem(0)

    delay = :erlang.timestamp
      |> :timer.now_diff(time_start)

    {delay, ExtAPI.parse(stdout)}
  end
end
