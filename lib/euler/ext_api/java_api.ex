defmodule Euler.ExtAPI.JavaAPI do
  alias Euler.ExtAPI

  @class_path Application.get_env(:euler, :java_api_cp)
  
  def call(set_prob) do
    "java"
    |> System.cmd(["JavaAPI" | set_prob], cd: @class_path)
    |> elem(0)
    |> ExtAPI.parse_stdout
  end
end
