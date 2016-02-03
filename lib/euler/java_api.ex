defmodule Euler.JavaAPI do
  @class_path Application.get_env(:euler, :java_api_cp)
  
  def call(set_prob) do
    "java"
    |> System.cmd(["JavaAPI" | set_prob], cd: @class_path)
    |> elem(0)
  end
end
