defmodule Euler.ExtAPI.JavaAPI do
  alias Euler.ExtAPI

  @cmd  Application.get_env(:euler, :java_cmd)
  @prog Application.get_env(:euler, :java_api_prog)
  @dir  Application.get_env(:euler, :java_api_cp)
  
  def call(set_prob) do
    @cmd
    |> System.cmd([@prog | set_prob], cd: @dir)
    |> elem(0)
    |> ExtAPI.parse_stdout
  end
end
