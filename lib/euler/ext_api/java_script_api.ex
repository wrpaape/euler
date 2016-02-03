defmodule Euler.ExtAPI.JavaScriptAPI do
  alias Euler.ExtAPI

  @cmd  Application.get_env(:euler, :java_script_cmd)
  @dir  Application.get_env(:euler, :java_script_api_dir)
  @prog Application.get_env(:euler, :java_script_api_prog)
  
  def call(set_prob) do
    @cmd
    |> System.cmd([@prog | set_prob], cd: @dir)
    |> elem(0)
    |> ExtAPI.parse_stdout
  end
end
