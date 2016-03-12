defmodule Euler.ExtAPI.JavaAPI do
  alias Euler.ExtAPI

  @lang_tup   Application.get_env(:euler, :java_lang_tup)
  @cmd        Application.get_env(:euler, :java_cmd)
  @class_path Application.get_env(:euler, :java_api_class_path)
  @jar_file   Application.get_env(:euler, :java_api_jar_file)
  
  def call(set_prob) do
    @cmd
    |> System.cmd(["-jar" | [@jar_file | set_prob]], cd: @class_path, stderr_to_stdout: true)
    |> elem(0)
    |> ExtAPI.parse_stdout
    |> Tuple.append(@lang_tup)
  end
end
