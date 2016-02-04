defmodule Euler.ExtAPI.JavaAPI do
  alias Euler.ExtAPI

  @lang_tup Application.get_env(:euler, :java_lang_tup)
  @cmd      Application.get_env(:euler, :java_cmd)
  @dir      Application.get_env(:euler, :java_api_class_path)
  @prog     Application.get_env(:euler, :java_api_main_class)
  
  def call(set_prob) do
    @cmd
    |> System.cmd([@prog | set_prob], cd: @dir)
    |> elem(0)
    |> ExtAPI.parse_stdout
    |> Tuple.append(@lang_tup)
  end
end
