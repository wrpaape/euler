defmodule Euler.ExtAPI.JavaScriptAPI do
  alias Euler.{ExtAPI,
               Ticker}

  @cmd  Application.get_env(:euler, :java_script_cmd)
  @dir  Application.get_env(:euler, :java_script_api_dir)
  @prog Application.get_env(:euler, :java_script_api_prog)
  
  def call(set_prob) do
    @cmd
    |> System.cmd([@prog | set_prob], cd: @dir, stderr_to_stdout: true)
    |> case do
      {stdout, 0}         ->
        stdout
        |> ExtAPI.parse_stdout
          

      {error_msg, status} -> 
        Ticker.stop

        error_msg
        |> IO.write

        System.halt(status)
    end
    |> ExtAPI.parse_stdout
  end
end
