defmodule Euler.ClangAPI do
  @clang_api_dir Application.get_env(:euler, :clang_api_dir)
  @clang_api_cmd Path.join(@clang_api_dir, "clang_api")
  
  def call(set_num, prob_num), do: System.cmd(@clang_api_cmd, [set_num, prob_num])
end
