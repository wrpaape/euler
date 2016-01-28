defmodule Euler.ClangAPI do
  @clang_api_dir Application.get_env(:euler, :clang_api_dir)
  @clang_api_cmd Path.join(@clang_api_dir, "clang_api")
  
  def call(argv), do: System.cmd(@clang_api_cmd, argv)
end
