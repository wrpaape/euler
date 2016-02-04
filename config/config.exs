# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
use Mix.Config

alias Mix.Project
alias IO.ANSI

ext_api_dir = ~w(.. .. lib euler ext_api)
  |> Path.join
  |> Path.expand(Project.build_path)

join_ansi = fn(ansi_fns) ->
  Enum.reduce(ansi_fns, "", fn(ansi_fn, ansi_acc) ->
    ansi_acc <> apply(ANSI, ansi_fn, [])
  end)
end

config :euler, [problems_per_module:  10,
                elixir_lang_tup:      {"Elixir", join_ansi.(~w(white_background magenta)a)},
                c_lang_tup:           {"C", join_ansi.(~w(faint white_background blue)a)},
                c_api_dir:            Path.join(ext_api_dir, "c_api"),
                c_api_exe:            "c_api",
                java_lang_tup:        {"Java", join_ansi.(~w(faint blue_background red)a)},
                java_cmd:             "java",
                java_api_class_path:  Path.join(ext_api_dir, "java_api"),
                java_api_main_class:  "JavaAPI",
                java_script_cmd:      "node",
                java_script_api_dir:  Path.join(ext_api_dir, "java_script_api"),
                java_script_api_prog: "java_script_api.js",
                java_script_lang_tup: {"JavaScript", join_ansi.(~w(yellow_background black)a)},
                ruby_lang_tup:        {"Ruby",  join_ansi.(~w(white_background red)a)},
                ruby_cmd:             "ruby",
                ruby_api_dir:         Path.join(ext_api_dir, "ruby_api"),
                ruby_api_prog:        "ruby_api.rb"]

# This configuration is loaded before any dependency and is restricted
# to this project. If another project depends on this project, this
# file won't be loaded nor affect the parent project. For this reason,
# if you want to provide default values for your application for
# 3rd-party users, it should be done in your "mix.exs" file.

# You can configure for your application as:
#
#     config :euler, key: :value
#
# And access this configuration in your application as:
#
#     Application.get_env(:euler, :key)
#
# Or configure a 3rd-party app:
#
#     config :logger, level: :info
#

# It is also possible to import configuration files, relative to this
# directory. For example, you can emulate configuration per environment
# by uncommenting the line below and defining dev.exs, test.exs and such.
# Configuration from the imported file will override the ones defined
# here (which is why it is important to import them last).
#
#     import_config "#{Mix.env}.exs"
