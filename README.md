# Euler

## Purpose

> _Euler_ is an ongoing exercise in exploring the "best" (fastest/most efficient/most concise/most "proper") implementations of new and familiar data-structures and algorithms in a variety of programming languages and paradigms while attempting to perserve industry(Stack Overflow)-best practices including: 
  - code readability and reuse
  - coherence to language-specific conventions of style and structure
  - thorough testing, documentation and error handling, and 
  - consistent and maintainable project organization.

> Basically this repo serves as a hub to catalog [the progress on my Project Euler account](https://projecteuler.net/progress=tastyham).


## Structure

> Wrapped in a commandline interface built with Elixir's Mix framework, _Euler_ houses solutions to mathematical word problems hosted on [projecteuler.net](https://projecteuler.net). Commands issued by one or more problem numbers are dispatched from the master Mix 'Euler' module to the 'API' module of the solution language. The solution code is then timed and executed by the API, and its results are relayed back to master application where they formatted and printed alongside other information pertaining to the problem(s)'s solution.

## Usage

  `$ ./euler [<problem number(s)> | --latest/-l | --all/-a | --help/-h]`


## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed as:

  1. Add euler to your list of dependencies in `mix.exs`:

        def deps do
          [{:euler, "~> 0.0.1"}]
        end

  2. Ensure euler is started before your application:

        def application do
          [applications: [:euler]]
        end

