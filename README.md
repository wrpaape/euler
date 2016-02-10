# Euler

## Purpose
> _Euler_ is an ongoing exercise in exploring the "best" (fastest/most efficient/most succinct/"proper") implementation of new and familiar data-structures and algorithms in a variety of programming languages and paradigms while attempting to perserve "industry"(Stack Overflow)-best practices of:
  - code readability and reuse
  - coherence to language-specific conventions of style and structure
  - thorough testing and documentation, and 
  - consistent and maintainable project organization.

> Basically this repo serves as a hub to house [the progress on my Project Euler account](https://projecteuler.net/progress=tastyham).

## Structure
> Wrapped in a commandline interface built with Elixir's Mix framework, Euler houses solutions to mathematical word problems hosted on [projecteuler.net](https://projecteuler.net).

## Usage

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

