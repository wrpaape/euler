defmodule EulerTest do
  use ExUnit.Case
  doctest Euler
  doctest Euler.Set1
  doctest Euler.Set2

  test "the truth" do
    assert 1 + 1 == 2
  end
end
