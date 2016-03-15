defmodule EulerTest do
  use ExUnit.Case
  doctest Euler
  doctest Euler.Sets
  doctest Euler.Set1
  doctest Euler.Set2
  doctest Euler.Set3
  doctest Euler.Set4
  doctest Euler.Set7

  test "the truth" do
    assert 1 + 1 == 2
  end
end
