defmodule Euler.Ticker do
  alias IO.ANSI

  @default_tick_interval 100
  @tick_colors ~w(red yellow green blue cyan magenta)a
    |> Enum.map(&apply(ANSI, &1, []))
  @ticks 3..5
    |> Enum.concat([2])
    |> Enum.flat_map(fn(level) ->
      [level, level + 6]
      |> Enum.map(&<<8590 + &1 :: utf8>>)
    end)

##################################### external API #####################################
# ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓#

  def start(tick_interval \\ @default_tick_interval) do
    Kernel
    |> Agent.start(:spawn, [__MODULE__, :tick, [tick_interval]], name: __MODULE__)
  end

  def stop do
    __MODULE__
    |> Agent.get(& &1)
    |> Process.exit(:kill)

    __MODULE__
    |> Agent.stop

    [ANSI.clear_line, ANSI.reset]
    |> IO.write
  end


# ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑ ↑#
##################################### external API #####################################

  def tick(tick_interval) do
    {:ok, cols} = :io.columns

    tick_stream
    |> Enum.reduce(cols, fn
      (_skip_tick, 0)->
        ANSI.clear_line
        |> IO.write

        cols  
      ({color, tick}, rem_cols)->
        color
        <> tick
        |> IO.write

      tick_interval
      |> :timer.sleep

      rem_cols - 1
    end)
  end

####################################### helpers ########################################
# ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓ ↓#

  defp tick_stream, do: apply(Stream, :zip, Enum.map([@tick_colors, @ticks], &Stream.cycle/1))
end
