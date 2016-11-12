defmodule House.Scene do
  @moduledoc """
  The scene is determined by the time of day.
  Based on a scene certain colour temperatures,
  and brightness values are chosen.

  Scenes might also use different subsets of lights.

  Lamps
  1: Sofa - living room
  2: Ceiling lamp - hallway
  3: Ceiling lamp - bedroom
  4: By the sink - bathroom
  5: By the shower - bathroom
  6: Over the kitchen table - kitchen
  """

  require Logger


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  def on(light) do
    case brightness(light) do
      0 -> false
      _ -> true
    end
  end

  def brightness(light), do: brightness(program(), light)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  # During the morning we want nice and bright lights
  def brightness(:morning, _), do: range_up(50, 254, minutes(10), seconds_since(~T[06:00:00]))
  # During the day we want nice and bright lights
  def brightness(:day, _), do: 254
  # During the evening we want to slowly dim the secondary lights
  def brightness(:evening, _), do: range_down(254, 100, hours(3), seconds_since(~T[18:00:00]))
  def brightness(:night, _), do: range_down(100, 50, minutes(30), seconds_since(~T[23:00:00]))

  defp program() do
    current_hour = Timex.local.hour
    cond do
      current_hour >= 6 and current_hour < 9 -> :morning
      current_hour >= 9 and current_hour < 18 -> :day
      current_hour >= 18 and current_hour < 23 -> :evening
      current_hour >= 23 and current_hour <= 24 -> :night
      current_hour >= 0 and current_hour <= 6 -> :night
    end
  end

  defp range_down(from, to, time_to_max, elapsed_time) do
    current_val = min(time_to_max, elapsed_time)
    range = from - to
    per_second = range / time_to_max
    round(from - current_val * per_second)
  end

  defp range_up(from, to, time_to_max, elapsed_time) do
    from + to - range_down(to, from, time_to_max, elapsed_time)
  end

  defp minutes(n), do: n * 60
  defp hours(n), do: minutes(n * 60)

  # Returns the number of seconds that have elapsed since time1
  defp seconds_since(time) do
    now = Timex.local
    (now.hour - time.hour) * 3600 +
    (now.minute - time.minute) * 60 +
    (now.second - time.second)
  end
end
