defmodule House.Scene do
  @moduledoc """
  The scene is determined by the time of day.
  Based on a scene certain colour temperatures,
  and brightness values are chosen.

  Scenes might also use different subsets of lights.

  Lamps
  1: Sofa - living room
  2: Ceiling lamp - bedroom
  3: Ceiling lamp - hallway
  4: By the sink - bathroom
  5: By the shower - bathroom
  6: Over the kitchen table - kitchen
  7: Living room side lamp bottom
  8: Living rood side lamp top
  9: Living room side lamp mid
  10: By the coffee maker
  11: Kitchen by the oven
  """

  require Logger

  @kitchen ~w(6 10 11)
  @kitchen_secondary ~w(10 11)
  @living_room ~w(1 7 8 9)
  @living_room_secondary ~w(7 8 9)
  @hallway ~w(3)
  @bedroom ~w(2)


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  def on(light) do
    if House.Mode.away?() do
      false
    else
      case brightness(light) do
        0 -> false
        _ -> true
      end
    end
  end

  def brightness(light), do: brightness(program(), light)

  def temperature(light), do: temperature(program(), light)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  # During the morning we want nice and bright lights
  def brightness(:morning, lamp) when lamp in @living_room_secondary, do: range_up(50, 254, minutes(10), seconds_since(~T[06:00:00]))
  def brightness(:morning, lamp) when lamp in @living_room, do: 0
  def brightness(:morning, _), do: range_up(50, 254, minutes(10), seconds_since(~T[06:00:00]))
  # During the day we want nice and bright lights
  def brightness(:day, lamp) when lamp in @living_room, do: 0
  def brightness(:day, _), do: 254
  # During the afternoon we want the light to slowly get dimmer, preparing for post-work time
  def brightness(:afternoon, lamp) when lamp in @living_room, do: range_up(0, 200, hours(3), seconds_since(~T[15:00:00]))
  def brightness(:afternoon, _), do: range_down(254, 200, hours(3), seconds_since(~T[15:00:00]))
  # During the evening we want to slowly dim the secondary lights
  def brightness(:evening, lamp) when lamp in @kitchen_secondary, do: range_down(200, 80, minutes(30), seconds_since(~T[18:00:00]))
  def brightness(:evening, lamp) when lamp in @kitchen, do: 200
  def brightness(:evening, lamp) when lamp in @living_room_secondary, do: range_down(200, 30, minutes(30), seconds_since(~T[18:00:00]))
  def brightness(:evening, lamp) when lamp in @bedroom, do: range_down(200, 50, hours(1), seconds_since(~T[18:00:00]))
  def brightness(:evening, _), do: range_down(200, 150, hours(3), seconds_since(~T[18:00:00]))
  def brightness(:night, lamp) when lamp in @kitchen_secondary, do: range_down(80, 30, minutes(30), seconds_since(~T[23:00:00]))
  def brightness(:night, lamp) when lamp in @kitchen, do: range_down(200, 30, minutes(30), seconds_since(~T[23:00:00]))
  def brightness(:night, lamp) when lamp in @bedroom, do: 0
  def brightness(:night, _), do: range_down(150, 30, minutes(30), seconds_since(~T[23:00:00]))


  # During the morning we want nice and bright lights
  def temperature(:morning, _), do: range_down(450, 153, minutes(10), seconds_since(~T[06:00:00]))
  # During the day we want nice and cool lights, but they should slowly become warmer
  def temperature(:day, _), do: 153
  def temperature(:afternoon, _), do: range_up(153, 250, hours(2), seconds_since(~T[16:00:00]))
  # During the evening we want the lights to get warm and cozy!
  def temperature(:evening, _), do: range_up(250, 450, minutes(5), seconds_since(~T[18:00:00]))
  def temperature(:night, _), do: 500

  defp program() do
    current_hour = Timex.local.hour
    cond do
      current_hour >= 6 and current_hour < 9 -> :morning
      current_hour >= 9 and current_hour < 13 -> :day
      current_hour >= 13 and current_hour < 18 -> :afternoon
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
