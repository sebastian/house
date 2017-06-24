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
  12: Mirror mid
  13: Mirror left
  14: Mirror bathtub
  15: Kitchen spot right
  16: Kitcehn spot middle
  17: Kitchen spot left
  """

  require Logger

  @kitchen ~w(6 10 11 15 16 17)
  @kitchen_secondary ~w(10 11 15 16 17)
  @living_room ~w(1 7 8 9)
  @living_room_secondary ~w(7 8 9)
  @living_room_primary ~w(1)
  @living_room_bottom_side_lamp ~w(7)
  @hallway ~w(3)
  @bedroom ~w(2)


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  def on(lamp) do
    if House.Mode.away?() do
      false
    else
      case brightness(lamp) do
        0 -> false
        _ -> true
      end
    end
  end

  def brightness(lamp), do: brightness(program(), lamp)

  def temperature(lamp), do: temperature(program(), lamp)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  # During the morning we want nice and bright lights
  def brightness(:morning, %{lamp: lamp}) when lamp in @living_room_secondary, do: range_up(50, 254, minutes(10), seconds_since(~T[06:00:00]))
  def brightness(:morning, %{lamp: lamp}) when lamp in @living_room, do: 0
  def brightness(:morning, _), do: range_up(50, 254, minutes(10), seconds_since(~T[06:00:00]))

  # During the day we want nice and bright lights
  def brightness(:day, _), do: 254

  # During the afternoon we want the light to slowly get dimmer, preparing for post-work time
  def brightness(:afternoon, _), do: range_down(254, 200, hours(3), seconds_since(~T[15:00:00]))

  # During the evening we want to slowly dim the secondary lights
  def brightness(:evening, %{lamp: lamp}) when lamp in @kitchen_secondary, do: range_down(200, 150, minutes(30), seconds_since(~T[18:00:00]))
  def brightness(:evening, %{lamp: lamp}) when lamp in @kitchen, do: 200
  def brightness(:evening, %{lamp: lamp}) when lamp in @living_room_secondary, do: range_down(200, 150, minutes(30), seconds_since(~T[18:00:00]))
  def brightness(:evening, %{lamp: lamp}) when lamp in @bedroom, do: range_down(200, 150, hours(1), seconds_since(~T[18:00:00]))
  def brightness(:evening, _), do: range_down(200, 150, hours(3), seconds_since(~T[18:00:00]))

  # Night
  def brightness(:night, %{primary?: false}), do: 0
  def brightness(:night, %{lamp: lamp}) when lamp in @bedroom, do: range_down(150, 0, minutes(1), seconds_since(~T[22:00:00]))
  def brightness(:night, %{lamp: lamp}) when lamp in @living_room_bottom_side_lamp, do: range_down(150, 50, minutes(5), seconds_since(~T[22:00:00]))
  def brightness(:night, %{lamp: lamp}) when lamp in @living_room, do: 0
  def brightness(:night, _), do: range_down(150, 50, minutes(5), seconds_since(~T[22:00:00]))

  # During the morning we want nice and bright lights
  def temperature(:morning, %{lamp: lamp}) when lamp in @kitchen, do: range_down(450, 153, minutes(10), seconds_since(~T[06:00:00]))
  def temperature(:morning, _), do: range_down(450, 250, minutes(10), seconds_since(~T[06:00:00]))
  # During the day we want nice and cool lights, but they should slowly become warmer
  def temperature(:day, %{lamp: lamp}) when lamp in @kitchen, do: 153
  def temperature(:day, _), do: 250
  def temperature(:afternoon, %{lamp: lamp}) when lamp in @kitchen, do: range_up(153, 300, hours(3), seconds_since(~T[13:00:00]))
  def temperature(:afternoon, _), do: range_up(250, 450, hours(3), seconds_since(~T[13:00:00]))
  # During the evening we want the lights to get warm and cozy!
  def temperature(:evening, %{lamp: lamp}) when lamp in @kitchen, do: range_up(300, 450, hours(1), seconds_since(~T[18:00:00]))
  def temperature(:evening, _), do: 450
  def temperature(:night, _), do: 450

  defp program() do
    if House.Mode.night? do
      :night
    else
      current_hour = Timex.local.hour
      cond do
        current_hour >= 6 and current_hour < 9 -> :morning
        current_hour >= 9 and current_hour < 13 -> :day
        current_hour >= 13 and current_hour < 18 -> :afternoon
        current_hour >= 18 and current_hour < 22 -> :evening
        current_hour >= 22 and current_hour <= 24 -> :night
        current_hour >= 0 and current_hour <= 6 -> :night
      end
    end
  end

  defp range_down(from, _to, _time_to_max, elapsed_time) when elapsed_time < 0, do: from
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
