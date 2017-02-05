defmodule House.Lights do
  use GenServer

  require Logger

  alias House.{Presence, Hue, Scene}
  alias House.Hue.Room

  @neighbouring_rooms %{
    "Bedroom" => ["Living room"],
    "Living room" => ["Bedroom", "Hallway"],
    "Hallway" => ["Living room", "Kitchen", "Bathroom"],
    "Kitchen" => ["Hallway"],
    "Bathroom" => ["Hallway"],
  }

  @secondary_room_min_brightness 50
  @min_secondary_fade_out_in_seconds 5 * 60
  @min_secondary_remain_max_in_seconds 1 * 60


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  def start_link(), do:
    GenServer.start_link(__MODULE__, [], [name: __MODULE__])

  def account_for_reading(reading), do:
    GenServer.cast(__MODULE__, {:account_for_reading, reading})


  # -------------------------------------------------------------------
  # Callbacks
  # -------------------------------------------------------------------

  def init(_) do
    Logger.info("Init light control")
    {:ok, %{
      secondary_rooms: [],
      primary_rooms: [],
    }}
  end

  def handle_cast({:account_for_reading, reading}, %{secondary_rooms: secondary_rooms} = state) do
    rooms_by_name = reading
    |> Hue.rooms()
    |> by_name()

    primary_rooms = Presence.active_rooms()
      |> Enum.map(&Map.get(rooms_by_name, &1.name, nil))
      |> Enum.reject(& &1 == nil)

    primary_rooms_by_name = by_name(primary_rooms)
    secondary_rooms_by_name = by_name(secondary_rooms)

    updated_secondary_rooms = Enum.reduce(primary_rooms, %{}, fn(room, secondaries) ->
      # Returns the secondary rooms for a room (i.e. neighbouring rooms),
      # and sets the last_updated timestamp to that of the primary room.
      # Hence when I enter the living room, the neighbouring bedroom has the
      # `last_updated` timestamp of when the living room sensor triggered.
      # This way I can dim out the light in the bed room over time should it
      # turn out I didn't want to enter it
      secondaries_for_room(room, rooms_by_name)
      # Prefer the previously cached secondary room object, so the timestamp
      # previous timestamp remains intact.
      |> Enum.map(&Map.get(secondary_rooms_by_name, &1.name, &1))
      |> Enum.reduce(secondaries, &Map.put(&2, &1.name, &1))
    end)
    |> Map.values()
    |> Enum.flat_map(fn(room) ->
      case Map.get(primary_rooms_by_name, room.name) do
        nil -> [room]
        _ -> []
      end
    end)
    |> by_name()
    |> Map.values()

    # We adjust all lights. If there are no changes, then the system will
    # realise, and prevent the action from taking place.
    primary_rooms
    |> Enum.each(&adjust_primary_lights/1)
    secondary_rooms
    |> Enum.each(&adjust_secondary_lights/1)

    # Turn off the lights in all rooms not in use
    primary_rooms ++ updated_secondary_rooms
    |> Enum.map(& &1.name)
    |> Enum.reduce(rooms_by_name, &Map.delete(&2, &1))
    |> Map.values()
    |> Enum.filter(& &1.on)
    |> Enum.each(&turn_off_lights/1)

    state = %{state |
      primary_rooms: primary_rooms,
      secondary_rooms: updated_secondary_rooms,
    }
    {:noreply, state}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp secondaries_for_room(room, rooms_by_name) do
    Map.get(@neighbouring_rooms, room.name, [])
    |> Enum.map(&Map.get(rooms_by_name, &1, nil))
    |> Enum.reject(& &1 == nil)
    |> Enum.map(& %Room{&1 | last_updated: room.last_updated})
  end

  defp by_name(rooms), do:
    Enum.reduce(rooms, %{}, fn(room, map) -> Map.put(map, room.name, room) end)

  defp adjust_primary_lights(room) do
    Enum.each(room.lights, fn(light) ->
      action = %{"on" => Scene.on(%{lamp: light, primary?: true})}
      action = if House.Mode.presence_only?() do
        action
      else
        action
        |> Map.put("bri", Scene.brightness(%{lamp: light, primary?: true}))
        |> Map.put("ct", Scene.temperature(%{lamp: light, primary?: true}))
      end
      House.Hue.set_primary_light(light, action)
    end)
  end

  defp adjust_secondary_lights(room) do
    Enum.each(room.lights, fn(light) ->
      action = %{"on" => Scene.on(%{lamp: light, primary?: false})}
      action = if House.Mode.presence_only?() do
        action
      else
        action
        |> Map.put("bri", brightness(room, Scene.brightness(%{lamp: light, primary?: false})))
        |> Map.put("ct", Scene.temperature(%{lamp: light, primary?: false}))
        |> Map.put("transitiontime", 8)
      end
      House.Hue.set_secondary_light(light, action)
    end)
  end

  defp turn_off_lights(room) do
    action = %{
      "on" => false
    }
    Enum.each(room.lights, &House.Hue.set_secondary_light(&1, action))
  end

  defp brightness(room, max_brightness) do
    seconds = Timex.diff(Timex.now, room.last_updated, :seconds)
    timespan = @min_secondary_fade_out_in_seconds - @min_secondary_remain_max_in_seconds
    current_seconds = min(
      max(0, seconds - @min_secondary_remain_max_in_seconds),
      timespan
    )
    fraction_of_way_there = current_seconds / timespan
    # Over five minutes we dim secondary rooms from bri 100% down to
    # @secondary_room_min_brightness, linearly.
    # The first minute it remains fully bright.
    brightness_range = max_brightness - @secondary_room_min_brightness
    round(@secondary_room_min_brightness + brightness_range - (brightness_range * fraction_of_way_there))
  end
end
