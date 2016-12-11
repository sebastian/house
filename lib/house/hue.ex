defmodule House.Hue do
  use GenServer

  require Logger

  alias House.Hue.{Sensors, Rooms}
  alias House.Presence


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  def start_link(), do:
    GenServer.start_link(__MODULE__, [], [name: __MODULE__])

  def set_primary_light(light, action), do:
    GenServer.cast(__MODULE__, {:schedule_primary_light, light, action})

  def set_secondary_light(light, action), do:
    GenServer.cast(__MODULE__, {:schedule_secondary_light, light, action})

  def last_reading(), do:
    GenServer.call(__MODULE__, :last_reading)

  def lights(), do:
    GenServer.call(__MODULE__, :lights)

  def sensors(), do:
    GenServer.call(__MODULE__, :sensors)

  def rooms(), do:
    GenServer.call(__MODULE__, :rooms)

  def geosensors(sensor_data \\ last_reading()), do:
    sensor_data["sensors"]
    |> Enum.map(fn({_, data}) -> data end)
    |> Enum.filter(&(&1["modelid"] =~ ~r/geo/i))

  def formatted_room_sensors(sensor_data \\ rooms()), do:
    sensor_data
    |> Enum.map(&(%{
      name: &1.name,
      temperature: &1.sensor.temperature / 100,
      lux: round(&1.sensor.lux),
    }))
    |> Enum.sort_by(&(&1.name))


  # -------------------------------------------------------------------
  # Callbacks
  # -------------------------------------------------------------------

  def init(_) do
    Logger.info("Init hue")
    send(self(), :read_hue)
    :timer.send_after(100, :update_lights)
    state = %{
      username: System.get_env("HUE_USERNAME"),
      endpoint: "",
      last_reading: %{},

      # Lights that should be controlled.
      scheduled_primary_lights: [],
      scheduled_secondary_lights: [],
    }
    case find_endpoint() do
      {:ok, endpoint} ->
        Logger.info("Using '#{endpoint}' as the endpoint")
        {:ok, %{state | endpoint: endpoint}}
      :error ->
        Logger.error("Can't find Hue base station")
        :error
    end
  end

  def handle_cast({:schedule_primary_light, new_light, action}, state) do
    light_schedule = adapt_if_not_redundant(state.scheduled_primary_lights, new_light, action, state)
    new_secondary_schedule = state.scheduled_secondary_lights
      |> Enum.reject(fn({light, _}) -> light == new_light end)
    state = %{state |
      scheduled_primary_lights: light_schedule,
      scheduled_secondary_lights: new_secondary_schedule
    }
    {:noreply, state}
  end

  def handle_cast({:schedule_secondary_light, light, action}, state) do
    light_schedule = adapt_if_not_redundant(state.scheduled_secondary_lights, light, action, state)
    {:noreply, %{state | scheduled_secondary_lights: light_schedule}}
  end

  def handle_call(:last_reading, _from, state) do
    {:reply, state.last_reading, state}
  end

  def handle_call(:lights, _from, state) do
    lights = state.last_reading["lights"]
      |> Enum.map(fn({_, light}) -> light end)
    {:reply, lights, state}
  end

  def handle_call(:sensors, _from, state) do
    {:reply, sensors_from_state(state), state}
  end

  def handle_call(:rooms, _from, state) do
    sensors = sensors_from_state(state)
    {:reply, rooms_from_state(state, sensors), state}
  end

  def handle_info(:read_hue, state) do
    state = read_hue(state)
    schedule_next_hue_check()
    state
    |> sensors_from_state()
    |> Presence.update()
    House.Lights.check_sensors()
    House.Mode.check_sensors()
    {:noreply, state}
  end

  def handle_info(:update_lights, state) do
    state = run_light_schedules(state)
    :timer.send_after(100, :update_lights)
    {:noreply, state}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp schedule_next_hue_check() do
    timeout = if House.Mode.auto?(), do: :timer.seconds(1), else: :timer.seconds(10)
    :timer.send_after(timeout, :read_hue)
  end

  defp run_light_schedules(%{scheduled_primary_lights: [light | lights]} = state) do
    set_light(light, state)
    %{state | scheduled_primary_lights: lights}
  end
  defp run_light_schedules(%{scheduled_secondary_lights: []} = state), do: state
  defp run_light_schedules(%{scheduled_secondary_lights: [light | lights]} = state) do
    set_light(light, state)
    %{state | scheduled_secondary_lights: lights}
  end

  defp remove_redundant_settings(action, name, state) do
    light = get_light(name, state)
    action
    |> Enum.reject(fn({key, val}) -> light["state"][key] == val end)
    |> Enum.into(%{})
  end

  defp get_light(light_name, state) do
    state.last_reading["lights"]
    |> Enum.find_value(fn({name, light}) ->
      if name == light_name do
        light
      else
        nil
      end
    end)
  end

  defp set_light({light, action}, state) do
    if House.Mode.manual?() do
      Logger.debug("Ignoring light event (mode: #{House.Mode.get()})")
    else
      Task.start(fn() ->
        start_time = Timex.now()
        url = "http://#{state.endpoint}/api/#{state.username}/lights/#{light}/state"
        case HTTPoison.put(url, Poison.encode!(action)) do
          {:ok, _} -> :ok
          {:error, reason} ->
            Logger.debug("Failed setting light #{light} (action: #{inspect action}): #{inspect reason}")
        end
        duration = Timex.diff(Timex.now(), start_time, :milliseconds) / 1000
        Logger.debug("Light: #{light}, action: #{inspect action} (took #{duration} seconds)")
      end)
    end
  end

  defp sensors_from_state(%{last_reading: last_reading}), do:
    last_reading["sensors"]
    |> Enum.map(fn({_, sensor}) -> sensor end)
    |> Sensors.from_data()

  defp rooms_from_state(%{last_reading: last_reading}, sensors), do:
    last_reading["groups"]
    |> Rooms.from_data(sensors)

  defp find_endpoint() do
    case HTTPoison.get("https://www.meethue.com/api/nupnp") do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        [station | _] = Poison.decode!(body)
        {:ok, station["internalipaddress"]}
      _ -> :error
    end
  end

  defp read_hue(state) do
    start_time = Timex.now()
    url = "http://#{state.endpoint}/api/#{state.username}/"
    response = Poison.decode!(HTTPoison.get!(url).body)
    duration = Timex.diff(Timex.now(), start_time, :milliseconds) / 1000
    Logger.debug("Read Hue state (took #{duration} seconds)")
    # In case there has been a change, we upadte the web clients
    if state.last_reading != response do
      Task.start(fn() ->
        House.UpdatesChannel.sensor_data_update(formatted_room_sensors())
      end)
    end
    %{state | last_reading: response}
  end

  defp adapt_if_not_redundant(schedule, name, action, state) do
    reduced_action = remove_redundant_settings(action, name, state)
    case Map.keys(reduced_action) do
      # If the only thing we should do is set a transition time,
      # then that's not worth a call
      ["transitiontime"] -> schedule
      # No need for an empty schedule
      [] -> schedule
      _ -> adapt_light_schedule(schedule, name, reduced_action)
    end
  end

  defp adapt_light_schedule(schedule, new_light, action) do
    new_schedule = {new_light, action}
    case Enum.find(schedule, nil, fn({light, _}) -> new_light == light end) do
      nil ->
        # Light isn't scheduled, so we add it to the schedule
        schedule ++ [new_schedule]
      _ ->
        # Light exist, so we replace the schedule, inline
        Enum.map(schedule, fn({light, _} = schedule) ->
          if light == new_light do
            new_schedule
          else
            schedule
          end
        end)
    end
  end
end
