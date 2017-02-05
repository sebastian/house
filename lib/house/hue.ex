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

  def rooms(hue_reading), do: rooms_from_state(hue_reading, sensors_from_reading(hue_reading))

  def home?(reading), do: sensor_presence(reading, ~r/HOMEAWAY/i)

  def geohome?(reading), do: sensor_presence(reading, ~r/geo/i)

  def formatted_room_sensors(hue_reading), do:
    hue_reading
    |> rooms()
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
      username: hue_username(),
      endpoint: "",
      last_reading: %{},

      # Lights that should be controlled.
      scheduled_primary_lights: [],
      scheduled_secondary_lights: [],
    }
    |> find_endpoint()
    |> sync_read_hue()
    {:ok, state}
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
    {:reply, sensors_from_reading(state.last_reading), state}
  end

  def handle_info({:new_hue_reading, reading}, state) do
    {:noreply, %{state | last_reading: reading}}
  end

  def handle_info(:read_hue, state) do
    read_hue(state)
    schedule_next_hue_check()
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
    timeout = if slow_mode?(), do: :timer.seconds(10), else: :timer.seconds(1)
    :timer.send_after(timeout, :read_hue)
  end

  defp slow_mode?(), do:
    House.Mode.manual?() or House.Mode.away?()


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

  defp sensors_from_reading(hue_state), do:
    hue_state["sensors"]
    |> Enum.map(fn({_, sensor}) -> sensor end)
    |> Sensors.from_data()

  defp rooms_from_state(hue_state, sensors), do:
    hue_state["groups"]
    |> Rooms.from_data(sensors)

  defp read_hue(state) do
    pid = self()
    Task.start(fn() ->
      %{last_reading: reading} = sync_read_hue(state)
      send(pid, {:new_hue_reading, reading})

      if state.last_reading != reading do
        reading
        |> formatted_room_sensors()
        |> House.UpdatesChannel.sensor_data_update()

        reading
        |> sensors_from_reading()
        |> Presence.update()

        reading
        |> House.Mode.account_for_reading()

        reading
        |> House.Lights.account_for_reading()
      end
    end)
  end

  defp sync_read_hue(state) do
    start_time = Timex.now()
    url = "http://#{state.endpoint}/api/#{state.username}/"
    response = Poison.decode!(HTTPoison.get!(url).body)
    duration = Timex.diff(Timex.now(), start_time, :milliseconds) / 1000
    Logger.debug("Read Hue state (took #{duration} seconds)")
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

  defp hue_username(), do:
    Application.get_env(:house, House.Endpoint)
    |> Keyword.get(:hue_username)

  defp sensor_presence(reading, match), do:
    reading["sensors"]
    |> Enum.map(fn({_, data}) -> data end)
    |> Enum.filter(&(&1["modelid"] =~ match))
    |> Enum.any?(&(&1["state"]["presence"]))

  defp find_endpoint(state) do
    case HTTPoison.get("https://www.meethue.com/api/nupnp") do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        [station | _] = Poison.decode!(body)
        endpoint = station["internalipaddress"]
        Logger.info("Using '#{endpoint}' as the endpoint")
        %{state | endpoint: endpoint}
      reason ->
        raise "Could not discover the hue endpoint (#{inspect reason})"
    end
  end
end
