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


  # -------------------------------------------------------------------
  # Callbacks
  # -------------------------------------------------------------------

  def init(_) do
    Logger.info("Init hue")
    send(self(), :read_hue)
    :timer.send_interval(:timer.seconds(2), :read_hue)
    :timer.send_interval(200, :update_lights)
    state = %{
      username: System.get_env("HUE_USERNAME"),
      endpoint: "",
      last_reading: %{},

      # Lights that should be controlled.
      scheduled_secondary_lights: [],
      scheduled_primary_lights: [],
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
    state
    |> sensors_from_state()
    |> Presence.update()
    {:noreply, read_hue(state)}
  end

  def handle_info(:update_lights, state) do
    state = case state.scheduled_primary_lights do
      [] ->
        case state.scheduled_secondary_lights do
          [] -> state
          [light | lights] ->
            set_light(light, state)
            %{state | scheduled_secondary_lights: lights}
        end
      [light | lights] ->
        set_light(light, state)
        %{state | scheduled_primary_lights: lights}
    end
    {:noreply, state}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp remove_redundant_settings(action, name, state) do
    light = get_light(name, state)
    action
    |> Enum.reject(fn({key, val}) -> light["state"][key] == val end)
    |> Enum.into(%{})
    |> Map.update("transitiontime", 10, & &1)
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
    Logger.info("Light: #{light}, action: #{inspect action}")
    url = "http://#{state.endpoint}/api/#{state.username}/lights/#{light}/state"
    case HTTPoison.put(url, Poison.encode!(action)) do
      {:ok, _} -> :ok
      {:error, reason} ->
        Logger.info("Failed setting light #{light} (action: #{inspect action}): #{inspect reason}")
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
    url = "http://#{state.endpoint}/api/#{state.username}/"
    response = Poison.decode!(HTTPoison.get!(url).body)
    %{state | last_reading: response}
  end

  defp adapt_if_not_redundant(schedule, name, action, state) do
    reduced_action = remove_redundant_settings(action, name, state)
    if length(Map.keys(reduced_action)) == 1 do
      schedule
    else
      adapt_light_schedule(schedule, name, reduced_action)
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
