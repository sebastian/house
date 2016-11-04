defmodule House.Hue do
  use Application

  require Logger

  alias House.Hue.Sensors


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  def start_link() do
    GenServer.start_link(__MODULE__, [], [name: __MODULE__])
  end

  def last_reading() do
    GenServer.call(__MODULE__, :last_reading)
  end

  def lights() do
    GenServer.call(__MODULE__, :lights)
  end

  def sensors() do
    GenServer.call(__MODULE__, :sensors)
  end


  # -------------------------------------------------------------------
  # Callbacks
  # -------------------------------------------------------------------

  def init(_) do
    Logger.info("Init hue")
    send(self(), :read_hue)
    :timer.send_interval(:timer.seconds(5), :read_hue)
    state = %{
      username: System.get_env("HUE_USERNAME"),
      endpoint: "",
      last_reading: %{},
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

  def handle_call(:last_reading, _from, state) do
    {:reply, state.last_reading, state}
  end

  def handle_call(:lights, _from, state) do
    lights = state.last_reading["lights"]
      |> Enum.map(fn({_, light}) -> light end)
    {:reply, lights, state}
  end

  def handle_call(:sensors, _from, state) do
    sensor = state.last_reading["sensors"]
      |> Enum.map(fn({_, sensor}) -> sensor end)
      |> Sensors.from_data()
    {:reply, sensor, state}
  end

  def handle_info(:read_hue, state) do
    {:noreply, read_hue(state)}
  end


  # -------------------------------------------------------------------
  # Callbacks
  # -------------------------------------------------------------------

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
end
