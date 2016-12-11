defmodule House.Mode do
  use GenServer

  require Logger


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  def start_link(), do:
    GenServer.start_link(__MODULE__, [], [name: __MODULE__])

  def check_sensors(), do:
    GenServer.cast(__MODULE__, :check_sensors)

  def auto(), do:
    GenServer.cast(__MODULE__, {:set, :auto})

  def manual(), do:
    GenServer.cast(__MODULE__, {:set, :manual})

  def away(), do:
    GenServer.cast(__MODULE__, {:set, :away})

  def get(), do:
    GenServer.call(__MODULE__, :get)

  def manual?(), do:
    get() == :manual

  def auto?(), do:
    get() == :auto

  def away?(), do:
    get() == :away


  # -------------------------------------------------------------------
  # Callbacks
  # -------------------------------------------------------------------

  def init(_) do
    Logger.info("Init mode controller")
    state = %{
      mode: :auto,
      lastset_change: current_timestamp(),
    }
    {:ok, state}
  end

  def handle_cast({:set, mode}, state) do
    state = %{state |
      mode: mode,
      lastset_change: current_timestamp(),
    }
    {:noreply, state}
  end
  def handle_cast(:check_sensors, %{mode: :away} = state) do
    House.Hue.geosensors()
    |> Enum.any?(&(&1["state"]["presence"]))
    |> if do
      if more_than_an_hour_since_set_to_away?(state) do
        {:noreply, %{state | mode: :auto}}
      else
        Logger.info("System is set to away mode, but sensors claim we are here. Ignore for now, since it was recently set to away")
        {:noreply, state}
      end
    else
      {:noreply, state}
    end
  end
  def handle_cast(:check_sensors, state), do:
    {:noreply, state}

  def handle_call(:get, _from, state) do
    {:reply, state.mode, state}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp current_timestamp(), do:
    :os.system_time(:seconds)

  defp more_than_an_hour_since_set_to_away?(state), do:
    current_timestamp() - state.lastset_change > 3600
end
