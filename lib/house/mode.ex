defmodule House.Mode do
  use GenServer

  require Logger

  alias House.Hue


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  def start_link(), do:
    GenServer.start_link(__MODULE__, [], [name: __MODULE__])

  def check_sensors(), do:
    GenServer.cast(__MODULE__, :check_sensors)

  def get(), do:
    GenServer.call(__MODULE__, :get)

  Enum.each(~w(auto manual away presence_only), fn(mode) ->
    def unquote(:"#{mode}")() do
      GenServer.cast(__MODULE__, {:set, unquote(:"#{mode}")})
    end

    def unquote(:"#{mode}?")() do
      get() == unquote(:"#{mode}")
    end
  end)


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
    Logger.info("Setting state: #{inspect mode}")
    state = %{state |
      mode: mode,
      lastset_change: current_timestamp(),
    }
    {:noreply, state}
  end
  def handle_cast(:check_sensors, %{mode: :away} = state) do
    if Hue.home? and Hue.geohome? do
      Logger.info("We are at home and geofence also has triggered")
      if more_than_half_an_hour_since_set_to_away?(state) do
        Logger.info("Was set to away more than 30 minutes ago. Going back to auto")
        House.UpdatesChannel.mode_update("auto")
        {:noreply, %{state | mode: :auto}}
      else
        Logger.info("System is set to away mode, but sensors claim we are here. Ignore for now, since it was recently set to away")
        {:noreply, state}
      end
    else
      {:noreply, state}
    end
  end
  def handle_cast(:check_sensors, state) do
    # The homeaway toggle trumps everything else.
    # It can force us to away.
    if not Hue.home? do
      Logger.info("Hue set to AWAY mode")
      state = %{state |
        mode: :away,
        # We fake the timestamp to be in the past, so we can quickly
        # get out of the away mode again :)
        lastset_change: current_timestamp() - 2 * 60 * 60
      }
      House.UpdatesChannel.mode_update("away")
      {:noreply, state}
    else
      {:noreply, state}
    end
  end

  def handle_call(:get, _from, state) do
    {:reply, state.mode, state}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp current_timestamp(), do:
    :os.system_time(:seconds)

  defp more_than_half_an_hour_since_set_to_away?(state), do:
    current_timestamp() - state.lastset_change > 1800
end
