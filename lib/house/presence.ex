defmodule House.Presence do
  use GenServer

  require Logger


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  def start_link() do
    GenServer.start_link(__MODULE__, [], [name: __MODULE__])
  end

  def update(readings) do
    GenServer.cast(__MODULE__, {:update, readings})
  end

  def active_rooms() do
    GenServer.call(__MODULE__, :active_rooms)
  end


  # -------------------------------------------------------------------
  # Callbacks
  # -------------------------------------------------------------------

  def init(_) do
    Logger.info("Init presence")
    state = %{
      active_rooms: [%{name: "Unknown location"}],
    }
    {:ok, state}
  end

  def handle_call(:active_rooms, _from, state) do
    {:reply, state.active_rooms, state}
  end

  def handle_cast({:update, sensor_readings}, state) do
    new_state = update_with_readings(state, sensor_readings)
    unless new_state.active_rooms == state.active_rooms do
      room_name = hd(new_state.active_rooms).name
      other_rooms = tl(new_state.active_rooms)
        |> Enum.map(& &1.name)
      House.UpdatesChannel.room_change(room_name, other_rooms)
    end
    {:noreply, new_state}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp update_with_readings(%{active_rooms: rooms} = state, readings) do
    active_rooms = readings
      |> Enum.filter(& &1.presence)
      |> Enum.map(&Map.take(&1, [:name, :last_updated]))
      |> Enum.sort_by(& &1.last_updated, &>=/2)
    case active_rooms do
      [] -> %{state | active_rooms: [hd(rooms)]}
      _rooms -> %{state | active_rooms: active_rooms}
    end
  end
end
