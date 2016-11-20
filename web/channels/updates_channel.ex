defmodule House.UpdatesChannel do
  @moduledoc """
  Main communication channel between web client and house system
  """
  use Phoenix.Channel
  require Logger


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Broadcast the most likely next room"
  @spec room_change(String.t, String.t) :: :ok
  def room_change(room_name, other_rooms) do
    Logger.debug("Broadcasting new room: #{room_name}")
    message = %{
      name: room_name,
      potential_rooms: other_rooms
    }
    House.Endpoint.broadcast("updates:all", "new_room", message)
  end

  @doc "Broadcast the state of rooms"
  @spec sensor_data_update(Map.t) :: :ok
  def sensor_data_update(sensor_data) do
    Logger.debug("Broadcasting sensor data")
    message = %{
      room_sensors: sensor_data,
    }
    House.Endpoint.broadcast("updates:all", "sensor_data_update", message)
  end


  # -------------------------------------------------------------------
  # Phoenix.Channel callback functions
  # -------------------------------------------------------------------

  @doc false
  def join("updates:" <> _, _, socket) do
    {:ok, %{}, socket}
  end

  @doc false
  def terminate(_reason, socket) do
    Logger.info("Person left web interface")
    {:ok, socket}
  end

  @doc false
  def handle_in("set_mode", mode, socket) do
    set_mode(mode)
    House.Endpoint.broadcast("updates:all", "new_mode", %{mode: mode})
    {:noreply, socket}
  end

  def handle_in(event, _payload, socket) do
    Logger.warn("unknown event #{event}")
    {:noreply, socket}
  end

  @doc false
  def handle_info(message, socket) do
    Logger.info("unhandled info #{inspect(message)}")
    {:noreply, socket}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp set_mode("manual"), do: House.Mode.manual()
  defp set_mode("auto"), do: House.Mode.auto()
  defp set_mode(other), do: Logger.warn("Uknown mode: #{inspect other}")
end
