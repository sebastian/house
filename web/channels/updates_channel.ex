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
    Logger.info("Broadcasting new room: #{room_name}")
    message = %{
      name: room_name,
      potential_rooms: other_rooms
    }
    House.Endpoint.broadcast("room:presence", "new_room", message)
  end


  # -------------------------------------------------------------------
  # Phoenix.Channel callback functions
  # -------------------------------------------------------------------

  @doc false
  def join("room:presence", _, socket) do
    {:ok, %{}, socket}
  end

  @doc false
  def terminate(_reason, socket) do
    Logger.info("Person left web interface")
    {:ok, socket}
  end

  @doc false
  def handle_in(event, _payload, socket) do
    Logger.warn("unknown event #{event}")
    {:noreply, socket}
  end

  @doc false
  def handle_info(message, socket) do
    Logger.info("unhandled info #{inspect(message)}")
    {:noreply, socket}
  end
end
