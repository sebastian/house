defmodule House.PageController do
  use House.Web, :controller

  def index(conn, _params) do
    room_data = Poison.encode!(House.Hue.formatted_room_sensors())
    room_names = House.Presence.active_rooms() |> Enum.map(&(&1.name))
    render(conn, "index.html", room_names: room_names, room_data: room_data)
  end
end
