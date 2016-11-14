defmodule House.PageController do
  use House.Web, :controller

  def index(conn, _params) do
    rooms = House.Presence.active_rooms() |> Enum.map(&(&1.name))
    render(conn, "index.html", rooms: rooms)
  end
end
