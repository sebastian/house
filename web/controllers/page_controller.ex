defmodule House.PageController do
  use House.Web, :controller

  require Logger

  alias House.Hue


  # -------------------------------------------------------------------
  # Callbacks
  # -------------------------------------------------------------------

  def index(conn, _params) do
    room_data = Hue.last_reading()
    |> Hue.formatted_room_sensors()
    |> Poison.encode!()
    room_names = House.Presence.active_rooms()
    |> Enum.map(&(&1.name))
    render(conn, "index.html", room_names: room_names, room_data: room_data)
  end

  def off(conn, _params) do
    Logger.info("OFF-page was triggered")
    House.Mode.away()
    redirect(conn, to: "/is_off")
  end

  def is_off(conn, _params), do:
    conn
    |> put_layout(false)
    |> render("off.html")
end
