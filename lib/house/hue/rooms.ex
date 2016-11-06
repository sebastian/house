defmodule House.Hue.Rooms do
  @moduledoc false

  alias House.Hue.Room


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  def from_data(data, sensors) do
    data
    |> only_rooms()
    |> to_rooms(sensors)
  end


  # -------------------------------------------------------------------
  # Internal API
  # -------------------------------------------------------------------

  defp to_rooms(data, sensors), do:
    Enum.map(data, &Room.from_data(&1, sensors))

  defp only_rooms(data), do:
    Enum.filter(data, fn({_, group}) -> group["type"] == "Room" end)
end
