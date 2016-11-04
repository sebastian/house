defmodule House.Hue.Sensors do
  @moduledoc false

  alias House.Hue.Sensor


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  def from_data(data) do
    data
    |> only_zll()
    |> Enum.group_by(&device_id/1)
    |> to_sensors()
  end


  # -------------------------------------------------------------------
  # Internal API
  # -------------------------------------------------------------------

  defp only_zll(data), do:
    Enum.filter(data, &String.starts_with?(&1["type"], "ZLL"))

  defp device_id(device), do:
    device["uniqueid"]
    |> String.split("-")
    |> hd()

  defp to_sensors(data), do:
    Enum.map(data, &Sensor.from_data/1)
end
