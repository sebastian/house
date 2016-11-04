defmodule House.Hue.Sensor do
  @moduledoc false


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  def from_data({id, sensor_data}) do
    default = %{
      id: id
    }
    state = Enum.reduce(sensor_data, default, &read_state/2)
  end


  # -------------------------------------------------------------------
  # Internal API
  # -------------------------------------------------------------------

  defp read_state(%{"type" => "ZLLTemperature"} = data, state) do
    state
    |> Map.put(:temperature, data["state"]["temperature"])
  end
  defp read_state(%{"type" => "ZLLPresence"} = data, state) do
    state
    |> Map.put(:presence, data["state"]["presence"])
    |> Map.put(:battery, data["config"]["battery"])
    |> Map.put(:on, data["config"]["on"])
    |> Map.put(:name, data["name"])
  end
  defp read_state(%{"type" => "ZLLLightLevel"} = data, state) do
    state
    |> Map.put(:dark, data["state"]["dark"])
    |> Map.put(:daylight, data["state"]["daylight"])
    |> Map.put(:lightlevel, data["state"]["lightlevel"])
  end
end
