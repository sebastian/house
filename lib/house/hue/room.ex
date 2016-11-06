defmodule House.Hue.Room do
  @moduledoc false

  alias House.Hue.{Room, Sensor}

  @type t :: %__MODULE__{
    id: String.t,
    name: String.t,
    lights: [String.t],
    last_updated: String.t,
    on: boolean,
    sensor: Sensor.t,
    bri: non_neg_integer,
    ct: non_neg_integer,
  }

  defstruct [
    id: "unknown",
    name: "unknown name",
    lights: [],
    last_updated: "unknown",
    on: false,
    sensor: %Sensor{},
    bri: 0,
    ct: 0,
  ]


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  def from_data({id, room_data}, sensors) do
    name = room_data["name"]
    sensor = sensor_for_room(sensors, name)
    %Room{
      id: id,
      name: room_data["name"],
      lights: room_data["lights"],
      last_updated: sensor.last_updated,
      sensor: sensor,
      on: room_data["action"]["on"],
      bri: room_data["action"]["bri"],
      ct: room_data["action"]["ct"],
    }
  end

  defp sensor_for_room(sensors, name), do:
    Enum.find(sensors, & &1.name == name)
end
