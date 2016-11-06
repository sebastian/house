defmodule House.Hue.Sensor do
  @moduledoc false

  @type t :: %__MODULE__{
    id: String.t,
    temperature: non_neg_integer,
    presence: boolean,
    battery: non_neg_integer,
    on: boolean,
    name: String.t,
    dark: boolean,
    daylight: boolean,
    lightlevel: non_neg_integer,
    lux: float,
    last_updated: String.t,
  }

  defstruct [
    id: "unknown",
    temperature: 0,
    presence: false,
    battery: 100,
    on: true,
    name: "unknown name",
    dark: false,
    daylight: false,
    lightlevel: 0,
    lux: 0,
    last_updated: "",
  ]

  alias House.Hue.Sensor


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  def from_data({id, sensor_data}) do
    default = %Sensor{
      id: id
    }
    Enum.reduce(sensor_data, default, &read_state/2)
  end


  # -------------------------------------------------------------------
  # Internal API
  # -------------------------------------------------------------------

  defp read_state(%{"type" => "ZLLTemperature"} = data, state) do
    %Sensor{state | temperature: data["state"]["temperature"]}
  end
  defp read_state(%{"type" => "ZLLPresence"} = data, state) do
    {:ok, timestamp} = Timex.parse(data["state"]["lastupdated"],
      "{YYYY}-{M}-{D}T{h24}:{m}:{s}")
    %Sensor{ state |
      presence: data["state"]["presence"],
      last_updated: timestamp,
      battery: data["config"]["battery"],
      on: data["config"]["on"],
      name: data["name"],
    }
  end
  defp read_state(%{"type" => "ZLLLightLevel"} = data, state) do
    lightlevel = data["state"]["lightlevel"]
    %Sensor{ state |
      dark: data["state"]["dark"],
      daylight: data["state"]["daylight"],
      lightlevel: lightlevel,
      lux: to_lux(lightlevel),
    }
  end

  defp to_lux(lightlevel), do: :math.pow(10, (lightlevel-1)/10000)
end
