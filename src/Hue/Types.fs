namespace Hue

module Types =
    open System

    type Bri = int
    type Ct = int
    
    // We want the brightness and color tone both when
    // turning a room on and off. Otherwise we get an
    // odd flickering of color.
    type LightState
        = Off of Bri * Ct 
        | On of Bri * Ct

    type Light = {
        Id: string
        Name: string
        State: LightState
        Reachable: bool
        MaxCt: int
        MinCt: int
    }

    type SensorGeofence = {
        Id: string
        Presence: bool
        Name: string
        LastUpdated: DateTime
    }

    // ZLLPresence
    type RoomPresence = {
        Name: string
        Presence: bool
        Battery: int
        LastUpdated: DateTime
        Reachable: bool
        UniqueId: string
    }

    // ZLLTemperature
    type RoomTemperature = {
        Temperature: int option
        UniqueId: string
    }

    // ZLLLightLevel
    type RoomLightLevel = {
        LightLevel: int option
        Dark: bool option
        DayLight: bool option
        UniqueId: string
    }

    type RoomSensor = {
        Name: string
        Battery: int
        LastUpdated: DateTime
        Reachable: bool
        Presence: bool // From ZLLPresence
        Temperature: int option // From ZLLTemperature
        LightLevel: int option // From ZLLLightLevel
        Dark: bool option
        DayLight: bool option
    }

    type Sensor =
        | Geofence of SensorGeofence
        | SensorPresence of RoomPresence
        | SensorTemperature of RoomTemperature
        | SensorLightLevel of RoomLightLevel

    type Room = {
        Id: string
        Name: string
        Lights: Light list
        RoomSensor: RoomSensor
        LightState: LightState option
        MaxCt: int
        MinCt: int
    }

    type Reading = {
        Lights: Light list
        RoomSensors: RoomSensor list
        GeoPresent: bool
        Rooms: Room list
    }

    type Mode
        = Automatic
        | Manual

    type State = {
        LastReading: Reading option
        Mode: Mode
    }