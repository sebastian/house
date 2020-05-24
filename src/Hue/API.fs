namespace Hue

module API =
    open System
    open FSharp.Data
    open Types

    type DiscoveryProvider = JsonProvider<"""[{"id":"001788fffe257327","internalipaddress":"10.0.0.102"}]""">
    type StateProvider = JsonProvider<"samples/all.json">
    type LightProvider = JsonProvider<"samples/lights.json", SampleIsList=true>
    type SensorProvider = JsonProvider<"samples/sensors.json", SampleIsList=true>
    type GroupProvider = JsonProvider<"samples/groups.json", SampleIsList=true>
    type ActionProvider = JsonProvider<"samples/action-group.json", RootName="action">
    
    let private defaultDateTime placement =
        function
        | None ->
            let datetime = DateTime.UtcNow.Add(TimeSpan.FromDays(-0.5))
            printfn "No DateTime value given for %s, defaulting to half a day ago: %A" placement datetime
            datetime
        | Some dateTime -> dateTime

    let getIp () = async {
        let! ipRecords = DiscoveryProvider.AsyncLoad "http://www.meethue.com/api/nupnp"
        let ips = ipRecords |> Array.map (fun entry -> entry.Internalipaddress)
        return
            if Array.length ips > 0
            then Some ips.[0]
            else None
    }

    let private parseLight (id, jsonValue) : Light =
        let light = LightProvider.Parse (jsonValue.ToString())
        let state =
            match light.State.On with
            | false ->  Off
            | true ->
                On (light.State.Bri, light.State.Ct)
        {
            Id = id
            Name = light.Name
            State = state
            Reachable = light.State.Reachable
            MaxCt = light.Capabilities.Control.Ct.Max
            MinCt = light.Capabilities.Control.Ct.Min
        }

    let private parseSensor (id, jsonValue) =
        let sensor = SensorProvider.Parse (jsonValue.ToString())
        match sensor.Type with
        | "ZLLPresence" ->
            let sensor: RoomPresence = {
                Name = sensor.Name
                Presence = sensor.State.Presence |> Option.defaultValue false
                Battery = sensor.Config.Battery |> Option.defaultValue 50
                LastUpdated = sensor.State.Lastupdated.DateTime |> defaultDateTime (sprintf "ZLLPresence sensor %s" sensor.Name)
                Reachable = sensor.Config.Reachable |> Option.defaultValue false
                UniqueId = sensor.Uniqueid |> Option.defaultValue "MISSING ID"
            }
            sensor |> SensorPresence |> Some

        | "ZLLTemperature" ->
            let sensor: RoomTemperature = {
                Temperature = sensor.State.Temperature
                UniqueId = sensor.Uniqueid |> Option.defaultValue "MISSING ID"
            }
            sensor |> SensorTemperature |> Some

        | "ZLLLightLevel" ->
            let sensor: RoomLightLevel = {
                LightLevel = sensor.State.Lightlevel
                Dark = sensor.State.Dark
                DayLight = sensor.State.Daylight
                UniqueId = sensor.Uniqueid |> Option.defaultValue "MISSING ID"
            }
            sensor |> SensorLightLevel |> Some

        | "Geofence" ->
            let sensor: SensorGeofence = {
                Id = id
                Name = sensor.Name
                Presence = sensor.State.Presence |> Option.defaultValue false
                LastUpdated = sensor.State.Lastupdated.DateTime |> defaultDateTime "geofence"
            }
            sensor |> Geofence |> Some
        | _ -> None

    let private sharedLightState (lights: Light list) =
        lights
        |> List.filter (fun l -> l.Reachable)
        |> List.map (fun l -> l.State)
        |> List.groupBy id
        |> function
           | [(lightState, _)] -> Some lightState // All agree, and share the same light setting
           | _ -> None

    let private parseRoom (lights: Light list) (roomSensors: RoomSensor list) (id, jsonValue) =
        let room = GroupProvider.Parse (jsonValue.ToString())
        match room.Type with
        | "Room" ->
            match roomSensors |> List.tryFind (fun sensor -> sensor.Name = room.Name) with
            | Some sensor ->
                let lightsInRoom
                    = (room.Lights: int [])
                    |> Array.toList
                    |> List.choose (fun lightId ->
                        lights |> List.tryFind (fun light -> light.Id = string lightId)
                    )
                Some {
                    Id = id
                    Name = room.Name
                    RoomSensor = sensor
                    LightState = sharedLightState lightsInRoom
                    Lights = lightsInRoom
                    MaxCt = lightsInRoom |> List.map (fun l -> l.MaxCt) |> List.min
                    MinCt = lightsInRoom |> List.map (fun l -> l.MinCt) |> List.max
                }
            | None -> None
        | _ -> None

    let private extractCommonPartFromUid (uid: String) =
        let parts = uid.Split [|'-'|]
        parts.[0]

    let private fuseSensors (_sharedId, sensors: Sensor list) : RoomSensor =
        let presence = sensors |> List.choose (function | SensorPresence s -> Some s | _ -> None) |> List.head
        let temperature = sensors |> List.choose (function | SensorTemperature s -> Some s | _ -> None) |> List.head
        let lightLevel = sensors |> List.choose (function | SensorLightLevel s -> Some s | _ -> None) |> List.head
        {
            Name = presence.Name
            Battery = presence.Battery
            LastUpdated = presence.LastUpdated
            Reachable = presence.Reachable
            Presence = presence.Presence
            Temperature = temperature.Temperature
            LightLevel = lightLevel.LightLevel
            Dark = lightLevel.Dark
            DayLight = lightLevel.DayLight
        }

    let getState ip username = async {
        let url = sprintf "http://%s/api/%s" ip username
        let! state = StateProvider.AsyncLoad url
        let lights =
            state.Lights.JsonValue.Properties()
            |> Array.map parseLight
            |> Array.toList
        let sensors =
            state.Sensors.JsonValue.Properties()
            |> Array.map parseSensor
            |> Array.toList
            |> List.choose id
        let fusedRoomSensors =
            sensors
            |> List.filter (
                function
                | Geofence _ -> false
                | _ -> true)
            |> List.groupBy (
                function
                | SensorPresence s -> s.UniqueId |> extractCommonPartFromUid
                | SensorTemperature s -> s.UniqueId |> extractCommonPartFromUid
                | SensorLightLevel s -> s.UniqueId |> extractCommonPartFromUid
                | _ -> failwith "Should have filtered out other sensors")
            |> List.map fuseSensors
        let geoPresent =
            sensors
            |> List.fold (fun acc sensor ->
                match sensor with
                | Geofence s -> acc || s.Presence
                | _ -> acc) false
        let rooms =
            state.Groups.JsonValue.Properties()
            |> Array.map (parseRoom lights fusedRoomSensors)
            |> Array.toList
            |> List.choose id
        return {
            Lights = lights
            RoomSensors = fusedRoomSensors
            GeoPresent = geoPresent
            Rooms = rooms
        }
    }

    let setState ip username (group: Room) (state: LightState) = async {
        let action =
            match state with
            | Off -> ActionProvider.Action (false, 200, 200)
            | On (bri, ct) -> ActionProvider.Action (true, bri, ct)
        let url = sprintf "http://%s/api/%s/groups/%s/action" ip username group.Id
        return! action.JsonValue.RequestAsync (url, "PUT")
    }