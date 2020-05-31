module LightRules

open Hue.Types

module private Internals =
    open System

    let spanToSeconds (timeSpan: TimeSpan) =
        (timeSpan.Hours * 60 + timeSpan.Minutes) * 60 + timeSpan.Seconds

    let recentMotionReading (room: Room) =
        let now = DateTime.UtcNow
        let timeDiff = now - room.RoomSensor.LastUpdated
        let secondsThatCountAsRecent =
            DateTime.Now.TimeOfDay
            |> function
                | now when now > TimeSpan (22, 00, 00) || now < TimeSpan (6, 0, 0) -> 60
                | _ -> 20 * 60
        spanToSeconds timeDiff < secondsThatCountAsRecent

    let scaleDown startPoint endPoint totalSecondsInWindow remainingSeconds =
        let diff = startPoint - endPoint |> double
        let diffPerSecond = diff / (double totalSecondsInWindow)
        startPoint - diffPerSecond * (totalSecondsInWindow - remainingSeconds |> double) |> int

    let scaleUp startPoint endPoint totalSecondsInWindow remainingSeconds =
        let diff = endPoint - startPoint
        let diffPerSecond = diff / (double totalSecondsInWindow)
        endPoint - diffPerSecond * (totalSecondsInWindow - remainingSeconds |> double) |> int

    let secondsInWindow startHour endHour =
        let span = TimeSpan(endHour, 0, 0) - TimeSpan(startHour, 0, 0)
        span |> spanToSeconds

    let secondsUntil endHour =
        let span = TimeSpan(endHour, 0, 0) - DateTime.Now.TimeOfDay
        span |> spanToSeconds


    // The supported range is 0 to 254 - where 0 is not off, but the lowest possible light level
    let appropriateBrightness () =
        DateTime.Now.TimeOfDay
        |> function
            | now when now > TimeSpan (22, 00, 00) -> 10 // After bedtime
            | now when now > TimeSpan (20, 00, 00) -> scaleDown 150. 75. (secondsInWindow 20 22) (secondsUntil 22) // When winding down
            | now when now > TimeSpan (17, 00, 00) -> scaleDown 254. 150. (secondsInWindow 17 20) (secondsUntil 20) // When winding down
            | now when now > TimeSpan (10, 00, 00) -> 254
            | now when now > TimeSpan (6, 15, 00) -> scaleUp 150. 254. (secondsInWindow 6 10 - 60 * 15) (secondsUntil 10) // When winding down
            | now when now > TimeSpan (6, 00, 00) -> 150
            | _ -> 10 // Before 6:00

    // The supported range is 153 (6500K - cool) to 500 (2000K - warm) 
    let appropriateCt (room: Room) =
        let maxCt = min room.MaxCt 500
        let minCt = max room.MinCt 153
        DateTime.Now.TimeOfDay
        |> function
            | now when now > TimeSpan (22, 00, 00) -> maxCt // After bedtime
            | now when now > TimeSpan (20, 00, 00) -> scaleDown 400. (double maxCt)  (secondsInWindow 20 22) (secondsUntil 22) // When winding down
            | now when now > TimeSpan (17, 00, 00) -> scaleDown 300. 400. (secondsInWindow 17 20) (secondsUntil 20) // When winding down
            | now when now > TimeSpan (10, 00, 00) -> 300
            | now when now > TimeSpan (6, 15, 00) -> scaleUp (double minCt) 300. (secondsInWindow 6 10 - 60 * 15) (secondsUntil 10) // When winding down
            | now when now > TimeSpan (6, 00, 00) -> minCt
            | _ -> 10 // Before 6:00

    let isBedroom room = room.Name = "Bedroom"

    let allowedOnAsPrimary room =
        DateTime.Now.TimeOfDay
        |> function
            | now when now > TimeSpan (22, 00, 00) || now < TimeSpan (6, 0, 0) -> not (isBedroom room)
            | _ -> true

    let allowedOnAsSecondary () =
        DateTime.Now.TimeOfDay
        |> function
            | now when now > TimeSpan (22, 00, 00) || now < TimeSpan (6, 0, 0) -> false
            | _ -> true

    let appropriateLightSettings mode room =
        mode (appropriateBrightness (), appropriateCt room)

    let appropriateLightSettingsForSecondary room =
        let reducedBrightness = appropriateBrightness () / 2 |> min 10
        On (reducedBrightness, appropriateCt room)

    let neighbouringRooms room rooms =
        let mappings
            = Map.ofList [
                ("Living room"), ["Hallway"; "Bedroom"]
                ("Bedroom"), ["Living room"]
                ("Hallway"), ["Living room"; "Bathroom"; "Kitchen"]
                ("Kitchen"), ["Hallway"]
                ("Bathroom"), ["Hallway"]
            ]
        Map.find room.Name mappings
        |> List.map(fun neighbouringRoomName -> List.tryFind (fun room -> room.Name = neighbouringRoomName) rooms)
        |> List.choose id

    let planForRooms (rooms: Room list): (Room * LightState) list =
        let primaryRoomState
            = rooms
            |> List.fold (fun settings room ->
                if recentMotionReading room && allowedOnAsPrimary room
                then Map.add room (appropriateLightSettings On room) settings
                else Map.add room (appropriateLightSettings Off room) settings) Map.empty
        primaryRoomState
        |> Map.map (fun room currentState ->
            match currentState with
            | Off _ ->
                neighbouringRooms room rooms
                |> List.choose (fun neighbouringRoom ->
                    match Map.find neighbouringRoom primaryRoomState with
                    | Off _ -> None
                    | On _ -> Some neighbouringRoom)
                |> function
                    | [] -> appropriateLightSettings Off room // All neighbouring rooms are off
                    | _neighbouringRooms ->
                        if allowedOnAsSecondary ()
                        then appropriateLightSettings On room
                        else appropriateLightSettings Off room
            | On _ -> currentState)
        |> Map.toList
        |> List.filter (fun (room, newState) ->
            match room.LightState with
            | None -> true
            | Some lightState -> lightState <> newState)
        |> List.filter (fun (room, _newState) ->
            match room.LightState, room.RoomSensor.LightLevel with
            | Some (Off _), Some lightLevel -> lightLevel < 15000
            | _ -> true
        )

let deriveChanges (reading: Reading) =
    Internals.planForRooms reading.Rooms
