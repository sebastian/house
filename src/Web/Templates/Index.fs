module Index

open Giraffe.GiraffeViewEngine
open Hue.Types

let index () =
  let lightState = LightServer.getState ()
  match lightState.LastReading with
  | None -> p [] [rawText "Currently there is no reading of the light state. Maybe the system hasn't booted."]
  | Some reading ->
    div [] [
      ul [] (
        reading.Rooms
        |> List.map(fun room ->
          let roomContent =
            match room.LightState with
            | None -> rawText <| sprintf "%s is in an unknown state" room.Name
            | Some (LightState.Off _) -> rawText <| sprintf "%s has the lights turned off" room.Name
            | Some (LightState.On _) -> strong [] [rawText <| sprintf "%s has the lights turned on" room.Name]
          li [] [roomContent]
        )
      )
      div [] [
        p [] [
          rawText (
            match lightState.Mode with
            | Mode.Automatic -> "The system is currently in automatic mode."
            | Mode.Manual -> "The system is currently in manual mode."
          )
        ]
        form [_action "/"; _method "POST"] [
          button [_type "Submit"] [rawText "Toggle mode"]
        ]
      ]
      div [] (
        reading.Rooms
        |> List.map(fun room ->
          let sensor = room.RoomSensor
          (room.Name, sensor.Battery)
        )
        |> List.filter(snd >> ((>) 30))
        |> List.map(fun (room, batteryLevel) ->
          div [] [
            rawText (sprintf "The battery level of the light sensor in room %s is at %i percent." room batteryLevel)
          ]
        )
      )
    ]

let layout =
  App.layout << index