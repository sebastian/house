module LightServer

open Hue.Types

let mutable private username = ""

let mutable private state: State = {
    LastReading = None
    Mode = Automatic
}

module private Internals =
    let rec getIp () = async {
        printfn "Looking up IP of Hue base station"
        let! ipOption = Hue.API.getIp ()
        match ipOption with
        | Some ip ->
            printfn "IP of Hue API endpoint is %s" ip
            return ip
        | None ->
            printfn "Could not determine the IP. Waiting a minute and trying again"
            do! Async.Sleep (60 * 1000)
            return! getIp ()
    }

    let rec action ip  = async {
        match state.Mode with
        | Manual -> ()
        | Automatic ->
            let! latestReading = Hue.API.getState ip username
            state <- {state with LastReading = Some latestReading}
            let! _ =
                LightRules.deriveChanges latestReading
                |> List.map (fun (room, lightState) -> async {
                    let! response = Hue.API.setState ip username room lightState
                    let statusCode = response.StatusCode
                    if statusCode < 200 || statusCode > 299
                    then printfn "Unexpected response when trying to set light: %s" (response.Body.ToString())
                    return ()
                })
                |> Async.Parallel
            ()
        do! Async.Sleep 500
        return! action ip
    }

let start () =
    match System.Environment.GetEnvironmentVariable "username" with
    | null ->
        printfn "Cannot start the system without a username. Please set the 'username' environment variable."
        exit 1
    | envUsername -> username <- envUsername 
    printfn "Starting light loop"
    async {
        let! ip = Internals.getIp ()
        return! Internals.action ip
    }
    |> Async.Start

let setMode mode =
    state <- {state with Mode = mode}

let getState () = state