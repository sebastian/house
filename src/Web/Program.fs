module Server

open Saturn

LightServer.start ()

let endpointPipe = pipeline {
    plug head
    plug requestId
}

let portNumber () =
    match System.Environment.GetEnvironmentVariable("PORT") with
    | null -> "5000"
    | port -> port
    
let app = application {
    pipe_through endpointPipe
    error_handler (fun ex _ -> pipeline { render_html (InternalError.layout ex) })
    use_router Router.appRouter
    url (sprintf "http://0.0.0.0:%s/" (portNumber()))
    use_static "static"
    use_gzip
}

[<EntryPoint>]
let main _ =
    printfn "Working directory - %s" (System.IO.Directory.GetCurrentDirectory())
    run app
    0 // return an integer exit code