module Router

open Saturn
open Giraffe.Core
open Giraffe.ResponseWriters
open Hue.Types

let browser = pipeline {
    plug acceptHtml
    plug putSecureBrowserHeaders
    set_header "x-pipeline-type" "Browser"
}

let toggleMode () =
    let lightState = LightServer.getState ()
    let nextMode =
        match lightState.Mode with
        | Mode.Automatic -> Manual
        | Mode.Manual -> Automatic
    LightServer.setMode nextMode
    redirectTo false "/"

let defaultView = router {
    get "/" (warbler (fun _ -> htmlView (Index.layout ())))
    post "/" (warbler (fun _ -> toggleMode ()))
    get "/index.html" (redirectTo false "/")
    get "/default.html" (redirectTo false "/")
}

let browserRouter = router {
    not_found_handler (htmlView NotFound.layout) 
    pipe_through browser 
    forward "" defaultView 
}

let appRouter = router {
    forward "" browserRouter
}