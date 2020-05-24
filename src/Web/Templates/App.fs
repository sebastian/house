module App

open Giraffe.GiraffeViewEngine

let layout (content: XmlNode) =
    html [] [
        head [] [
            meta [_charset "utf-8"]
            meta [_name "viewport"; _content "width=device-width, initial-scale=1" ]
            title [] [encodedText "Lights!"]
        ]
        body [] [
            h1 [] [rawText "House!"]
            content
        ]
    ]