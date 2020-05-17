open System.IO

open FSharp.Control.Tasks.V2
open Giraffe
open Saturn

open DebriefingCube.Data

let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x

let publicPath = Path.GetFullPath "../Client/public"

let port =
    "SERVER_PORT"
    |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us

let webApp = router {

    get DebriefingCube.Uris.Deck (fun next ctx ->
        task {
            let deck = Cards.load()
            return! json deck next ctx
        })

    get DebriefingCube.Uris.Lenses (fun next ctx ->
        task {
            let deck = Cards.load()
            return! json deck next ctx
        })
}

let app = application {
    url ("http://0.0.0.0:" + port.ToString() + "/")
    use_router webApp
    memory_cache
    use_static publicPath
    use_json_serializer(Thoth.Json.Giraffe.ThothSerializer())
    use_gzip
}

run app
