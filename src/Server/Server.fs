open System.IO

open FSharp.Control.Tasks.V2
open Giraffe
open Saturn

open Shared
open DebriefingCube.Cube
open DebriefingCube.Data

let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x

let publicPath = Path.GetFullPath "../Client/public"

let port =
    "SERVER_PORT"
    |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us

let webApp = router {

    get "/api/init" (fun next ctx ->
        task {
            let counter = {Value = 0}
            return! json counter next ctx
        })

    get "/api/deck" (fun next ctx ->
        task {
            let deck = loadDeck()
            return! json deck next ctx
        })

    get "/api/card" (fun next ctx ->
        task {
            let deck = loadDeck()
            let card = deck |> List.head
            return! json card next ctx
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
