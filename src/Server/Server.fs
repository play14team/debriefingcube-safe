open System.IO
open System.Threading.Tasks

open System
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open FSharp.Control.Tasks.V2
open Giraffe
open Saturn
open Shared

open Microsoft.WindowsAzure.Storage

let tryGetEnv key =
    match Environment.GetEnvironmentVariable key with
    | x when String.IsNullOrWhiteSpace x -> None
    | x -> Some x

let publicPath = tryGetEnv "public_path" |> Option.defaultValue "../Client/public" |> Path.GetFullPath
let storageAccount = tryGetEnv "STORAGE_CONNECTIONSTRING" |> Option.defaultValue "UseDevelopmentStorage=true" |> CloudStorageAccount.Parse

let port =
    "SERVER_PORT"
    |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us

let webApp = router {
    get DebriefingCube.Uris.Deck (fun next ctx ->
        task {
            let deck = DebriefingCube.Data.Cards.load()
            return! json deck next ctx
        })

    get DebriefingCube.Uris.Lenses (fun next ctx ->
        task {
            let deck = DebriefingCube.Data.Lenses.load()
            return! json deck next ctx
        })
}

let configureAzure (services:IServiceCollection) =
    tryGetEnv "APPINSIGHTS_INSTRUMENTATIONKEY"
    |> Option.map services.AddApplicationInsightsTelemetry
    |> Option.defaultValue services

let app = application {
    url ("http://0.0.0.0:" + port.ToString() + "/")
    use_router webApp
    memory_cache
    use_static publicPath
    use_json_serializer(Thoth.Json.Giraffe.ThothSerializer())
    service_config configureAzure
    use_gzip
}

run app
