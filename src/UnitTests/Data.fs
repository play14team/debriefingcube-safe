module Data

open System
open Xunit
open Swensen.Unquote
open FSharp.Data

open DebriefingCube.Cube

type DebriefingCubeJson = JsonProvider<"../Server/Data/debriefingcube-en.json">

[<Fact>]
let ``Load data from Json`` () =
    let doc = DebriefingCubeJson.Load("./Data/debriefingcube-en.json")
    test <@ doc.Version = 2.0m @>

[<Fact>]
let ``Load cards from Json`` () =
    let deck = DebriefingCube.Data.getCards ()
    test <@ deck.Length = 42 @>

    let firstCard = deck |> List.head
    test <@ firstCard.Number = 1 @>
    test <@ firstCard.Lens = Goal @>
