module Data

open System
open Xunit
open Swensen.Unquote
open FSharp.Data

open DebriefingCube.Cube

type CardsJson = JsonProvider<"../Server/Data/cards-en.json">

[<Fact>]
let ``Load data from Json`` () =
    let doc = CardsJson.Load("./Data/cards-en.json")
    test <@ doc.Version = 2.0m @>

[<Fact>]
let ``Load cards from Json`` () =
    let deck = DebriefingCube.Data.loadDeck ()
    test <@ deck.Length = 42 @>

    let firstCard = deck |> List.head
    test <@ firstCard.Number = 1 @>
    test <@ firstCard.Lens = Goal @>
