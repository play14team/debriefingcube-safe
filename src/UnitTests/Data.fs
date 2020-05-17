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
    let deck = DebriefingCube.Data.Cards.load ()
    test <@ deck.Length = 42 @>

    let firstCard = deck |> List.head
    test <@ firstCard.Number = 1 @>
    test <@ firstCard.Lens = Goal @>

[<Fact>]
let ``Load lenses from Json`` () =
    let lenses = DebriefingCube.Data.Lenses.load ()
    test <@ lenses.Length = 6 @>

    let firstLens = lenses |> List.head
    test <@ firstLens.Lens = Goal @>
