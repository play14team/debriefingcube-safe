namespace DebriefingCube

module Data =

    open System
    open FSharp.Data
    open DebriefingCube.Cube

    type CardsJson = JsonProvider<"./Data/cards-en.json", EmbeddedResource="Server, cards-en.json">
    
    module Cards =

        let toCard (card : CardsJson.Card) : Card =
            {
                Number = card.Number
                Lens = card.Lens |> Lens.toLens
                Question = card.Question
                DeepeningQuestions = card.DeepeningQuestions
            }

        let load () : Deck =
            let doc = CardsJson.Load("./Data/cards-en.json")
            let cards = doc.Cards |> Seq.map toCard
            cards |> Seq.toList

    type LensesJson = JsonProvider<"./Data/lenses-en.json", EmbeddedResource="Server, lenses-en.json">

    module Lenses =
        let toLensInfo (lens : LensesJson.Lensis) : LensInfo =
            {
                Lens = Lens.toLens lens.Lens
                Name = lens.Name
                Description = lens.Description
                DeepeningQuestions = lens.DeepeningQuestions
            }

        let load() : Lenses =
            let doc = LensesJson.Load("./Data/lenses-en.json")
            let lenses = doc.Lenses |> Seq.map toLensInfo
            lenses |> Seq.toList
