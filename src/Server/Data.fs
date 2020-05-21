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


module FakeData =

    open Cube

    let goal : Card = { Number = 1; Lens = Goal; Question = "Goal 1" ; DeepeningQuestions = [||]}
    let proc : Card = { Number = 2; Lens = Process; Question = "Process 1" ; DeepeningQuestions = [||]}
    let groupDynamics : Card = { Number = 4; Lens = GroupDynamics; Question = "GroupDynamics 1" ; DeepeningQuestions = [||]}
    let communication : Card = { Number = 5; Lens = Communication; Question = "Communication 1" ; DeepeningQuestions = [||]}
    let emotions : Card = { Number = 6; Lens = Emotions; Question = "Emotions 1" ; DeepeningQuestions = [||]}
    let takeAway : Card = { Number = 3; Lens = TakeAway; Question = "TakeAway 1" ; DeepeningQuestions = [||]}
    
    let deck : Deck = [ goal ; proc ; groupDynamics ; communication ; emotions ; takeAway ]

