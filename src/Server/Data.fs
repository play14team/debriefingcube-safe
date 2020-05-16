namespace DebriefingCube

module Data =

    open System
    open FSharp.Data
    open DebriefingCube.Cube

    type DebriefingCube = JsonProvider<"./Data/debriefingcube-en.json", EmbeddedResource="Server, debriefingcube-en.json">
    

    let toCard (card : DebriefingCube.Card) : Card =
        {
            Number = card.Number
            Lens = card.Lens |> Lens.toLens
            Question = card.Question
            DeepeningQuestions = card.DeepeningQuestions
        }

    let loadDeck () : Deck =
        let doc = DebriefingCube.Load("./Data/debriefingcube-en.json")
        let cards = doc.Cards |> Seq.map toCard
        cards |> Seq.toList
