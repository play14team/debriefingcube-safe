namespace DebriefingCube

module Data =

    open System
    open FSharp.Data
    open DebriefingCube.Cube

    type DebriefingCube = JsonProvider<"./Data/debriefingcube-en.json", EmbeddedResource="Server, debriefingcube-en.json">
    
    let toLens s =
        match s : string with
        | "Goal" -> Goal
        | "Process" -> Process
        | "GroupDynamics" -> GroupDynamics
        | "Communication" -> Communication
        | "Emotions" -> Emotions
        | "TakeAway" -> TakeAway

    let toCard (card : DebriefingCube.Card) : Card =
        //let number = row.Num
        //let lens = row.Lens |> toLens
        //let question = row.MainQuestion
        //let deepeningQuestions = row.DeepeningQuestions.Split ('\n')
        {
            Number = card.Number
            Lens = card.Lens |> toLens
            Question = card.Question
            DeepeningQuestions = card.DeepeningQuestions
        }

    let getCards () : Deck =
        let doc = DebriefingCube.Load("./Data/debriefingcube-en.json")
        let cards = doc.Cards |> Seq.map toCard
        cards |> Seq.toList
