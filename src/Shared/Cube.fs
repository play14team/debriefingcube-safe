namespace DebriefingCube

module Helpers =
    open System

    let shuffleR (r : Random) xs = xs |> List.sortBy (fun _ -> r.Next())

module Cube =

    type Lens =
    | Goal
    | Process
    | GroupDynamics
    | Communication
    | Emotions
    | TakeAway

    module Lens =

        let toLens s =
            match s : string with
            | "Goal" -> Goal
            | "Process" -> Process
            | "GroupDynamics" -> GroupDynamics
            | "Communication" -> Communication
            | "Emotions" -> Emotions
            | "TakeAway" -> TakeAway
            | _ -> raise <| System.ArgumentOutOfRangeException("s", "Cannot be mapped")

        let fromLens (l : Lens) =
            sprintf "%A" l

    let rollDice () : Lens =
        [ Goal; Process; GroupDynamics; Communication; Emotions; TakeAway ]
        |>  Helpers.shuffleR (System.Random())
        |> List.head

    type Card = {
        Number : int
        Lens : Lens
        Question : string
        DeepeningQuestions : string seq
    }

    type Deck = Card list

    module Card = 
        let isLens (lens : Lens) (card : Card) = card.Lens = lens

        let ofLens (lens : Lens) (deck : Deck) : Deck = deck |> List.filter (isLens lens)

    let tryDrawCard (lens : Lens) (deck : Deck) : ( Card option * Deck ) =
        let card = deck |> Card.ofLens lens |> Helpers.shuffleR (System.Random()) |> Seq.tryHead
        match card with
        | Some c -> 
            let remainingDeck : Deck = deck |> List.filter (fun x -> x <> c)
            (card, remainingDeck)
        | None ->
            (None, deck)

module FakeData =

    open Cube

    let goal : Card = { Number = 1; Lens = Goal; Question = "Goal 1"; DeepeningQuestions = [ "Deep 1"; "Deep 2"; "Deep 3" ] }
    let proc : Card = { Number = 2; Lens = Process; Question = "Process 1"; DeepeningQuestions = [ "Deep 1"; "Deep 2"; "Deep 3" ] }
    let groupDynamics : Card = { Number = 4; Lens = GroupDynamics; Question = "GroupDynamics 1"; DeepeningQuestions = [ "Deep 1"; "Deep 2"; "Deep 3" ] }
    let communication : Card = { Number = 5; Lens = Communication; Question = "Communication 1"; DeepeningQuestions = [ "Deep 1"; "Deep 2"; "Deep 3" ] }
    let emotions : Card = { Number = 6; Lens = Emotions; Question = "Emotions 1"; DeepeningQuestions = [ "Deep 1"; "Deep 2"; "Deep 3" ] }
    let takeAway : Card = { Number = 3; Lens = TakeAway; Question = "TakeAway 1"; DeepeningQuestions = [ "Deep 1"; "Deep 2"; "Deep 3" ] }
    
    let deck : Deck = [ goal ; proc ; groupDynamics ; communication ; emotions ; takeAway ]