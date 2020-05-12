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