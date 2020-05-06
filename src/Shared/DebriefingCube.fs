namespace DebriefindCube

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

    let roll : Lens =
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

    let isLens (lens : Lens) (card : Card) = card.Lens = lens

    let ofLens (lens : Lens) (deck : Deck) : Deck = deck |> List.filter (isLens lens)

    let pickCard (lens : Lens) (deck : Deck) : ( Card * Deck ) =
        let card = deck |> ofLens lens |> Helpers.shuffleR (System.Random()) |> Seq.head
        let remainingDeck : Deck = deck |> List.filter (fun x -> x <> card)
        (card, remainingDeck)

