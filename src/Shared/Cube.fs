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

    type LensInfo = {
        Lens : Lens
        Name : string
        Description : string
        DeepeningQuestions : string []
    }

    type Lenses = LensInfo list

    module Lens =

        let all =
            [ Goal; Process; GroupDynamics; Communication; Emotions; TakeAway ]    

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

    module Lenses =
        let getInfo lens lenses =
            lenses |> List.find (fun l -> l.Lens = lens)

    type Card = {
        Number : int
        Lens : Lens
        Question : string
        DeepeningQuestions : string []
    }

    module Card = 
        let isLens (lens : Lens) (card : Card) = card.Lens = lens

    type Deck = Card list

    module Deck =
        let ofLens (lens : Lens) (deck : Deck) : Deck = deck |> List.filter (Card.isLens lens)

        let countLens (lens : Lens) (deck : Deck) : int =
            let lensCards = deck |> ofLens lens
            lensCards.Length

        let countLenses (deck : Deck) =
            Lens.all
            |> List.map (fun l -> l, (countLens l deck) )

        let tryDrawCard (lens : Lens) (deck : Deck) : ( Card option * Deck ) =
            let card = deck |> ofLens lens |> Helpers.shuffleR (System.Random()) |> Seq.tryHead
            match card with
            | Some c -> 
                let remainingDeck : Deck = deck |> List.filter (fun x -> x <> c)
                (card, remainingDeck)
            | None ->
                (None, deck)

    let roll () : Lens =
        Lens.all
        |> Helpers.shuffleR (System.Random())
        |> List.head

module Uris =

    [<Literal>]
    let Deck = "/api/deck/"

    [<Literal>]
    let Lenses = "/api/lenses/"
