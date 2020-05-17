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

        let toString (l : Lens) =
            match l with
            | GroupDynamics -> "Group Dynamics"
            | TakeAway -> "Take-Away"
            | _ -> sprintf "%A" l

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

module FakeData =

    open Cube

    let goal : Card = { Number = 1; Lens = Goal; Question = "Goal 1" ; DeepeningQuestions = [||]}
    let proc : Card = { Number = 2; Lens = Process; Question = "Process 1" ; DeepeningQuestions = [||]}
    let groupDynamics : Card = { Number = 4; Lens = GroupDynamics; Question = "GroupDynamics 1" ; DeepeningQuestions = [||]}
    let communication : Card = { Number = 5; Lens = Communication; Question = "Communication 1" ; DeepeningQuestions = [||]}
    let emotions : Card = { Number = 6; Lens = Emotions; Question = "Emotions 1" ; DeepeningQuestions = [||]}
    let takeAway : Card = { Number = 3; Lens = TakeAway; Question = "TakeAway 1" ; DeepeningQuestions = [||]}
    
    let deck : Deck = [ goal ; proc ; groupDynamics ; communication ; emotions ; takeAway ]