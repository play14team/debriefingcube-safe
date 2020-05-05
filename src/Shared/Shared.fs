namespace Shared

type Counter = { Value : int }


module Cube =

    type Lens =
    | Goal
    | Process
    | GroupDynamics
    | Communication
    | Emotions
    | TakeAway

    let lenses = [| Goal; Process; GroupDynamics; Communication; Emotions; TakeAway |]

    let roll () : Lens =
        let value = System.Random().Next(1, 6)
        lenses.[value]

module Cards =

    open Cube

    type Card = {
        Number : int
        Lens : Lens
        Question : string
        DeepeningQuestions : string seq
    }

    type Deck = Card list

    type Deal = Deck -> Deck * Card

    let deck =
    [
        { Number = 1; Lens = Goal; Question = "Goal 1"; DeepeningQuestions = [ "Deep 1"; "Deep 2"; "Deep 3" ] }
        { Number = 2; Lens = Process; Question = "Process 1"; DeepeningQuestions = [ "Deep 1"; "Deep 2"; "Deep 3" ] }
        { Number = 4; Lens = GroupDynamics; Question = "GroupDynamics 1"; DeepeningQuestions = [ "Deep 1"; "Deep 2"; "Deep 3" ] }
        { Number = 5; Lens = Communication; Question = "Communication 1"; DeepeningQuestions = [ "Deep 1"; "Deep 2"; "Deep 3" ] }
        { Number = 6; Lens = Emotions; Question = "Emotions 1"; DeepeningQuestions = [ "Deep 1"; "Deep 2"; "Deep 3" ] }
        { Number = 3; Lens = TakeAway; Question = "TakeAway 1"; DeepeningQuestions = [ "Deep 1"; "Deep 2"; "Deep 3" ] }
    ]


