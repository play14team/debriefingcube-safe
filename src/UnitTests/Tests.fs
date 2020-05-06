module Tests

open System
open Xunit
open Swensen.Unquote

open DebriefindCube.Cube

let goal = { Number = 1; Lens = Goal; Question = "Goal 1"; DeepeningQuestions = [ "Deep 1"; "Deep 2"; "Deep 3" ] }
let proc = { Number = 2; Lens = Process; Question = "Process 1"; DeepeningQuestions = [ "Deep 1"; "Deep 2"; "Deep 3" ] }
let groupDynamics = { Number = 4; Lens = GroupDynamics; Question = "GroupDynamics 1"; DeepeningQuestions = [ "Deep 1"; "Deep 2"; "Deep 3" ] }
let communication = { Number = 5; Lens = Communication; Question = "Communication 1"; DeepeningQuestions = [ "Deep 1"; "Deep 2"; "Deep 3" ] }
let emotions = { Number = 6; Lens = Emotions; Question = "Emotions 1"; DeepeningQuestions = [ "Deep 1"; "Deep 2"; "Deep 3" ] }
let takeAway = { Number = 3; Lens = TakeAway; Question = "TakeAway 1"; DeepeningQuestions = [ "Deep 1"; "Deep 2"; "Deep 3" ] }

let deck : Deck = [ goal ; proc ; groupDynamics ; communication ; emotions ; takeAway ]

[<Fact>]
let ``Pick a Group Dynamics card`` () =
    let result = deck |> pickCard (GroupDynamics)
    let (card : Card, remaingDeck : Deck) = result
    test <@ card.Lens = GroupDynamics @>
    test <@ remaingDeck.Length = 5 @>
    test <@ remaingDeck |> List.contains card = false @>
    
