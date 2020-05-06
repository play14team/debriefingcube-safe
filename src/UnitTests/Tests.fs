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
    let (c, d) = deck |> tryPickCard GroupDynamics

    test <@ Option.isSome c @>
    match c with
    | Some card -> 
        test <@ card.Lens = GroupDynamics @>
        test <@ d.Length = 5 @>
        test <@ d |> List.contains card |> not @>
    | None -> 
        raise <| Exception "Should not have been None"

[<Fact>]
let ``Pick 2 Group Dynamics card`` () =
    let (card', deck') = deck |> tryPickCard GroupDynamics
    let (card'', deck'') = deck' |> tryPickCard GroupDynamics

    test <@ Option.isSome card' @>
    test <@ Option.isNone card'' @>
    test <@ deck' = deck'' @>
