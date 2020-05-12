module Cube

open System
open Xunit
open Swensen.Unquote

open DebriefingCube.Cube

[<Fact>]
let ``Pick a Group Dynamics card`` () =
    let (c, d) = DebriefingCube.FakeData.deck |> Deck.tryDrawCard GroupDynamics

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
    let (card', deck') = DebriefingCube.FakeData.deck |> Deck.tryDrawCard GroupDynamics
    let (card'', deck'') = deck' |> Deck.tryDrawCard GroupDynamics

    test <@ Option.isSome card' @>
    test <@ Option.isNone card'' @>
    test <@ deck' = deck'' @>

