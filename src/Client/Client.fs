module Client

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fetch.Types
open Thoth.Fetch
open Fulma
open Thoth.Json

open DebriefingCube.Cube

type Model = {
    Lens: Lens option
    Card: Card option
    Deck: Deck option
    Lenses: Lenses option
    }

type Msg =
    | Reset
    | Roll
    | InitialDeckLoaded of Deck
    | InitialLensesLoaded of Lenses

let initialDeck () = Fetch.fetchAs<unit, Deck> DebriefingCube.Uris.Deck
let initialLenses () = Fetch.fetchAs<unit, Lenses> DebriefingCube.Uris.Lenses

let init () : Model * Cmd<Msg> =
    let initialModel = { Lens = None ;  Card = None ; Deck = None ; Lenses = None }
    let getDeckCmd = Cmd.OfPromise.perform initialDeck () InitialDeckLoaded
    let getLensesCmd = Cmd.OfPromise.perform initialLenses () InitialLensesLoaded
    initialModel, Cmd.batch [ getDeckCmd ; getLensesCmd ]

let rec update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match currentModel, msg with
    | _, Reset ->
        init()
    | model, Roll ->
        match model.Deck with
        | Some deck ->
            if deck.Length = 0 then
                init()
            else
                let newLens = DebriefingCube.Cube.roll()
                let count = (deck |> Deck.countLens newLens)
                if count > 0 then
                    let (newCard, newDeck) = deck |> Deck.tryDrawCard newLens
                    let nextModel = { model with Lens = Some newLens ; Card = newCard ; Deck = Some newDeck }
                    nextModel, Cmd.none
                else
                    update msg currentModel
        | None ->
            model, Cmd.none
    | model, InitialDeckLoaded deck ->
        let nextModel = { model with Deck = Some deck }
        nextModel, Cmd.none
    | model, InitialLensesLoaded lenses ->
        let nextModel = { model with Lenses = Some lenses }
        nextModel, Cmd.none

let footerComponents =
    [
        strong [ ] [
            a [ Href "https://thedebriefingcube.com"
                Target "_blank" ]
                [ str "#TheDebriefingCube" ]
            ]
        str " version "
        strong [ ] [ str Version.cube ]
        str " created by "
        strong [ ] [
            a [ Href "https://play14.org/players/chris-caswell"
                Target "_blank" ]
                [ str "Chris Caswell" ]
            str " & "
            a [ Href "https://play14.org/players/julian-kea"
                Target "_blank" ]
                [ str "Julian Kea" ]
            ]
        str ". This app was developed by "
        strong [ ] [ 
            a [ Href "https://play14.org/players/cedric-pontet"
                Target "_blank" ]
                [ str "Cédric Pontet" ]
            ]
        str " using "
        a [ Href "https://github.com/SAFE-Stack/SAFE-template"
            Target "_blank" ]
            [ str "SAFE  "
              str Version.template ]
        br []
        str "This work is licensed under a "
        a [ Href "https://creativecommons.org/licenses/by/4.0/"
            Target "_blank" ]
            [ str "Creative Commons Attribution 4.0 International License (CC BY 4.0)." ]
        br []
        str "The debriefing cube was first discussed at "
        a [ Href "https://play14.org/events/timisoara/2017-07"
            Target "_blank" ]
                [ str "#play14 Timișoara 2017" ]
        str " and first presented at "
        a [ Href "https://play14.org/events/luxembourg/2018-03"
            Target "_blank" ]
            [ str "#play14 Luxembourg 2018." ]
    ]

module Cube =
    let button txt onClick color =
        Button.button
            [ Button.IsFullWidth
              Button.Color color
              Button.OnClick onClick ]
            [ str txt ]

    let cubeImage = function
        | Some l -> (l |> sprintf "%A-lens.png").ToLower()
        | None -> "cube.png"

    let showCube (model : Model) =
        Image.image
            [ Image.CustomClass "is-192x192 is-inline-block" ]
            [ img [ Src (cubeImage model.Lens) ] ]

    let showLens (model : Model) =
        let lens = 
            match model.Lens, model.Lenses with
            | Some lens, Some lenses ->
                let info = lenses |> Lenses.getInfo lens
                info.Name
            | _, _ ->
                "Roll the cube"
        Heading.h4 [] [ str lens ]

    let showButtons (dispatch : Msg -> unit) =
        Columns.columns [ Columns.IsGap (Screen.All, Columns.Is8) ]
            [ Column.column [ ]
                [ button "Roll" (fun _ -> dispatch Roll) IsPrimary ]
              Column.column [ ]
                [ button "Reset" (fun _ -> dispatch Reset) IsDanger ]
            ]
    let show (model : Model) (dispatch : Msg -> unit) =
        [ Heading.p [ ] [ str "Cube" ]
          p [ ]
            [ Columns.columns
                [ Columns.IsGap (Screen.All, Columns.Is1) ]
                [ Column.column [ ]
                    [ Content.content
                        [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                        [ showCube model
                          showLens model
                          showButtons dispatch
                        ]
                    ]

                ]
            ]
        ]
        
module Card =
    let question q =
        h3 [] [ str q ]

    let deepeningQuestion q =
        h4 []
           [ Icon.icon [ ] [ i [ ClassName "fas fa-angle-right" ] [ ] ]
             str q
           ]

    let showQuestions (card : Card) =
        let question = question card.Question
        let deepeningQuestions = card.DeepeningQuestions |> Array.map deepeningQuestion |> Array.toList
        let elements = question :: deepeningQuestions
        Content.content [ ] elements

    let noCard =
        Content.content [ ] [ h4 [ ] [str "No card"] ]

    let tryShowQuestions = function
        | Some c -> showQuestions c
        | None -> noCard

    let showNumber (card : Card) = 
        Content.content [ ] [ h5 [] [ str (sprintf "#%i" card.Number) ] ]

    let tryShowNumber = function
        | Some c -> showNumber c
        | None -> span [] []

    let show (model : Model) =
        [ Level.level [ ]
            [ Level.left [ ]
                [ Level.item [ ] [ Heading.p [ ] [ str "Questions" ] ] ]
              Level.right [ ]
                [ Level.item [ ] [ tryShowNumber model.Card ] ]
            ]
          p [ ] [ tryShowQuestions model.Card ]
        ]

module Deck = 
    let lensImage (lens: Lens) (count : int) =
        Image.image [ ]
            [ img [
                Src (sprintf "%A-back.png" lens)
                Class ( sprintf "shadow%i" count) ] ]

    let lensContent (lenses : Lenses) (lens : Lens, count : int) =
        let info = lenses |> Lenses.getInfo lens
        Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
            [ str info.Name
              h2 [] [ str (sprintf "%i" count) ]
              lensImage lens count
              str info.Description
            ]

    let lensColumn (lenses : Lenses) (lens : Lens, count : int) =
        Column.column [ Column.Option.Width (Screen.All, Column.Is3) ] [ lensContent lenses (lens, count) ]

    let lensColumns deck lenses =
        let counters = deck |> Deck.countLenses
        let columnList = counters |> List.map (lensColumn lenses)
        let columns = columnList
                        |> List.splitInto 2
                        |> List.map (fun l -> 
                                        Columns.columns [ Columns.IsGrid
                                                          Columns.IsCentered
                                                          Columns.IsGap (Screen.All, Columns.Is8) ] l)
        let elements = columns.Head :: hr [] :: columns.Tail
        [ Text.div [ Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
            elements
        ]

    let showLoading =
        Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
            [ h2 [] [ str "Loading..." ] ]

    let show (model: Model) =
        let elements =
            match model.Deck, model.Lenses with
            | Some deck, Some lenses ->
                lensColumns deck lenses
            | _, _ -> [ showLoading ]
        [ Heading.p [ ] [ str "Cards" ]
          p [ ] elements
        ]

let view (model : Model) (dispatch : Msg -> unit) =
    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
            [ Navbar.Item.div [ ]
                [ Icon.icon [ Icon.Size IsLarge ] [ i [ ClassName "fas fa-2x fa-cube" ] [ ] ]
                  Heading.h2 [ ]
                    [ str "The Debriefing Cube" ] ] ]

          Container.container [ Container.IsFluid ]
              [
                Tile.ancestor [ ]
                    [ Tile.parent [ Tile.IsVertical
                                    Tile.Size Tile.Is4 ]
                        [ Tile.child [ ]
                            [ Box.box' [ ]
                                (Cube.show model dispatch) ]
                          Tile.child [ ]
                            [ Box.box' [ ]
                                (Card.show model) ]
                        ]
                      Tile.parent [ ]
                        [ Tile.child [ ]
                            [ Box.box' [ Common.Props [ Style [ Height "100%" ] ] ]
                                (Deck.show model) 
                            ]
                        ]
                    ]
              ]

          Footer.footer [ ]
                [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                    footerComponents ] ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
