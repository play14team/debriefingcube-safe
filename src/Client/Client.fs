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

let initialDeck () = Fetch.fetchAs<Deck> DebriefingCube.Uris.Deck
let initialLenses () = Fetch.fetchAs<Lenses> DebriefingCube.Uris.Lenses

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

let safeComponents =
    let components =
        span [ ]
           [ a [ Href "https://github.com/SAFE-Stack/SAFE-template" ]
               [ str "SAFE  "
                 str Version.template ]
             str ", "
             a [ Href "https://saturnframework.github.io" ] [ str "Saturn" ]
             str ", "
             a [ Href "http://fable.io" ] [ str "Fable" ]
             str ", "
             a [ Href "https://elmish.github.io" ] [ str "Elmish" ]
             str ", "
             a [ Href "https://fulma.github.io/Fulma" ] [ str "Fulma" ]

           ]

    span [ ]
        [ str "Version "
          strong [ ] [ str Version.app ]
          str " powered by: "
          components ]

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

    let showCube (lens : Lens option) =
        Image.image
            [ Image.CustomClass "is-192x192 is-inline-block" ]
            [ img [ Src (cubeImage lens) ] ]

    let showLens (lens : Lens option) =
        Heading.h4 []
            [ str ( match lens with
                        | Some l -> Lens.toString l
                        | None -> "Roll the cube" )
            ]

    let showButtons (dispatch : Msg -> unit) =
        Columns.columns [ Columns.IsGap (Screen.All, Columns.Is8) ]
            [ Column.column [ ]
                [ button "Roll" (fun _ -> dispatch Roll) IsPrimary ]
              Column.column [ ]
                [ button "Reset" (fun _ -> dispatch Reset) IsDanger ]
            ]
    let show (lens : Lens option) (dispatch : Msg -> unit) =
        [ Heading.p [ ] [ str "Cube" ]
          p [ ]
            [ Columns.columns
                [ Columns.IsGap (Screen.All, Columns.Is1) ]
                [ Column.column [ ]
                    [ Content.content
                        [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                        [ showCube lens
                          showLens lens
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

    let tryShow (card : Card option) =
        [ Level.level [ ]
            [ Level.left [ ]
                [ Level.item [ ] [ Heading.p [ ] [ str "Questions" ] ] ]
              Level.right [ ]
                [ Level.item [ ] [ tryShowNumber card ] ]
            ]
          p [ ] [ tryShowQuestions card ]
        ]

module Deck = 
    let lensImage (lens: Lens) =
        Image.image [ Image.IsSquare ] [ img [ Src (sprintf "%A-back.png" lens) ] ]

    let lensDeck (lenses : Lenses) (lens : Lens, count : int) =
        let info = lenses |> List.find (fun l -> l.Lens = lens)
        Column.column [ ]
            [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                [ str info.Name
                  h2 [] [ str (sprintf "%i" count) ]
                  p [] [ lensImage lens ]
                  p [] [ str info.Description ]
                ]
            ]

    let showDeck deck lenses =
        let counters = deck |> Deck.countLenses
        let columns = counters |> List.map (lensDeck lenses)
        Columns.columns [ Columns.IsGap (Screen.All, Columns.Is1) ] columns

    let showLoading =
        Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
            [ h2 [] [ str "Loading..." ] ]

    let show (model: Model) =
        let elements =
            match model.Deck, model.Lenses with
            | Some deck, Some lenses ->
                showDeck deck lenses
            | _, _ -> showLoading
        [ Heading.p [ ] [ str "Cards" ]
          p [ ] [ elements ]
        ]

let view (model : Model) (dispatch : Msg -> unit) =
    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
            [ Navbar.Item.div [ ]
                [ Heading.h2 [ ]
                    [ str "The Debriefing Cube" ] ] ]

          Container.container [ Container.IsFluid ]
              [
                Tile.ancestor [ ]
                    [ Tile.parent [ Tile.IsVertical
                                    Tile.Size Tile.Is4 ]
                        [ Tile.child [ ]
                            [ Box.box' [ ]
                                (Cube.show model.Lens dispatch) ]
                          Tile.child [ ]
                            [ Box.box' [ ]
                                (Card.tryShow model.Card) ]
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
                    [ safeComponents ] ] ]

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
