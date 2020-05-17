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
    }

type Msg =
    | RollDice
    | Reset
    | InitialDeckLoaded of Deck

let initialDeck () = Fetch.fetchAs<Deck> "/api/deck"

let init () : Model * Cmd<Msg> =
    let initialModel = { Lens = None ; Deck = None ; Card = None }
    let getDeckCmd =
        Cmd.OfPromise.perform initialDeck () InitialDeckLoaded
    initialModel, getDeckCmd

let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match currentModel.Deck, msg with
    | _, Reset ->
        init()
    | Some deck, RollDice ->
        let newLens = rollDice()
        let (newCard, newDeck) = deck |> Deck.tryDrawCard newLens
        let nextModel = { Lens = Some newLens ; Card = newCard ; Deck = Some newDeck}
        nextModel, Cmd.none
    | None, RollDice ->
        { Lens = None ; Deck = None ; Card = None }, Cmd.none
    | _, InitialDeckLoaded initialDeck ->
        let nextModel = { Lens = None ; Deck = Some initialDeck ; Card = None }
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
                [ button "Roll" (fun _ -> dispatch RollDice) IsPrimary ]
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
    let showDeckImage (lens: Lens) =
        Image.image [ Image.IsSquare ] [ img [ Src (sprintf "%A-back.png" lens) ] ]

    let lensDeck (lens : Lens, count : int) =
        Column.column [ ]
            [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                [ str (Lens.toString lens)
                  h2 [] [ str (sprintf "%i" count) ]
                  p [] [ showDeckImage lens ]
                ]
            ]

    let showDeck deck =
        let counters = deck |> Deck.countLenses
        let columns = counters |> List.map lensDeck 
        Columns.columns [ Columns.IsGap (Screen.All, Columns.Is1) ] columns

    let showLoading =
        Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
            [ h2 [] [ str "Loading..." ] ]

    let tryShowDeck = function
        | Some d -> showDeck d
        | None -> showLoading

    let tryShow (deck : Deck option) =
        [ Heading.p [ ] [ str "Cards" ]
          p [ ] [ tryShowDeck deck ]
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
                                (Deck.tryShow model.Deck) 
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
