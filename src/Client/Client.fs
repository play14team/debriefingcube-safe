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

// The model holds data that you want to keep track of while the application is running
// in this case, we are keeping track of a counter
// we mark it as optional, because initially it will not be available from the client
// the initial value will be requested from server
type Model = {
    Lens: Lens option
    Card: Card option
    Deck: Deck option
    }

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
    | RollDice
    | Reset
    | InitialDeckLoaded of Deck

let initialDeck () = Fetch.fetchAs<Deck> "/api/deck"

// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
    let initialModel = { Lens = None ; Deck = None ; Card = None }
    let getDeckCmd =
        Cmd.OfPromise.perform initialDeck () InitialDeckLoaded
    initialModel, getDeckCmd

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
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

let button txt onClick color =
    Button.button
        [ Button.IsFullWidth
          Button.Color color
          Button.OnClick onClick ]
        [ str txt ]

let cubeImage = function
    | Some l -> (l |> sprintf "%A-lens.png").ToLower()
    | None -> "cube.png"

let showLens = function
| Some lens -> Lens.toString lens
| _ -> "Roll the dice"

let showCube (lens : Lens option) (dispatch : Msg -> unit) =
    Columns.columns [ Columns.IsGap (Screen.All, Columns.Is1) ]
     [ Column.column [ ]
         [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
             [ Image.image
                [ Image.CustomClass "is-256x256 is-inline-block" ]
                    [ img [ Src (cubeImage lens) ] ]
               Heading.h4 [] [ str (showLens lens) ]
               Columns.columns [ Columns.IsGap (Screen.All, Columns.Is8) ]
                [ Column.column [ ]
                    [ button "Roll" (fun _ -> dispatch RollDice) IsPrimary ]
                  Column.column [ ]
                    [ button "Reset" (fun _ -> dispatch Reset) IsDanger ]
                ]
             ]
         ]
     ]

let deepeningQuestion q =
    h4 [] [ Icon.icon [ ]
                [ i [ ClassName "fas fa-angle-right" ] [ ] ]
            str q ]

let showCard (card : Card) =
    let question = h3 [] [ str card.Question ]
    let deepeningQuestions = card.DeepeningQuestions |> Array.map deepeningQuestion |> Array.toList
    let all = question :: deepeningQuestions
    Content.content [ ] all

let noCard =
    Content.content [ ] [ h3 [ ] [str "No card"] ]

let tryShowCard = function
    | Some c -> showCard c
    | None -> noCard

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
                                [ Heading.p [ ]
                                    [ str "Cube" ]
                                  p [ ]
                                    [ showCube model.Lens dispatch ] ] ]
                          Tile.child [ ]
                            [ Box.box' [ ]
                                [ Heading.p [ ]
                                    [ str "Card" ]
                                  p [ ]
                                    [ tryShowCard model.Card ] ] ]
                        ]
                      Tile.parent [ ]
                        [ Tile.child [ ]
                            [ Box.box' [ Common.Props [ Style [ Height "100%" ] ] ]
                                [ Heading.p [ ]
                                    [ str "Deck" ]
                                  p [ ]
                                    [ tryShowDeck model.Deck ] ] ] ] ]
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
