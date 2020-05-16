module Client

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fetch.Types
open Thoth.Fetch
open Fulma
open Thoth.Json

open Shared
open DebriefingCube.Cube

// The model holds data that you want to keep track of while the application is running
// in this case, we are keeping track of a counter
// we mark it as optional, because initially it will not be available from the client
// the initial value will be requested from server
type Model = {
    Lens: Lens option
    Card: Card option
    Deck: Deck
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
    let initialModel = { Lens = None ; Deck = [] ; Card = None }
    let getDeckCmd =
        Cmd.OfPromise.perform initialDeck () InitialDeckLoaded
    initialModel, getDeckCmd

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match currentModel, msg with
    | _, Reset ->
        init()
    | model, RollDice ->
        let { Lens=_ ; Card=_ ; Deck= deck } = model
        let newLens = rollDice()
        let (newCard, newDeck) = deck |> Deck.tryDrawCard newLens
        let nextModel = { Lens = Some newLens ; Card = newCard ; Deck = newDeck}
        nextModel, Cmd.none
    | _, InitialDeckLoaded initialDeck ->
        let nextModel = { Lens = None ; Deck = initialDeck ; Card = None }
        nextModel, Cmd.none
    | _ -> currentModel, Cmd.none


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

let showCounter = function
| { Deck = deck } ->
    let counts = deck |> Deck.countLenses
    string (sprintf "%A" counts)

let showLens = function
| Some lens -> string lens
| _ -> "Roll the dice"

let showCard = function
| Some card -> card.Question
| _ -> "No card"

let showCardDetails = function
| Some card -> sprintf "%A" card.DeepeningQuestions
| _ -> ""

let getDicePicture model =
    match model.Lens with
    | Some lens ->
        let s = lens |> sprintf "%A"
        s.ToLower() + "-lens.png"
    | None -> "cube.png"

let button txt onClick =
    Button.button
        [ Button.IsFullWidth
          Button.Color IsPrimary
          Button.OnClick onClick ]
        [ str txt ]

let view (model : Model) (dispatch : Msg -> unit) =
    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
            [ Navbar.Item.div [ ]
                [ Heading.h2 [ ]
                    [ str "The Debriefing Cube" ] ] ]

          Container.container []
              [
                Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                    [ Heading.h3 [] [ str (showCounter model) ] ]  
                Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                    [ Heading.h3 [] [ str (showLens model.Lens) ] ]
                Content.content []
                    [ Image.image [ Image.Is128x128 ]
                        [ img [ Src (getDicePicture model) ] ] ]
                Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                    [ Heading.h3 [] [ str (showCard model.Card) ]
                      Heading.h4 [] [ str (showCardDetails model.Card) ]
                    ]
                Columns.columns []
                    [ Column.column [] [ button "Roll dice" (fun _ -> dispatch RollDice) ]
                      Column.column [] [ button "Reset" (fun _ -> dispatch Reset) ]
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
