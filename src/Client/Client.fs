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
    Counter: Counter option
    Lens: Lens option
    Card: Card option
    Deck: Deck option
    }

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
    | RollDice
    | Reset
    | InitialDeckLoaded of Counter

let initialDeck () = Fetch.fetchAs<Counter> "/api/init"

// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
    let initialModel = { Counter = None ; Lens = None ; Deck = None ; Card = None }
    let getDeckCmd =
        Cmd.OfPromise.perform initialDeck () InitialDeckLoaded
    initialModel, getDeckCmd

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match currentModel.Counter, msg with
    | _, Reset ->
        init()
    | Some countrer, RollDice ->
        let lens = rollDice()
        //let (c, d) = deck |> tryDrawCard lens
        let nextModel = { currentModel with Counter = Some { Value = countrer.Value + 1 } ; Lens = Some lens }
        nextModel, Cmd.none
    | _, InitialDeckLoaded initialCount ->
        let nextModel = { Counter = Some initialCount ; Lens = None ; Deck = None ; Card = None }
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
| { Counter = Some counter } -> string counter.Value
| { Counter = None   } -> "Loading..."

let showLens = function
| { Lens = Some lens } -> string lens
| { Lens = None   } -> "Roll the dice"

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
                    [ Heading.h3 [] [ str (showLens model) ] ]
                Content.content []
                    [ Image.image [ Image.Is128x128 ]
                        [ img [ Src (getDicePicture model) ] ] ]
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
