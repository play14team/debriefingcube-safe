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
    Deck: Deck option
    Card: Card option
    }

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
    | RollDice
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
    match currentModel.Lens, currentModel.Deck, currentModel.Card, msg with
    | _, Some deck, _, RollDice ->
        let lens = rollDice()
        let (c, d) = deck |> tryDrawCard lens
        let nextModel = { currentModel with Lens = Some lens ; Deck = Some d ; Card = c }
        nextModel, Cmd.none
    | _, _, _, InitialDeckLoaded initialDeck ->
        let nextModel = { Lens = None ; Deck = Some initialDeck ; Card = None }
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

let show (model : Model) : string =
    match model.Lens, model.Deck, model.Card with
    | _, _, Some card -> sprintf "%A : %s" card.Lens card.Question
    | Some lens, _, None -> sprintf "No more card for lens %A" lens
    | _, _, _ -> "Loading..."

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
                    [ str "SAFE Template" ] ] ]

          Container.container []
              [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                    [ Heading.h3 [] [ str ("Debriefing cube: " + show model) ] ]
                Columns.columns []
                    [ Column.column [] [ button "Roll dice" (fun _ -> dispatch RollDice) ] ] ]

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
