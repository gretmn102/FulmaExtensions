module Index

open Elmish
open Feliz
open Browser

open Fulma.Extensions

type State =
    {
        InputTagsState: InputTags.State
        TagsSuggestions: string Set
        InputWithSuggestions: InputWithSuggestions.State
    }
type Msg =
    | SetInputTagsState of InputTags.Msg
    | Submit
    | SetInputWithSuggestions of InputWithSuggestions.Msg
    | Submit2 of string
let tagsSuggestions =
    [|
        "Bunk House"
        "Inn"
        "Tavern"
        "Gambling Hall"
        "Drug Den"
        "Brothel"
        "Market"
        "Workshop"
        "Bakery"
        "Butchery"
        "Forge"
        "Tailory"
        "Work House"
        "Goat Stables"
        "Brewery"
        "Watch Post"
        "Court"
        "Jail"
        "Dock"
        "Ruin"
        "Row Houses"
        "Tenements"
        "Apartment Building"
        "Small House"
        "Bath House"
        "Shrine"
        "Tattoo Parlor"
        "Physicker Office"
        "Fighting Pit"
        "Warehouse"
        "Stockyard"
        "Factory"
        "Refinery"
        "Eelery"
    |]

let init () =
    let state =
        {
            InputTagsState = InputTags.init()
            InputWithSuggestions = InputWithSuggestions.init ""
            TagsSuggestions = Set tagsSuggestions
        }
    state, Cmd.none

let getTagSuggestionsFromServer (state:State) pattern  =
    async {
        do! Async.Sleep 250

        return
            if pattern = "" then
                [||]
            else
                state.TagsSuggestions
                |> Seq.filter (fun x -> x.Contains pattern)
                |> Array.ofSeq
    }

let update (msg: Msg) (state: State) =
    match msg with
    | SetInputTagsState msg ->
        let inputTagsState, cmd =
            InputTags.update (getTagSuggestionsFromServer state) msg state.InputTagsState
        let state =
            { state with
                InputTagsState = inputTagsState
            }
        let cmd = Cmd.map SetInputTagsState cmd
        state, cmd
    | Submit ->
        let state =
            { state with
                TagsSuggestions =
                    state.InputTagsState.InputTagsState.Tags
                    |> List.fold
                        (fun st x ->
                            Set.add x st
                        )
                        state.TagsSuggestions
            }
        state, Cmd.none

    | SetInputWithSuggestions msg ->
        let inputWithSuggestionsState, cmd =
            InputWithSuggestions.update (getTagSuggestionsFromServer state) msg state.InputWithSuggestions
        let state =
            { state with
                InputWithSuggestions = inputWithSuggestionsState
            }
        let cmd = Cmd.map SetInputWithSuggestions cmd
        state, cmd
    | Submit2 sug ->
        let state =
            { state with
                TagsSuggestions =
                    Set.add sug state.TagsSuggestions
            }
        state, Cmd.none

open Fable.React
open Fable.React.Props
open Fulma
open Fable.FontAwesome

let containerBox (state : State) (dispatch : Msg -> unit) =
    Box.box' [] [
        Box.box' [] [
            InputTags.view state.InputTagsState (SetInputTagsState >> dispatch)

            Button.button [
                let isDisabled =
                    List.isEmpty state.InputTagsState.InputTagsState.Tags

                Button.Disabled isDisabled
                Button.OnClick (fun _ ->
                    if not isDisabled then
                        dispatch Submit
                )
            ] [
                str "Submit"
            ]
        ]
        Box.box' [] [
            InputWithSuggestions.view "Tag" (Submit2 >> dispatch) state.InputWithSuggestions (SetInputWithSuggestions >> dispatch)
        ]
    ]

let navBrand =
    Navbar.Brand.div [] [
        Navbar.Item.a [
            Navbar.Item.Props [ Href "https://safe-stack.github.io/" ]
            Navbar.Item.IsActive true
        ] [
            img [
                Src "/fable.ico"
                Alt "Logo"
            ]
        ]
    ]

let view (state : State) (dispatch : Msg -> unit) =
    Hero.hero [
        Hero.IsFullHeight
    ] [
        Hero.head [] [
            Navbar.navbar [] [
                Container.container [] [
                    navBrand
                ]
            ]
        ]

        Hero.body [] [
            Container.container [] [
                Column.column [
                ] [
                    containerBox state dispatch
                ]
            ]
        ]
    ]
