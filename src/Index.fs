module Index

open Elmish
open Feliz
open Browser

open Fulma.Extensions

type State =
    {
        InputTagsState: InputTags.State
        Tags: string list
        InputTag: string

        TagsSuggestions: string []
    }
type Msg =
    | SetState of State
    | GetSuggestions of string

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
            InputTagsState = InputTags.State.Empty

            Tags = []
            InputTag = ""

            TagsSuggestions = tagsSuggestions
        }
    state, Cmd.none

let update (msg: Msg) (state: State) =
    match msg with
    | SetState state ->
        state, Cmd.none
    | GetSuggestions pattern ->
        let state =
            { state with
                TagsSuggestions =
                    if pattern = "" then
                        tagsSuggestions
                    else
                        tagsSuggestions
                        |> Array.filter (fun x -> x.Contains pattern)
                }
        state, Cmd.none
open Fable.React
open Fable.React.Props
open Fulma
open Fable.FontAwesome

let containerBox (state : State) (dispatch : Msg -> unit) =
    Box.box' [] [
        state.Tags
        |> InputTags.inputTags
            "inputTagsId"
            state.InputTagsState
            (fun tag ->
                { state with
                    Tags = state.Tags |> List.filter ((<>) tag)
                }
                |> SetState
                |> dispatch
            )
            (fun st ->
                { state with InputTagsState = st }
                |> SetState
                |> dispatch

                st.CurrentTag
                |> GetSuggestions
                |> dispatch
            )
            (fun (st, tag) ->
                { state with
                    InputTagsState = st
                    Tags =
                        List.foldBack
                            (fun x st ->
                                if x = tag then st
                                else x::st
                            )
                            state.Tags
                            [tag]
                }
                |> SetState
                |> dispatch
            )
            state.TagsSuggestions
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
