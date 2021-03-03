module Index

open Elmish
open Feliz
open Browser

open Fulma.Extensions

type State =
    {
        InputTagsState: InputTags.State
        TagsSuggestions: string []
    }
type Msg =
    | SetInputTagsState of InputTags.State
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
            TagsSuggestions = tagsSuggestions
        }
    state, Cmd.none

let update (msg: Msg) (state: State) =
    match msg with
    | SetInputTagsState inputTagsState ->
        let state =
            { state with InputTagsState = inputTagsState }
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
        InputTags.inputTags
            "inputTagsId"
            (SetInputTagsState >> dispatch)
            (GetSuggestions >> dispatch)
            state.TagsSuggestions
            state.InputTagsState
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
