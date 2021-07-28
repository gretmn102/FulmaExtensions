module Fulma.Extensions.InputWithSuggestions
open Fulma.Extensions.Types
type State =
    {
        Input: string
        Suggestions: Deferred<string []>
        SelectedSuggestion: int
        IsActive: bool
    }

let init initInput =
    {
        Input = initInput
        Suggestions = HasNotStartedYet
        SelectedSuggestion = -1
        IsActive = false
    }

type Submit =
    | Input
    | Suggestion of string

type Msg =
    | SetInput of string
    | Suggestions of string []
    | Submit of Submit
    | SelectSuggestion of int
    | IsActive of bool

open Elmish

let update getTagSuggestions (msg: Msg) (state: State) =
    match msg with
    | SetInput txt ->
        let cmd =
            Cmd.OfAsync.perform getTagSuggestions txt Suggestions
        let state =
            { state with
                Input = txt
                Suggestions = InProgress
            }
        state, cmd
    | Suggestions xs ->
        let state =
            { state with

                Suggestions = Resolved xs
            }
        state, Cmd.none
    | SelectSuggestion idx ->
        match state.Suggestions with
        | Resolved suggestions ->
            let state =
                { state with
                    Input = suggestions.[idx]
                    SelectedSuggestion = idx
                }
            state, Cmd.none
        | _ ->
            state, Cmd.none

    | Submit x ->
        let state =
            match x with
            | Input ->
                state
            | Suggestion sug ->
                { state with
                    Input = sug
                    IsActive = false
                }
        let state =
            { state with
                SelectedSuggestion = -1
            }

        state, Cmd.none

    | IsActive isActive ->
        let state =
            { state with
                IsActive = isActive
            }

        state, Cmd.none

open Fable.React
open Fable.React.Props
open Fulma
open Fable.FontAwesome

let view placeholder submit (state:State) dispatch =
    let isEmptySuggestions =
        match state.Suggestions with
        | Resolved suggestions -> Array.isEmpty suggestions
        | _ -> true

    Dropdown.dropdown [
        Dropdown.IsActive (not isEmptySuggestions && state.IsActive)
    ] [
        Dropdown.trigger [] [
            let disabled =
                System.String.IsNullOrWhiteSpace state.Input

            Control.p [
                Control.IsExpanded
                Control.IsLoading (state.Suggestions = InProgress)
            ] [
                Input.text [
                    Input.Value state.Input
                    Input.Placeholder placeholder
                    Input.OnChange (fun x ->
                        SetInput x.Value
                        |> dispatch
                    )
                    Input.Props [
                        OnFocus (fun e ->
                            IsActive true
                            |> dispatch
                        )
                        OnBlur (fun e ->
                            match e.relatedTarget :?> Browser.Types.HTMLElement with
                            | null ->
                                IsActive false
                                |> dispatch
                            | related ->
                                if related.classList.contains "dropdown-item" then
                                    ()
                                else
                                    IsActive false
                                    |> dispatch
                        )
                        OnKeyDown (fun e ->
                            match state.Suggestions with
                            | Resolved suggestions ->
                                match e.key with
                                | "ArrowDown" ->
                                    let i = state.SelectedSuggestion
                                    if i + 1 < suggestions.Length then
                                        i + 1
                                    else
                                        0
                                    |> SelectSuggestion
                                    |> dispatch
                                | "ArrowUp" ->
                                    e.preventDefault() // so that the cursor is always at the end
                                    let i = state.SelectedSuggestion
                                    if i - 1 < 0 then
                                        suggestions.Length - 1
                                    else
                                        i - 1
                                    |> SelectSuggestion
                                    |> dispatch
                                | "Enter" ->
                                    if not disabled then
                                        dispatch (Submit Input)
                                        submit state.Input
                                | _ -> ()
                            | HasNotStartedYet
                            | InProgress ->
                                if not disabled then
                                    if e.key = "Enter" then
                                        dispatch (Submit Input)
                                        submit state.Input
                        )
                    ]
                ]
            ]
        ]
        Dropdown.menu [
        ] [
            Dropdown.content [] [
                match state.Suggestions with
                | Resolved items ->
                    for i, (sug:string) in Array.indexed items do
                        Dropdown.Item.a [
                            Dropdown.Item.IsActive (state.SelectedSuggestion = i)

                            Dropdown.Item.Props [
                                OnClick (fun _ ->
                                    dispatch (Submit (Suggestion sug))
                                    submit sug
                                )
                                TabIndex -1 // for .relatedTarget
                            ]
                        ] [
                            str sug
                        ]
                | HasNotStartedYet
                | InProgress -> ()
            ]
        ]
    ]
